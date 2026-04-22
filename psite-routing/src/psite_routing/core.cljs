(ns psite-routing.core
  "Routing helpers over Reitit + ring-style requests.

   Contract: each function takes a request whose :config map contains the
   keys declared by `config-schema`. The application wires values in via
   psite-config.runtime. In festival-macchiato, this is attached by the
   wrap-config middleware in serving.core."
  (:require [reitit.core :as reit]))

(def config-schema
  [:map
   [:psite-routing/devmode?        {:optional true} :boolean]
   [:psite-routing/locale-fallback [:sequential :keyword]]])

(defn make-path-absolute [req relative-path]
  (let [protocol (if (get-in req [:config :psite-routing/devmode?]) "http" "https")
        host     (get-in req [:headers "host"])]
    (str protocol "://" host relative-path)))

(defn path-param [req param-key]
  (get-in req [:parameters :path param-key]))

(defn coerce-locale [req]
  (let [fallback       (get-in req [:config :psite-routing/locale-fallback])
        default-locale (first fallback)
        locale-set     (set fallback)
        found
        (some (fn [[_ f]] (when-let [r (f req)] r))
              [["injected"      #(get-in % [:locale])]
               ["reitit path"   #(get-in % [:parameters :path :locale])]
               ["analysed path" #(keyword (get-in % [:path-params :locale]))]
               ["raw path"      #(keyword (second (re-find #"^/([a-z]+)/"
                                                           (or (get % :uri) ""))))]
               ["cookie"        #(keyword (get-in % [:cookies "locale" :value]))]
               ["header"        #(keyword (re-find #"^[a-z]+"
                                                   (or (get-in % [:headers "accept-language"]) "")))]])]
    (locale-set found default-locale)))

(defn wrap-locale
  "Coerces locale from request and attaches it as :locale.
   Sets a locale cookie on every response."
  [handler]
  (fn [req res raise]
    (let [coerced-locale (coerce-locale req)]
      (handler (assoc req :locale coerced-locale)
               #(res (assoc-in % [:cookies "locale"] {:value   (name coerced-locale)
                                                       :signed? true}))
               raise))))

(defn reverse-match
  "Reverse-route via reitit. Returns '/' if no router on request.
   opts: {:absolute? bool :fragment str}"
  ([request route-name params]
   (reverse-match request route-name params {} {}))
  ([request route-name params query-params]
   (reverse-match request route-name params query-params {}))
  ([{::reit/keys [router] :as request} route-name params query-params
    {:keys [absolute? fragment] :or {absolute? false}}]
   (if-not router
     "/"
     (let [params (merge {:locale (or (get request :locale)
                                      (get-in request [:parameters :path :locale]))}
                         params)]
       ((if absolute? (partial make-path-absolute request) identity)
        (str (reit/match->path (reit/match-by-name router route-name params) query-params)
             fragment))))))
