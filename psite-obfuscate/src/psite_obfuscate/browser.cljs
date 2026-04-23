(ns psite-obfuscate.browser
  "Browser-side injection of `react-obfuscate` components into marker spans
  emitted by `psite-obfuscate.hiccup`.

  A `get-value` fn receives the payload EDN the server wrote into the marker
  and must return the plaintext string to display. Application-specific
  concerns (config lookup, base64 decoding, etc.) live in `get-value`, not
  in this lib.

  For the common shape — look up a config path, then `js/atob` the result —
  call `(base64-getter get-setting)`. Install it once with
  `set-default-get-value!` and subsequent `inject`/`email`/`phone` calls can
  be used no-arg.

  Requires `reagent` and the `react-obfuscate` npm package on the consumer's
  classpath / package.json."
  (:require
   ["react-obfuscate" :as Obfuscate]
   [clojure.edn :as edn]
   [psite-obfuscate.hiccup :as hic]
   [reagent.core :as r]
   [reagent.dom :as rdom]))

(defonce ^:private default-get-value (atom nil))

(defn set-default-get-value!
  "Install a default `get-value` fn used when `inject`/`email`/`phone` are
  called without one."
  [f]
  (reset! default-get-value f))

(defn base64-getter
  "Compose a `get-value` fn: `payload -> get-setting -> js/atob`. Typical
  shape for callers whose frontend config holds base64-obfuscated strings."
  [get-setting]
  (fn [payload] (js/atob (get-setting payload))))

(defn- resolve-get-value [f]
  (or f @default-get-value
      (throw (ex-info "psite-obfuscate: no get-value supplied and no default installed"
                      {:hint "call set-default-get-value! at startup, or pass a fn"}))))

(defn- component [prop-key value]
  [(r/adapt-react-class (.-default Obfuscate))
   {prop-key value}])

(defn- payload [node]
  (edn/read-string
   (js/decodeURIComponent (.getAttribute node "env-path"))))

(defn inject
  "Expand every element matching `.{class}` into a `react-obfuscate`
  component, passing `(get-value (payload node))` under `prop-key`.
  With no `get-value`, falls back to the default installed via
  `set-default-get-value!`."
  ([class prop-key]           (inject class prop-key nil))
  ([class prop-key get-value]
   (let [gv (resolve-get-value get-value)]
     (doseq [node (js/document.querySelectorAll (str "." class))]
       (rdom/render (component prop-key (gv (payload node))) node)))))

(defn email
  ([]          (inject hic/email-class :email))
  ([get-value] (inject hic/email-class :email get-value)))

(defn phone
  ([]          (inject hic/phone-class :tel))
  ([get-value] (inject hic/phone-class :tel   get-value)))
