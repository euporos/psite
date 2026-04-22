(ns psite-mail.core
  "Minimal ClojureScript wrapper around nodemailer for Node.js.
   Returns native js/Promise instances."
  (:require
   ["nodemailer" :as nodemailer]
   [taoensso.timbre :refer-macros [info errorf]]))

;; ---------------------------------------------------------------------------
;; Transporter lifecycle
;; ---------------------------------------------------------------------------

(defn verify-transporter
  "Verifies SMTP connectivity for the given transporter.
   Returns a js/Promise that resolves to the transporter on success,
   or rejects with the verification error."
  [transporter]
  (let [user (-> ^js transporter .-options .-auth .-user)]
    (info "Verifying transporter for" user)
    (-> (.verify transporter)
        (.then (fn [_]
                 (info "Transporter for" user "verified")
                 transporter))
        (.catch (fn [e]
                  (errorf "Transporter for %s failed verification: %s"
                          user (.-message e))
                  (throw e))))))

(defn create-transporter
  "Creates a nodemailer transporter from a Clojure config map.
   When verify? is true (default), fires verification as a side-effect
   (logs result, does not block). Returns the transporter synchronously."
  ([config]
   (create-transporter true config))
  ([verify? config]
   (let [transporter (.createTransport nodemailer (clj->js config))]
     (when verify?
       (-> (verify-transporter transporter)
           (.catch (fn [e]
                     (errorf "Background verification failed: %s"
                             (.-message e))))))
     transporter)))

;; ---------------------------------------------------------------------------
;; Sending
;; ---------------------------------------------------------------------------

(defn send!
  "Sends an email via the given transporter.
   params is a Clojure map (:from, :to, :subject, :text, :html, etc.).
   Returns a js/Promise that resolves to {:success true ...info}
   or rejects with the error."
  [transporter params]
  (info "Sending mail")
  (-> (.sendMail ^js transporter (clj->js params))
      (.then (fn [info]
               (-> (js->clj info :keywordize-keys true)
                   (assoc :success true))))))
