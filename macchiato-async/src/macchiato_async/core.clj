(ns macchiato-async.core)

(defmacro defhandler
  "Defines a Macchiato ring handler from a simple [req] -> response function.
   The body is async-agnostic: it may return a promise, a channel, or a plain map.

   (defhandler my-handler [req]
     (p/let [data (db/query pool {...})]
       (html->response (render data))))"
  [var args & body]
  `(def ~var
     (macchiato-async.core/wrap-async
      ~(str var)
      (fn ~args ~@body))))
