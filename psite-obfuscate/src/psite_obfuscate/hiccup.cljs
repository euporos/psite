(ns psite-obfuscate.hiccup
  "Server-side hiccup markers for obfuscated contact info.

  Emits a bare `<span>` with a CSS class and an `env-path` attribute holding
  an opaque, URI-encoded EDN payload. `psite-obfuscate.browser/inject` reads
  the payload back, asks the caller to resolve it to a plaintext value, and
  swaps in a `react-obfuscate` component.")

(def email-class "psite-oml")
(def phone-class "psite-oph")

(defn marker
  "Hiccup for an obfuscation marker. `payload` is any Clojure value (commonly
  a config path); it is round-tripped via `pr-str` + URI encoding."
  [class payload]
  [:span {:class    class
          :env-path (js/encodeURIComponent (pr-str payload))}])

(defn email [payload] (marker email-class payload))
(defn phone [payload] (marker phone-class payload))
