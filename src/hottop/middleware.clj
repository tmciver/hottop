(ns hottop.middleware
  (:use [ring.util.mime-type :only (ext-mime-type)])
  (:require [clojure.string :as str]))

;; a private function stolen from ring.util.mime-type
(defn- filename-ext
  "Returns the file extension of a filename or filepath."
  [filename]
  (if-let [ext (second (re-find #"\.([^./\\]+)$" filename))]
    (str/lower-case ext)))

(defn wrap-accept-from-extension
  "Sets the 'Accept' header of the request map to the appropriate media type if
the request URI has a known extension. If a valid extension is found, the given
handler will be called with an updated request map.  If and extension is found
but a corresponding media type is not found, a 406 response is returned. If
there is no extension found, the given handler is called with the unaltered
request map."
  [handler]
  (fn [req]
    (if-let [ext (filename-ext (:uri req))]
      (if-let [ct-from-ext (ext-mime-type (:uri req))]
        (handler (assoc-in req [:headers "accept"] ct-from-ext))
        {:status 406 :body "Unsupported file extension"})
      (handler req))))
