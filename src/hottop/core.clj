(ns hottop.core
  (:use hottop.proc))

(def ^:private processors [process-options
                           validate-method
                           validate-authorization
                           process-acceptable-media-types
                           process-request])

(defn run-processors
  "Runs the processors on the given resource and request to produce a response."
  [resource request]
  (loop [processors processors
         resource resource
         request request
         response {}
         handlers {}]
    (if-let [processor (first processors)]
      (let [[resource request response handlers] (processor resource request response handlers)]
        (if (nil? (:status response))
          (recur (rest processors) resource request response handlers)
          response))
      {:status 500
       :body "Internal Server Error"})))
