(ns hottop.core
  (:use hottop.proc)
  (:require [clout.core :as clout]))

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

(defn handler
  "Main application handler. Takes a collection of resource maps as
argument. Returns a function to be passed as a handler to the run-jetty
function."
  [resources]
  (fn [request]
    (if-let [resource (first (filter #(clout/route-matches (:uri %) request) resources))]
      (run-processors resource request)
      {:status 404
       :body "Resource Not Found"})))

#_(defmacro app
  [& forms]
  {:pre (= (count forms) 2)}  ;; for now, we should only have a route and a resource map
  (let [[route resource] forms]
    `(fn [req]
       (let [segment (first (.split (:uri req) "/"))]
         (when (= route segment)
           (run-processors resource req))))))

(defmacro app
  [& forms]
  ;; for now, we should only have a route and a resource map
  (let [[[route] resource] forms]
    (list 'fn ['req]
          (list 'let ['segment (list 'second (list '.split (list :uri 'req) "/"))]
                (list 'when (list '= route 'segment)
                      (list 'hottop.core/run-processors resource 'req))))))
