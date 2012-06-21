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

(defmacro app
  "Currently only supports one or more route-resource pairs."
  [& forms]
  (let [routes+resources (partition 2 forms)
        routes+resources (map vec routes+resources)
        routes+resources (vec routes+resources)
        matcher '(fn [segment [[route] resource]]
                  (when (= route segment)
                    resource))]
    `(fn [req#]
       (let [segment# (second (.split (:uri req#) "/"))
             [~'_ resource#] (first (filter (fn [[[route#] ~'_]]
                                             (= route# segment#)) ~routes+resources))]
         (if resource#
           (run-processors resource# req#)
           {:status 404
            :body "Resource Not Found"})))))

(comment
  ;; for this version this . . .
  (app ["hello"] hello-resource-map
       ["goodbye"] goodbye-resource-map)

  ;; should become something like . . .
  (fn [request]
    (let [segment (second (.split (:uri request) "/"))
          [_ resource] (first (filter (fn [[[route] _]]
                                         (= route segment))
                                   [[["hello"] hello-resource-map]
                                    [["goodbye"] goodbye-resource-map]]))]
      (if resource
        (run-processors resource request)
        {:status 404
         :body "Resource Not Found"})))
  )
