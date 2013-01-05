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

(defn- compile-route
  "Takes a string representing a URI and two-element seq whose first element
is vector containing a string representing a route and whose second element is a
map representing a resource. Returns syntax for a compiled route."
  [uri [[route] resource]]
  `(when (= ~uri ~route)
     ~resource))

(defn app*
  [& forms]
  (when (not (= (mod (count forms) 2) 0))
    (throw (IllegalArgumentException. "Must provide an even number of arguments.")))
  (let [routes+resources (partition 2 forms)
        uri (gensym "uri")]
    `(fn [request#]
       (let [~uri (:uri request#)
             resource# (or ~@(map (partial compile-route uri) routes+resources))]
         (if resource#
           (run-processors resource# request#)
           {:status 404 :body "Resource Not Found"})))))

(defmacro app
  "Takes an unlimited number of pairs containing a vector and a map. Currently,
the vector should contain a single string representing the URI to route to. The
map should be a valid hottop resource map."
  [& forms]
  (apply app* forms))

(comment
  ;; for this version this . . .
  (app ["/hello"] hello-resource-map
       ["/goodbye"] goodbye-resource-map)

  ;; should become something like . . .
  (fn [request]
    (let [uri (:uri request)
          resource (or (when (= uri "/hello")
                         hello-resource-map)
                       (when (= uri "/goodbye")
                         goodbye-resource-map))]
      (if resource
        (run-processors resource request)
        {:status 404 :body "Resource Not Found"})))
  )
