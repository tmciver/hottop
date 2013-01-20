(ns hottop.core
  (:use hottop.proc)
  (:require [clout.core :as clout]))

(def http-processor (-> process-request
                        process-options
                        check-authorization
                        validate-method))

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
           (http-processor request# resource#)
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
        (http-processor request resource)
        {:status 404 :body "Resource Not Found"})))
  )
