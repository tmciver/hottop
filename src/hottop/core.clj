(ns hottop.core
  (:use hottop.proc)
  (:require [clout.core :as clout]
            [hottop.response :as response]))

(defmacro <-
  "Applies -> to the given forms in reverse. This facilitates building a
sequence of Ring-like handler functions as the functions will be executed in the
order given."
  [& forms]
  `(-> ~@(reverse forms)))

(def http-processor (<- validate-method
                        check-authorization
                        process-options
                        process-get
                        process-post
                        (constantly (response/code 500))))

(defn- compile-resource-handler
  "Takes a string representing a URI and two-element seq whose first element
is vector containing a string representing a route and whose second element is a
map representing a resource. Returns syntax for a compiled route."
  [request [[route] resource]]
  `(when-let [params# (clout/route-matches ~route ~request)]
     (let [request# (merge-with merge ~request {:route-params params#, :params params#})]
       (http-processor request# ~resource))))

(defn routes*
  [& forms]
  (when (not (= (mod (count forms) 2) 0))
    (throw (IllegalArgumentException. "Must provide an even number of arguments.")))
  (let [routes+resources (partition 2 forms)
        request (gensym "request")]
    `(fn [~request]
       (if-let [response# (or ~@(map (partial compile-resource-handler request) routes+resources))]
         response#
         (response/code 404)))))

(defmacro routes
  "Takes an unlimited number of pairs containing a vector and a map. Currently,
the vector should contain a single string representing the URI to route to. The
map should be a valid hottop resource map."
  [& forms]
  (apply routes* forms))

(comment
  ;; for this version this . . .
  (routes ["/hello"] hello-resource-map
          ["/goodbye"] goodbye-resource-map)

  ;; should become something like . . .
  (fn [request]
    (if-let [response (or (when-let [params (clout/route-matches "/hello" request)]
                            (let [request (merge-with merge request {:route-params params, :params params})]
                              (http-processor request hello-resource-map)))
                          (when-let [params (clout/route-matches "/goodbye" request)]
                            (let [request (merge-with merge request {:route-params params, :params params})]
                              (http-processor request goodbye-resource-map))))]
      response
      {:status 404 :body "Resource Not Found"}))
  )
