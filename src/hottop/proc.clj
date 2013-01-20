(ns hottop.proc
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [hottop.util :as util]))

;; The public functions in this namespace are modelled after Ring middleware in
;; that they are functions that take a handler as argument and return a
;; function. They are different in that the handler is a function of two args -
;; the Ring request and a hottop resource map. This is, of course, also true of
;; the function that is returned.
;;
;; These functions typically (always?) return an HTTP response if their
;; particular condition is not met.  Otherwise, they call the passed-in handler
;; with the request and resource arguments.
;;
;; Where possible the processor functions have metadata attached indicating the
;; Webmachinge node it is most like (see
;; http://wiki.basho.com/images/http-headers-status-v3.png)

(defn ^{:webmachine-node :b10} validate-method
  "If the resource does not implement the HTTP method (verb), then a 405
response is returned.  Otherwise the given handler is called with request and
resource arguments."
  [handler]
  (fn [request resource]
    (let [request-method (:request-method request)
          available-methods (set (keys (:methods resource)))]
      (if (available-methods request-method)
        (handler request resource)
        {:status 405 :body "Method Not Allowed"}))))

(defn ^{:webmachine-node :b8} check-authorization
  "If the result of calling the function associated with the :auth key in the
resource map with arguments 'request' and 'resource' returns false, returns a
401 'Unauthorized' response. Otherwise the given handler is called with request
and resource arguments."
  [handler]
  (fn [request resource]
    (let [auth-fn (:auth resource)]
      (if (auth-fn request)
        (handler request resource)
        {:status 401 :body "Unauthorized"}))))

(defn ^{:webmachine-node :b3} process-options
  "If the request method is OPTIONS, creates a response whose :status is 200 and
  whose \"Allow\" header is a string of comma-separated, upper-case HTTP
  methods supported by this resource.

NOTE: it looks like the servlet used by ring/jetty intercepts an OPTIONS
request. Look into fixing this."
  [handler]
  (fn [request resource]
    (if (= (:request-method request) :options)
      {:status 200 :headers {"Allow" (util/allow-header-str resource)}}
      (handler request resource))))

(defn process-request
  [request resource]
  (let [method (:request-method request)
        f (get-in resource [:methods method])
        ct-desired (util/optimal-media-type request resource)]
    (if (and (= method :get) (not ct-desired))
      ;; the client requested a resource without an acceptable content type
      {:status 406 :body "Not Acceptable"}
      (let [result (f request)
            result (if (= method :get)
                     ;; transform the data from get using the optimal content-
                     ;; types-provided function
                     (if-let [ct-fn (get-in resource
                                            [:content-types-provided ct-desired])]
                       (ct-fn result)
                       {:status 500 :body "Internal Server Error"})
                     result)]
        (if (nil? result)
          {:status 500 :body "Internal Server Error"}
          {:status 200 :body result})))))
