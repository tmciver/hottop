(ns hottop.proc
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [hottop.util :as util]
            [hottop.response :as response]
            [ring.util.response :as ring]))

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
          available-methods (-> (:methods resource)
                                keys
                                (conj :options)
                                set)]
      (if (available-methods request-method)
        (handler request resource)
        (response/code 405)))))

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
        (response/code 401)))))

(defn ^{:webmachine-node :c4} validate-accept
  "Returns a hottop handler function that checks if the resource provides a
content type acceptable to the client. This handler function returns a 406
response if the resource does not provide one, otherwise the passed-in handler
is called with the request and resource as arguments."
  [handler]
  (fn [request resource]
    (if (get-in request [:headers "accept"])
      (if-let [optimal-ct (util/optimal-media-type request resource)]
        (handler (assoc request :optimal-ct optimal-ct) resource)
        (response/code 406))
      (handler request resource))))

(defmulti process-request*
  "Processes the HTTP request as a function of the request method."
  (fn [request resource] (:request-method request)))

(defmethod process-request* :get
  [request resource]
  (if-let [ct-desired (or (:optimal-ct request) ;; put in place by 'validate-accept'
                          (util/optimal-media-type request resource))]
    (if-let [ct-fn (get-in resource [:content-types-provided ct-desired])]
      (let [get-fn (get-in resource [:methods :get])
            result (ct-fn (get-fn request))]
        (if (util/response? result)
          result
          (-> (ring/response result)
              (ring/header "content-type" ct-desired))))
      (response/code 500))
    (response/code 406)))

(defmethod process-request* :post
  [request resource]
  (if-let [post-fn (get-in resource [:methods :post])]
    (let [result (post-fn request)]
      (if (util/response? result)
        result
        (let [redirect-uri ((:redirect-after-html-post resource) request)]
          (if (and redirect-uri (util/accepts-html? request))
            (ring/redirect-after-post redirect-uri)
            (response/code 200)))))
    (response/code 500)))

;; If the request method is OPTIONS, creates a response whose :status is 200 and
;; whose \"Allow\" header is a string of comma-separated, upper-case HTTP
;; methods supported by this resource.
(defmethod process-request* :options
  [request resource]
  {:status 200 :headers {"Allow" (util/allow-header-str resource)}})

(defmethod process-request* :default
  [request resource]
  (if-let [method-fn (get-in resource [:methods (:request-method request)])]
    (let [response (method-fn request)]
      (if (util/response? response)
        response
        (response/code 200)))
    (response/code 500)))

(defn process-request
  "Processes the request."
  [request resource]
  (process-request* request resource))
