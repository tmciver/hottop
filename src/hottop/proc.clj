(ns hottop.proc
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.string :as str]
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

(defmacro ^:private defmethodprocessor
  "A macro used to remove boiler plate from the HTTP method-processing
functions. Note that the following symbols are available for use in the body of
the macro: handler, request, resource."
  [proc-name doc & body]
  (let [[process method-str] (str/split (str proc-name) #"-")
        method (keyword method-str)]
    (when (not (and (= process "process")
                    (#{:get :head :put :post :delete} method)))
      (throw (IllegalArgumentException. "processor name must be of the form
\"process-<http-method>\".")))
    `(defn ~proc-name
       ~doc
       [~'handler]
       (fn ~['request 'resource]
         (let [method# (:request-method ~'request)]
           (if (= method# ~method)
             ~@body
             (~'handler ~'request ~'resource)))))))

(defmethodprocessor process-get
  "Creates a defn that returns a hottop handler function to appropriately deal
with a GET request. Will optimally return the formatted response but could also
return a 406 'Not Acceptable' if the resource is unable to supply a content type
listed among those in the 'Accept' header, or a 500 'Internal Server Error' if
there was no 'content-types-provided' function for the optimal content type.

Note: this should be changed so that it uses a 'content-types-provided' function
that could be placed into the resource map by a previous function and only
attempt to calculate the optimal content type to use if said function has not
been used."
  (if-let [ct-desired (util/optimal-media-type request resource)]
    (if-let [ct-fn (get-in resource [:content-types-provided ct-desired])]
      (let [get-fn (get-in resource [:methods :get])
            result (ct-fn (get-fn request))]
        (ring/response result))
      (response/code 500))
    (response/code 406)))

(defmethodprocessor process-post
  "Creates a defn that returns a hottop handler function to appropriately deal
with a POST request. Note that the normal response to a POST request is a '200'
but this function will respond with a redirect under the following circumstances:
1. the :redirect-after-html-post key in the resource is non-nil and 2. the media
type returned by util/optimal-media-type is one of 'text/html' or
'application/xhtml+xml'. This is to give the right behavior when the client is a
web browser."
  (let [post-fn (get-in resource [:methods :post])
        result (post-fn request)]
    (if (util/response? result)
      result
      (let [redirect-uri (:redirect-after-html-post resource)]
        (if (and redirect-uri (util/accepts-html? request))
          (ring/redirect-after-post redirect-uri)
          (response/code 200))))))
