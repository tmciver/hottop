(ns hottop.proc
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [hottop.util :as util]))

;; The functions contained in this namespace are processors and are intended to
;; process the request in step.  Each processor function takes four arguments:
;;
;; resource
;;   The resource map
;; request
;;   The Ring request map
;; response
;;   The Ring response map
;; handlers
;;   A map that is populate 'along the way' as the request is processed with the
;;   functions needed to process the request.
;;
;; Likewise, each processor function returns a vector containing the above four
;; data structures (with augmentation, if necessary).
;;
;; Where possible the processor functions have metadata attached indicating the
;; Webmachinge node it is most like (see
;; http://wiki.basho.com/images/http-headers-status-v3.png)

(defn ^{:webmachine-node :b10} validate-method
  "Checks whether the request method is implemented by the given resource. If it
is, assoc's key :method-handler in handlers map whose value is the function
associated with that method. If the method is not supported by the resource, a
response with status code 405 is returned."
  [resource request response handlers]
  (let [request-method (:request-method request)
        available-methods (set (keys (:methods resource)))]
    (if (available-methods request-method)
      (let [handlers (assoc handlers :method-handler (get-in resource [:methods request-method]))]
        [resource request response handlers])
      (let [response {:status 405
                      :headers {"allow" (util/allow-header-str resource)}
                      :body "Method Not Allowed"}]
        [resource request response handlers]))))

(defn ^{:webmachine-node :b8} validate-authorization
  "Calls the function associated with resource key :auth with request as
argument and considers the user authorized if that function returns true. This
  method returns a response map with a status code 401 if the user is not
  authorized."
  [resource request response handlers]
  (let [auth-fn (:auth resource)]
    (if (auth-fn request)
      [resource request response handlers]
      [resource request {:status 401
                         :body "Unauthorized"} handlers])))

(defn ^{:webmachine-node :b3} process-options
  "If the request method is OPTIONS, creates a response whose :status is 200 and
  whose \"Allow\" header is a string of comma-separated, upper-case HTTP
  methods supported by this resource."
  [resource request response handlers]
  (if (= (:request-method request) :options)
    (let [methods-str (util/allow-header-str resource)
          response {:status 200
                    :headers {"Allow" methods-str}}]
      [resource request response handlers])
    [resource request response handlers]))

(defn ^{:webmachine-node :c4} process-acceptable-media-types
  "Determine the optimal media type to send to the client given the Accept
  request header and the media types available. If no acceptable media type is
  available, returns a response map with a 406 status (Not Acceptable). If there
  is an acceptable media type availabe, key :transform-handler is added the
  handlers map whose value is the function associated with that media type from
  the resource.

  WARNING! This function is broken. Specifically, */* and type/* media types in
  the Accept header are not handled."
  [resource request response handlers]
  (let [ct-provided (set (keys (:content-types-provided resource)))
        accept-maps (util/parse-accept-header (get-in request [:headers "accept"]))
        accept-maps (filter #(not (= 0 (:q %))) accept-maps)
        ct-accepted (set (map :type accept-maps))
        intersection (set/intersection ct-provided ct-accepted)
        type (->> (filter #(intersection (:type %)) accept-maps)
                  (sort-by :q >)
                  first
                  :type)]
    (if type
      (let [handlers (assoc handlers :transform-handler (get-in resource [:content-types-provided type]))]
        [resource request response handlers])
      [resource request {:status 406
                         :body "Not Acceptable"} handlers])))

(defn process-request
  "Calls the function associated with the request method with the Ring request
  map as argument. Passes the result of that to the function associated with the
  content type from the :content-types-provided map."
  [resource request response handlers]
  (let [method-fn (:method-handler handlers)
        transform-fn (:transform-handler handlers)
        method-result (method-fn request)
        formatted (transform-fn method-result)
        response {:status 200
                  :body formatted}]
    [resource request response handlers]))
