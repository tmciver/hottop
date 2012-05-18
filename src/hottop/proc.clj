(ns hottop.proc
  (:require [clojure.string :as str]))

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
  "Checks whether the request method is implemented by the given
resource. Returns nil if it is, or a response map with a status code 405."
  [resource request response handlers]
  (let [request-method (:request-method request)
        available-methods (set (keys (:methods resource)))]
    (if (available-methods request-method)
      (let [handlers (assoc handlers :method-handler (get-in resource [:methods request-method]))]
        [resource request response handlers])
      [resource request {:status 405
                         :body "Method Not Allowed"} handlers])))

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
    (let [methods-str (->> resource
                           :methods
                           keys
                           (cons :options)
                           (map name)
                           (map str/upper-case)
                           (interpose ", ")
                           (apply str))
          response {:status 200
                    :headers {"Allow" methods-str}}]
      [resource request response handlers])
    [resource request response handlers]))

(defn ^{:webmachine-node :c4} process-acceptable-media-types
  "Determine the optimal media type to send to the client given the Accept
  request header and the media types available. If no acceptable media type is
  available, returns a response map with a 406 status (Not Acceptable)."
  [resource request response handlers]
  )
