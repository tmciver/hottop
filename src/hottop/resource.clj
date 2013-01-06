(ns hottop.resource)

;; A resource is a regular Clojure map and can contain the following keys and
;; values:
;;
;; :content-types-provided
;;   A map whose keys are strings of internet media types (see
;;   http://en.wikipedia.org/wiki/Internet_media_type for valid media types) to
;;   functions of a single argument. These functions are intended to convert the
;;   data returned by the :method functions (see below) to the format given by
;;   the media type.
;; :content-types-accepted
;;   A map whose keys are strings of internet media types (see
;;   http://en.wikipedia.org/wiki/Internet_media_type for valid media types) to
;;   functions of a single argument. These functions are intended to convert
;;   data from the client into a more usable format (This needs to be defined
;;   better).
;; :languages-provided
;;   A set of strings representing language codes. Defaults to #{"en" "en-us"}.
;; :methods
;;   A map of HTTP method keywords to functions. The function associated with a
;;   method should take one argument which will be the Ring request. The return
;;   value can be any application-specific data structure. The returned data
;;   structure will be passed to the function associated with the optimal
;;   internet media type found in the :content-types-provided map.
;; :auth
;;   A function whose only argument is the Ring request and which should
;;   return true if the user is authorized to access this resource, false
;;   otherwise.
(def base-resource {:content-types-provided {}
                    :content-types-accepted {"application/x-www-form-urlencoded" 'some-fn-to-parse-like-from-Ring}
                    :auth (constantly true)
                    :languages-provided #{"en" "en-us"}
                    :methods {}})

(defn create-readonly-html-resource
  "Returns a resource that will only respond to a GET request. get-fn should be
a function of the ring request. html-fn should be a function of one argument (it
is passed the result of get-fn) and should return well-formed HTML. If you want
to use a single function that takes the ring request and returns HTML directly,
then pass that function as get-fn and use identity for html-fn."
  [get-fn html-fn]
  (-> base-resource
      (assoc-in [:methods :get] get-fn)
      (assoc-in [:content-types-provided "text/html"] html-fn)))
