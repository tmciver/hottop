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
;; :auth
;;   A function whose only argument is the Ring request and which should
;;   return true if the user is authorized to access this resource, false
;;   otherwise.
;; :languages-provided
;;   A set of strings representing language codes. Defaults to #{"en" "en-us"}.
;; :redirect-after-html-post
;;   A function of the Ring request that returns a URI string to which the
;;   client will be redirect (via a 303 response). This occurs only if 1. the
;;   user supplies a redirect URI and 2. if the client lists either or both of
;;   'text/html' or 'application/xhtml+xml' in the Accept header.
;; :methods
;;   A map of HTTP method keywords to functions. The function associated with a
;;   method should take one argument which will be the Ring request. The return
;;   value can be any application-specific data structure. The returned data
;;   structure will be passed to the function associated with the optimal
;;   internet media type found in the :content-types-provided map.

(def base-resource {:content-types-provided {}
                    :content-types-accepted {}
                    :auth (constantly true)
                    :languages-provided #{"en" "en-us"}
                    :redirect-after-html-post (constantly nil)
                    :methods {}})

;; NOTE: rename to 'readonly-html' for next version bump
(defn create-readonly-html-resource
  "Returns a resource that will only respond to a GET request. get-fn should be
a function of the ring request. html-fn should be a function of one argument (it
is passed the result of get-fn) and should return well-formed HTML. You can also
pass in a single function that takes the ring request and returns HTML directly."
  ([get-fn html-fn]
     (-> base-resource
         (assoc-in [:methods :get] get-fn)
         (assoc-in [:content-types-provided "text/html"] html-fn)))
  ([get-html-fn]
     (create-readonly-html-resource get-html-fn identity)))

(defn add-method-handler
  "Adds handler to resource for the given HTTP method."
  [resource method handler]
  (assoc-in resource [:methods method] handler))

(defn add-view
  "Adds view-function to the :content-types-provided map for the key given by
content-type"
  [resource content-type view-function]
  (assoc-in resource [:content-types-provided content-type] view-function))
