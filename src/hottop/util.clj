(ns hottop.util
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- split-and-trim
  "Splits the input string on the given regular expression and trims the
  result."
  [s re]
  (map str/trim (str/split s re)))

(defn- accept-segment-to-map
  "Takes an string representing an Accept- segment and returns a map containing
  at least key :type. If there are parameters present in the Accept segment,
  their names will be placed in the map as keyword with their corresponding
  values.  For example, the common 'q value' will have key :q in the map."
  [accept-seg-str]
  (let [[type & parameters] (split-and-trim accept-seg-str #";")]
    (apply merge {:type type}
           (for [param parameters]
             (let [[k v] (split-and-trim param #"=")]
               {(keyword k) v})))))

(defn parse-accept-header
  "Parses an Accept- header string into a seq of maps which have at least the
  :type and :q keys. Any other parameters in the Accept string will be given
  keys that are keywords of their names with their corresponding values."
  [accept-str]
  (let [accept-segs (split-and-trim accept-str #",")]
    (->> accept-segs
         (map accept-segment-to-map)
         (map (fn [{:keys [q] :or {q "1.0"} :as m}]
                (let [q (Double/parseDouble q)]
                  (assoc m :q q)))))))

(defn allow-header-str
  "Returns a string of a comma-separated list (with spaces) of the methods
  (upper-cased) supported by the given resource. This string is intended to be
  used with the \"Allow\" header in an HTTP response."
  [resource]
  (->> resource
       :methods
       keys
       (map name)
       (map str/upper-case)
       (interpose ", ")
       (apply str)))

(defn accepts-html?
  "Returns truthy if the request indicates that it will accept a response in
HTML format (the Accept header contains one or both of 'text/html' or
'application/xhtml+xml'), falsey otherwise."
  [request]
  (when-let [{{accept-str "accept"} :headers} request]
    (let [accept-types (map :type (parse-accept-header accept-str))]
          (some #{"text/html" "application/xhtml+xml"} accept-types))))

(defn ^{:webmachine-node :c4} optimal-media-type
  "Returns a string representing the optimal client-requested media type or nil
if there isn't one.  See RFC 2046 (http://tools.ietf.org/html/rfc2046) or
http://en.wikipedia.org/wiki/MIME_type for examples of media type strings.

  WARNING! This function is broken. Specifically, */* and type/* media types in
  the Accept header are not handled."
  [request resource]
  (let [ct-provided (set (keys (:content-types-provided resource)))
        accept-maps (parse-accept-header (get-in request [:headers "accept"]))
        accept-maps (filter #(not (= 0 (:q %))) accept-maps)
        ct-accepted (set (map :type accept-maps))
        intersection (set/intersection ct-provided ct-accepted)
        type (->> (filter #(intersection (:type %)) accept-maps)
                  (sort-by :q >)
                  first
                  :type)]
    type))

(defn response?
  "Returns truthy if argument is a map that contains the key :status, false
otherwise."
  [data]
  (and (map? data)
       (some #{:status} (keys data))))
