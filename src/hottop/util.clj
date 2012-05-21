(ns hottop.util
  (:require [clojure.string :as str]))

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
       (cons :options)
       (map name)
       (map str/upper-case)
       (interpose ", ")
       (apply str)))
