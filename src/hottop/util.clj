(ns hottop.util
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- split-and-trim
  "Splits the input string on the given regular expression and trims the
  result."
  [s re]
  (map str/trim (str/split s re)))

(defn- accept-segment-to-map
  "Takes a string representing an Accept- segment and returns a map containing
  at least key :type. If there are parameters present in the Accept segment,
  their names will be placed in the map as keyword with their corresponding
  values.  For example, the common 'q value' will have key :q in the map."
  [accept-seg-str]
  (let [[type & parameters] (split-and-trim accept-seg-str #";")
        [type subtype] (split-and-trim type #"/")]
    (apply merge {:type type :subtype subtype}
           (for [param parameters]
             (let [[k v] (split-and-trim param #"=")]
               {(keyword k) v})))))

(defn- type-map-to-content-str
  "Returns a media type string based on the given accept map."
  [accept-map]
  (str/join "/" [(:type accept-map) (:subtype accept-map)]))

(defn parse-accept-header
  "Parses an Accept- header string into a seq of maps which have at least the
  :type, :subtype and :q keys. Any other parameters in the Accept string will be
  given keys that are keywords of their names with their corresponding values."
  [accept-str]
  (let [accept-segs (split-and-trim accept-str #",")]
    (->> accept-segs
         (map accept-segment-to-map)
         (map (fn [{:keys [q] :or {q "1.0"} :as m}]
                (let [q (Double/parseDouble q)]
                  (assoc m :q q)))))))

(defn- provided-maps
  "Returns a set of maps with keys :type and :subtype based on the content types
  listed in the :content-types-provided map of the given resource."
  [resource]
  (->> resource
       :content-types-provided 
       keys
       (map #(str/split % #"/"))
       (map (fn [t] {:type (first t) :subtype (second t)}))
       set))

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
       (str/join ", ")))

(defn accepts-html?
  "Returns truthy if the request indicates that it will accept a response in
HTML format (the Accept header contains one or both of 'text/html' or
'application/xhtml+xml'), falsey otherwise."
  [request]
  (when-let [{{accept-str "accept"} :headers} request]
    (let [accept-types (map type-map-to-content-str (parse-accept-header accept-str))]
          (some #{"text/html" "application/xhtml+xml"} accept-types))))

(defn- at-accepts-pt?
  "Returns true if provided-type map (pt) is acceptable when compared to the
  given accepted-type map (at) irrespective of at's q value."
  [at pt]
  (or (= (:type at) "*")
      (and (= (:type at) (:type pt))
           (or (= (:subtype at) "*")
               (= (:subtype at) (:subtype pt))))))

(defn- group-by-provided-types
  "Returns a map with provide-type maps as keys and the set of accept-type maps
that 'accept' it as values."
  [accept-maps provided-maps]
  (reduce (fn [res [at pt]]
            (if (at-accepts-pt? at pt)
              (let [ats (or (res pt) #{})]
                (assoc res pt (conj ats at)))
              res))
          {}
          (for [at accept-maps
                pt provided-maps]
            [at pt])))

(defn optimal-media-type
  "Returns a string representing the optimal client-requested media type or nil
if there isn't one.  See RFC 2046 (http://tools.ietf.org/html/rfc2046) or
http://en.wikipedia.org/wiki/MIME_type for examples of media type strings."
  [request resource]
  (let [accept-maps (parse-accept-header (get-in request [:headers "accept"]))
        wanted? (fn [at] (> (:q at) 0.0))
        separate (juxt filter remove)
        ;; split the accept maps into those with q values greater than zero
        ;; (wanted-types) and those with q equal to zero (unwanted-types).
        [wanted-types unwanted-types] (separate wanted? accept-maps)
        ;; keep only the :type and :subtype keys of unwanted-types (so that we
        ;; can subtract these types from the provided types)
        unwanted-types (map #(select-keys % [:type :subtype]) unwanted-types)
        ;; create a set of provided maps with the unwanted types removed
        provided-maps (-> (provided-maps resource)
                          set
                          (set/difference unwanted-types))
        ;; create a map of provided-type to set of accept types that accept it
        pt-to-at (group-by-provided-types wanted-types provided-maps)
        ;; assoc into each provided type the maximum q value from the accept
        ;; types that accept it
        types (reduce (fn [types [pt ats]]
                        (let [mx (apply max (map :q ats))]
                          (conj types (assoc pt :q mx))))
                      []
                      pt-to-at)]
    (when (seq types)
      (->> types
           (sort-by :q >)
           first
           type-map-to-content-str))))

(defn response?
  "Returns truthy if argument is a map that contains the key :status, false
otherwise."
  [data]
  (and (map? data)
       (some #{:status} (keys data))))
