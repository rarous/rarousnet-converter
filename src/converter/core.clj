(ns converter.core
  (:require clojure.pprint)
  (:import [java.text Normalizer Normalizer$Form]))

(use 'clj-time.coerce
     'clojure.string)

(require '[clojure.java.io :as io]
         '[clojure.zip :as zip]
         '[clojure.data.xml :as xml]
         '[clojure.data.zip :as zd]
         '[clojure.data.zip.xml :as zf])


(defn remove-diacritics
  "Remove diacritical marks from the string `s`, E.g., 'żółw' is transformed
  to 'zolw'."
  [ ^String s ]
  (.replaceAll (Normalizer/normalize s Normalizer$Form/NFD) "\\p{InCombiningDiacriticalMarks}+" ""))

(defmethod print-dup org.joda.time.DateTime
  [^org.joda.time.DateTime d out]
  (print-dup (.toDate d) out))

(defmethod print-method org.joda.time.DateTime
  [^org.joda.time.DateTime d out]
  (print-method (.toDate d) out))

(def articles
  (slurp (io/resource "articles.xml")))

(defn root [xml-string]
  (some-> xml-string
          xml/parse-str
          zip/xml-zip))

(defn urlize [s]
  (str (remove-diacritics (replace (lower-case s) #"[\s|\.]" "-")) ".aspx"))

(defn tags->vector [article]
  (let [tags (some-> (zf/xml1-> article :tags zf/text)
                     (clojure.string/replace "#x20;" "")
                     (clojure.string/split #"\s"))]
    (filterv #(not (clojure.string/blank? %))
             (or tags []))))

(defn article->detail-map [article]
  {:id (read-string (zf/xml1-> article :id zf/text))
   :title (zf/xml1-> article :title zf/text)
   :description (zf/xml1-> article :description zf/text)
   :html (zf/xml1-> article :html
                    (fn [loc]
                      (apply str (zf/xml-> loc zd/descendants zip/node string?))))
   ;:raw (zf/xml1-> article :raw zf/text)
   :author (zf/xml1-> article :author zf/text)
   :published (from-string (zf/xml1-> article :published zf/text))
   :url (zf/xml1-> article :url zf/text)
   :category (zf/xml1-> article :category zf/text)
   :keywords (zf/xml1-> article :keywords zf/text)
   :is-published (= 1 (read-string (zf/xml1-> article :is-published zf/text)))
   :edited (from-string (zf/xml1-> article :edited zf/text))
   :tags (tags->vector article)})

(defn index-by-url [article]
  (let [url (str (:id article) "-" (:url article) ".aspx")]
    [(keyword url) article]))

(def articles-result
  (->> (zf/xml-> (root articles) :article)
       (mapv article->detail-map)
       (filterv #(not= (:category %) "Reference"))
       (filterv #(not= (:category %) "Projekty"))
       (filterv :is-published)
       (mapv #(conj % {:category-url (urlize (:category %))}))
       (map index-by-url)
       (into {})))

(defn article->rubrics-map [article]
  {:id (read-string (zf/xml1-> article :id zf/text))
   :title (zf/xml1-> article :title zf/text)
   :published (from-string (zf/xml1-> article :published zf/text))
   :url (zf/xml1-> article :url zf/text)
   :category (zf/xml1-> article :category zf/text)
   :is-published (= 1 (read-string (zf/xml1-> article :is-published zf/text)))})

(def rubrics-result
  (->> (zf/xml-> (root articles) :article)
       (mapv article->rubrics-map)
       (filterv #(not= (:category %) "Reference"))
       (filterv #(not= (:category %) "Projekty"))
       (filterv :is-published)
       (mapv #(conj % {:category-url (keyword (urlize (:category %)))}))
       (group-by :category-url)))

(defn -main [& args]
  (clojure.pprint/pprint articles-result (io/writer "articles.edn"))
  (clojure.pprint/pprint rubrics-result (io/writer "rubrics.edn")))

