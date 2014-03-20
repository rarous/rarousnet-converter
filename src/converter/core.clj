(ns converter.core
  (:require clojure.pprint))

(use 'clj-time.coerce)

(require '[clojure.java.io :as io]
         '[clojure.zip :as zip]
         '[clojure.data.xml :as xml]
         '[clojure.data.zip :as zd]
         '[clojure.data.zip.xml :as zf])

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

(defn tags->vector [article]
  (let [tags (some-> (zf/xml1-> article :tags zf/text)
                     (clojure.string/replace "#x20;" "")
                     (clojure.string/split #"\s"))]
    (filterv #(not (clojure.string/blank? %))
             (or tags []))))

(defn article->map [article]
  {:id (read-string (zf/xml1-> article :id zf/text))
   :title (zf/xml1-> article :title zf/text)
   :description (zf/xml1-> article :description zf/text)
   :html (clojure.string/trim (zf/xml1-> article :html
                                         (fn [loc]
                                           (apply str (zf/xml-> loc zd/descendants zip/node string?)))))
   ;:raw (zf/xml1-> article :raw zf/text)
   :author (zf/xml1-> article :author zf/text)
   :published (from-string (zf/xml1-> article :published zf/text))
   :url (zf/xml1-> article :url zf/text)
   :category (zf/xml1-> article :category zf/text)
   :keywords (zf/xml1-> article :keywords zf/text)
   :is-published (boolean (read-string (zf/xml1-> article :is-published zf/text)))
   :edited (from-string (zf/xml1-> article :edited zf/text))
   :tags (tags->vector article)})

(defn map->hash [article]
  (let [url (str (:id article) "-" (:url article) ".aspx")]
       [(keyword url) article]))

(def result
  (->> (zf/xml-> (root articles) :article)
       (mapv article->map)
       (filterv #(not= (:category %) "Reference"))
       (filterv #(not= (:category %) "Projekty"))
       (filterv :is-published)
       (map map->hash)
       (into {})
       ))

(defn -main [& args]
  (clojure.pprint/pprint result (io/writer "articles.edn")))
