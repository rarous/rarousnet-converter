(ns converter.core
  (:require [clj-time.core :as t]
            [clojure.core.reducers :as r]
            [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.xml :as xml]
            [clojure.data.zip :as zd]
            [clojure.data.zip.xml :as zf]
            [clojure.string :as str])
  (:import [java.text Normalizer Normalizer$Form]))

(use 'clj-time.coerce)

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

(defn- get-root [xml-string]
  (some-> xml-string
          xml/parse-str
          zip/xml-zip))

(def articles (slurp (io/resource "articles.xml")))
(def root (get-root articles))

(defn- urlize [s]
  (str (remove-diacritics (str/replace (str/lower-case s) #"[\s|\.]" "-")) ".aspx"))

(defn- tags->vector [article]
  (let [tags (some-> (zf/xml1-> article :tags zf/text)
                     (clojure.string/replace "#x20;" "")
                     (clojure.string/split #"\s"))]
    (filterv (comp not empty?) (or tags []))))

(defn- article->detail-map [article]
  {:id (read-string (zf/xml1-> article :id zf/text))
   :title (zf/xml1-> article :title zf/text)
   :description (zf/xml1-> article :description zf/text)
   :html (zf/xml1-> article :html #(apply str (zf/xml-> % zd/descendants zip/node string?)))
   ;:raw (zf/xml1-> article :raw zf/text)
   :author (zf/xml1-> article :author zf/text)
   :published (from-string (zf/xml1-> article :published zf/text))
   :url (zf/xml1-> article :url zf/text)
   :category (zf/xml1-> article :category zf/text)
   :keywords (zf/xml1-> article :keywords zf/text)
   :is-published (= 1 (read-string (zf/xml1-> article :is-published zf/text)))
   :edited (from-string (zf/xml1-> article :edited zf/text))
   :tags (tags->vector article)})

(defn- index-by-url [{:keys [id url] :as article}]
    [(keyword (str id "-" url ".aspx")) article])

(defn- articles-result [root]
  (->> (zf/xml-> root :article)
       (r/map article->detail-map)
       (r/filter #(not= (:category %) "Reference"))
       (r/filter #(not= (:category %) "Projekty"))
       (r/filter :is-published)
       (r/map #(assoc % :category-url (urlize (:category %))))
       (r/map index-by-url)
       (into {})))

(defn- article->rubrics-map [article]
  {:id (read-string (zf/xml1-> article :id zf/text))
   :title (zf/xml1-> article :title zf/text)
   :published (from-string (zf/xml1-> article :published zf/text))
   :url (zf/xml1-> article :url zf/text)
   :category (zf/xml1-> article :category zf/text)
   :is-published (= 1 (read-string (zf/xml1-> article :is-published zf/text)))})

(defn- rubrics-result [root]
  (->> (zf/xml-> root :article)
       (r/map article->rubrics-map)
       (r/filter #(not= (:category %) "Reference"))
       (r/filter #(not= (:category %) "Projekty"))
       (r/filter :is-published)
       (into [])
       (sort-by :id >)
       (group-by (comp keyword urlize :category))))

(defn -main [& args]
  (println "Processing articles")
  (time (clojure.pprint/pprint (articles-result root) (io/writer "articles.edn")))
  (println "Processing categories")
  (time (clojure.pprint/pprint (rubrics-result root) (io/writer "rubrics.edn"))))

