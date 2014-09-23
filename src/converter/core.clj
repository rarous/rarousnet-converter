(ns converter.core
  (:require [clj-time.core :as t]
            [clj-time.coerce :refer [from-string to-date]]
            [clojure.core.reducers :as r]
            [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.xml :as xml]
            [clojure.data.zip :as zd]
            [clojure.data.zip.xml :as zf]
            [clojure.string :as str]
            [cognitect.transit :as transit])
  (:import [java.text Normalizer Normalizer$Form]))

(defn remove-diacritics
  "Remove diacritical marks from the string `s`, E.g., 'żółw' is transformed
  to 'zolw'."
  [^String s]
  (.replaceAll (Normalizer/normalize s Normalizer$Form/NFD) "\\p{InCombiningDiacriticalMarks}+" ""))

(defn- get-root [^String xml-string]
  (some-> xml-string
          xml/parse-str
          zip/xml-zip))

(defn- urlize [^String s]
  (let [urlized (-> s remove-diacritics str/lower-case (str/replace #"[\s|\.]" "-"))]
    (str urlized ".aspx")))

(defn- tags->vector [article]
  (let [tags (some-> (zf/xml1-> article :tags zf/text)
                     (str/replace "#x20;" "")
                     (str/split #"\s"))]
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

(def joda-time-writer
  (transit/write-handler
   (constantly "m")
   #(-> % to-date .getTime)
   #(-> % to-date .getTime .toString)))

(defn trans-write [data file]
  (with-open [out (io/output-stream file)]
     (transit/write (transit/writer out :msgpack {:handlers {org.joda.time.DateTime joda-time-writer}}) data)))

(defn trans [root]
  (println "Processing articles")
  (time (trans-write (articles-result root) "out/articles.mp"))
  (println "Processing categories")
  (time (trans-write (rubrics-result root) "out/rubrics.mp")))

(defn -main [& args]
  (let [articles (slurp (io/resource "articles.xml"))
        root (get-root articles)]
    (trans root)))

;; (-main)
