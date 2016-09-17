(ns converter.core
  (:require
    [clj-time.coerce :refer [from-string to-date]]
    [clojure.pprint]
    [clojure.java.io :as io]
    [clojure.zip :as zip]
    [clojure.data.xml :as xml]
    [clojure.data.zip :as zd]
    [clojure.data.zip.xml :as zf]
    [clojure.string :as string]
    [cognitect.transit :as transit])
  (:import
    (java.text Normalizer Normalizer$Form)
    (org.joda.time DateTime)))

(defn remove-diacritics
  "Remove diacritical marks from the string `s`, E.g., 'żółw' is transformed
  to 'zolw'."
  [^String s]
  (let [normalized (Normalizer/normalize s Normalizer$Form/NFD)]
    (.replaceAll normalized "\\p{InCombiningDiacriticalMarks}+" "")))

(defn- get-root [^String xml-string]
  (some-> xml-string
          xml/parse-str
          zip/xml-zip))

(defn- urlize [^String s]
  (let [urlized (-> s remove-diacritics string/lower-case (string/replace #"[\s|\.]" "-"))]
    (str urlized ".aspx")))

(defn- tags->vector [article]
  (let [tags (some-> (zf/xml1-> article :tags zf/text)
                     (string/replace "#x20;" "")
                     (string/split #"\s"))
        tags (or tags [])]
    (filterv seq tags)))

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

(def ^:private blog-rubrics
  (comp
    (remove #(= (:category %) "Reference"))
    (remove #(= (:category %) "Projekty"))
    (filter :is-published)))

(def ^:private articles-by-url
  (comp
    (map article->detail-map)
    blog-rubrics
    (map #(assoc % :category-url (urlize (:category %))))
    (map index-by-url)))

(defn- articles-result [root]
  (->> (zf/xml-> root :article)
       (into {} articles-by-url)))

(defn- article->rubrics-map [article]
  {:id (read-string (zf/xml1-> article :id zf/text))
   :title (zf/xml1-> article :title zf/text)
   :published (from-string (zf/xml1-> article :published zf/text))
   :url (zf/xml1-> article :url zf/text)
   :category (zf/xml1-> article :category zf/text)
   :is-published (= 1 (read-string (zf/xml1-> article :is-published zf/text)))})

(def ^:private articles-for-rubric
  (comp
    (map article->rubrics-map)
    blog-rubrics))

(defn- rubrics-result [root]
  (->> (zf/xml-> root :article)
       (into [] articles-for-rubric)
       (sort-by :id >)
       (group-by (comp keyword urlize :category))))

(def joda-time-writer
  (transit/write-handler
    (constantly "m")
    #(-> % to-date .getTime)
    #(-> % to-date .getTime .toString)))

(defn write [data file format]
  (with-open [out (io/output-stream file)]
    (let [opts {:handlers {DateTime joda-time-writer}}
          writer (transit/writer out format opts)]
      (transit/write writer data))))

(defn trans-json [root]
  (println "Processing articles")
  (time (write (articles-result root) "out/articles.json" :json-verbose))
  (println "Processing categories")
  (time (write (rubrics-result root) "out/rubrics.json" :json-verbose)))


(defn trans-mp [root]
  (println "Processing articles")
  (time (write (articles-result root) "out/articles.mp" :msgpack))
  (println "Processing categories")
  (time (write (rubrics-result root) "out/rubrics.mp" :msgpack)))

(defn -main [& args]
  (let [articles (slurp (io/resource "articles.xml"))
        root (get-root articles)]
    (trans-json root)))

;; (-main)
