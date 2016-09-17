(ns generator.core
  (:require
    [clj-time.coerce :refer [from-string to-date]]
    [clojure.pprint]
    [clojure.java.io :as io]
    [clojure.zip :as zip]
    [clojure.data.xml :as xml]
    [clojure.data.zip :as zd]
    [clojure.data.zip.xml :as zf]
    [clojure.pprint :refer [pprint]]
    [clojure.string :as string])
  (:import
    (java.text Normalizer Normalizer$Form)
    (java.io StringWriter)))

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
                     (string/replace "," "")
                     (string/split #"\s"))
        tags (or tags [])]
    (filterv seq tags)))

(defn- article->detail-map [article]
  {:id (read-string (zf/xml1-> article :id zf/text))
   :title (zf/xml1-> article :title zf/text)
   :description (zf/xml1-> article :description zf/text)
   :raw (zf/xml1-> article :raw #(apply str (zf/xml-> % zd/descendants zip/node string?)))
   :author (zf/xml1-> article :author zf/text)
   :published (zf/xml1-> article :published zf/text)
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
    (map #(assoc % :category-url (urlize (:category %))))))

(defn- articles-result [root]
  (->> (zf/xml-> root :article)
       (into [] articles-by-url)))

(defn- article->rubrics-map [article]
  {:id (read-string (zf/xml1-> article :id zf/text))
   :title (zf/xml1-> article :title zf/text)
   :published (zf/xml1-> article :published zf/text)
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

(defn dump [m]
  (let [w (StringWriter.)]
    (pprint m w)
    (.toString w)))

(defn article->text [{:keys [url raw published] :as article}]
  [(str (first (string/split published #"T")) "-" url ".texy")
   (str
     "---\n"
     (dump (select-keys article [:id :title :description :author :tags :published :draft?]))
     "---\n\n"
     raw)])

(defn categories->tags [{:keys [tags category] :as article}]
  (let [tags (conj tags (string/lower-case category))]
    (assoc article :tags (set tags))))

(defn is-published->draft [{:keys [is-published] :as article}]
  (if-not is-published
    (assoc article :draft? true)
    article))

(defn trans-text [root]
  (->> root
       articles-result
       (map categories->tags)
       (map is-published->draft)
       (remove #(empty? (:tags %)))
       (map article->text)))

(defn -main [& args]
  (let [articles (slurp (io/resource "articles.xml"))
        root (get-root articles)]
    (doseq [[file content] (trans-text root)]
      (spit (str "out/weblog/" file) content))))

;; (-main)

(comment
  (def articles (slurp (io/resource "articles.xml")))
  (def root (get-root articles)))
