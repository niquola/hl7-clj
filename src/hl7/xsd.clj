(ns hl7.xsd
  (:require [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [clj-yaml.core :as yaml]

            [clojure.string :as str]))


(defn by-tag [tag]
  (fn [x] (when-let [t (:tag x)] (= tag (name t)))))

(defn process-complex-type [t]
  (when t
    (->> t
         (:content)
         (filterv (by-tag "sequence"))
         first
         (:content)
         (filterv (by-tag "element"))
         (mapv :attrs)
         (mapv (fn [attrs]
                 (cond-> {:ref (:ref attrs)}
                   (= "1" (:minOccurs attrs)) (assoc :required true)
                   (= "unbounded" (:maxOccurs attrs)) (assoc :collection true)))))))

(defn process [doc]
  (let [types-idx (->> (:content doc)
                       (filter (by-tag "complexType"))
                       (group-by #(get-in % [:attrs :name])))
        el-idx (->> (:content doc)
                    (filter (by-tag "element"))
                    (group-by #(get-in % [:attrs :name])))]
    (reduce (fn [acc [nm [el]]]
              (let [attrs (:attrs el)
                    tp-nm (:type attrs)
                    tp (first (get types-idx tp-nm))]
                (assoc acc nm (assoc attrs
                                     :segments (process-complex-type tp)))))
            {} el-idx)))

(defn *treefy [idx sch]
  (update sch :segments
          (fn [es]
            (when es
              (->> es
                   (mapv (fn [e]
                           (when (and e (:ref e))
                             (if (str/includes? (:ref e) ".")
                               (-> (merge e (*treefy idx (get idx (:ref e))))
                                   (dissoc :ref :type)
                                   (update :name (fn [x] (str/lower-case (last (str/split x #"\."))))))
                               (-> e
                                   (dissoc :ref)
                                   (assoc :seg (:ref e))))))))))))

(defn treefy [sch]
  (reduce (fn [acc [k e]]
            (if-not (str/includes? k ".")
              (assoc acc k (dissoc (*treefy sch e) :type))
              acc))
          {} sch))

(defn get-doc [x]
  (->> x
       :content
       (filterv (by-tag "annotation"))
       first
       :content
       (filterv (by-tag "documentation"))
       first
       :content
       first))

(defn get-type [x]
  (->> x
       :content
       (filterv (by-tag "annotation"))
       first
       :content
       (filterv (by-tag "appinfo"))
       first
       :content
       (filterv (by-tag "Type"))
       first
       :content
       first))

(defn process-field-content [x]
  {:name (get-doc x)
   :type (get-type x)})

(defn process-fields [idx els]
  (when els
    (->> els
        (mapv (fn [{ref :ref :as attrs}]
                (if-let [tp (first (get idx (str ref ".CONTENT")))]
                  (process-field-content tp)
                  {:error "ups no content"}))))))

(defn datatypes [doc]
  (let [ types-idx (->> (:content doc)
                        (filter (by-tag "complexType"))
                        (group-by #(get-in % [:attrs :name])))]

    types-idx
    (reduce (fn [acc [k [v]]]
              (if-not (str/includes? k ".")
                (assoc acc k (process-fields types-idx (process-complex-type v)))
                acc) 
              ) {} types-idx)))

(defn fields [doc]
  (let [attrs-idx (->> (:content doc)
                       (filter (by-tag "attributeGroup"))
                       (group-by #(get-in % [:attrs :name])))
        types (->> (:content doc) (filter (by-tag "complexType")))]
    (reduce (fn [acc tp]
              (let [nm (str/replace (get-in tp [:attrs :name]) #"\.CONTENT$" "")]
                (assoc acc nm {:name (get-doc tp)
                               :type (get-type tp)})
                )) {} types)))

(defn segments [segs-doc fields-doc]
  (let [fld-idx (fields fields-doc)
        segs (process segs-doc)]

    (reduce (fn [acc [k v]]
              (assoc acc k
                     (->> (:segments v)
                          (mapv (fn [{ref :ref :as cmp}]
                                  (merge cmp (get fld-idx ref))))))
              ) {} segs)))


(defn load [v]
  (let [types-doc (xml/parse-str (slurp (.getPath (io/resource (str v "/datatypes.xsd")))))
        flds-doc (xml/parse-str (slurp (.getPath (io/resource (str v "/fields.xsd")))))
        segs-doc (xml/parse-str (slurp (.getPath (io/resource (str v "/segments.xsd")))))]
    (->> (file-seq (io/file (.getPath (io/resource v))))
         (reduce (fn [acc f]
                   (let [nm (str/replace (.getName f) #".xsd$" "")]
                     (if (str/includes? nm "_")
                       (let [doc (xml/parse-str (slurp (.getPath f)))]
                         (println (.getPath f))
                         (merge acc (-> (process doc) (treefy))))
                       acc)))
                 {})
         (assoc {:types (datatypes types-doc)
                 :segments (segments segs-doc flds-doc)}
                :messages))))

(defn take-until [f col]
  (loop [acc [] [x & xs] col]
    (if (nil? x)
      acc
      (if (f x)
        (conj acc x)
        (recur (conj acc x) xs)))))

(defmulti reduce-machine
  (fn [acc {sm-state :sm-state
            cur-seg :current-seg
            segs-tail :segments-rest}]
    (cond
      (:seg cur-seg)  :segment
      (:name cur-seg) :group
      :else           :default)))

(defmethod reduce-machine
  :default [acc _] acc)

(defn state-name [path seg]
  (->>
   (into (conj (mapv name path) (name (or (:name seg) (:seg seg)))))
   (str/join "-" )
   (keyword)))

(state-name [] {:seg :PID})

(defn forward-transitions [segs-tail path & [coll]]
  (->> segs-tail
       (take-until :required)
       (reduce (fn [acc s]
                 (if (:seg s)
                   (assoc acc (keyword (:seg s)) (cond-> {:next (state-name path s)}
                                                   (not (empty? path)) (assoc :path path)
                                                   (or coll (:collection s)) (assoc :collection true)))
                   (merge acc (forward-transitions (:segments s) (conj path (:name s)) (:collection s)))
                   )) {})))

(defn backward-transitions [segs path {ss :stop-segment sp :stop-path :as opts}]
  (->> segs
       (take-until (fn [x] (or (:required x) #_(and (= ss x) (= sp path)))))
       (reduce (fn [acc s]
                 (if (:seg s)
                   (assoc acc (keyword (:seg s)) (cond-> {:next (state-name path s)
                                                          :back true
                                                          :path path
                                                          :collection true}
                                                   (not (empty? path)) (assoc :path path)))
                   (merge acc (backward-transitions (:segments s) (conj path (:name s)) opts))
                   )) {})))


(defn reduce-tail [acc
                   {path :path
                    transitions :backward-transitions
                    segs-tail :segments-rest
                    :as opts}]
  (loop [acc acc [s & ss] segs-tail]
    (if (nil? s)
      acc
      (let [acc (let [next-state (state-name path s)]
                  (if-not (get acc next-state)
                    (reduce-machine acc (assoc opts
                                               :sm-state next-state
                                               :backward-transitions transitions
                                               :current-seg s
                                               :segments-rest ss))
                    acc))]
        (if-not (:required s) (recur acc ss) acc)))))

(defmethod reduce-machine
  :segment
  [acc {{seg-nm :seg col :collection :as cur-seg} :current-seg
        path :path
        bwt :backward-transitions
        segs-tail :segments-rest
        :as opts}]
  (let [transitions (apply merge
                           (conj bwt
                                 (if col {(keyword seg-nm) {:next (state-name path cur-seg)
                                                            :path path
                                                            :back true
                                                            :collection true}} {})
                                 (forward-transitions segs-tail path)))
        acc (assoc acc (state-name path cur-seg) transitions)]
    (reduce-tail acc opts)))

(defmethod reduce-machine
  :group
  [acc {sm-state :sm-state
        {seg-nm :name coll :collection :as cur-seg} :current-seg
        path :path
        bwt :backward-transitions
        segs-tail :segments-rest
        :as opts}]
  (let [transitions (apply merge (conj bwt (forward-transitions segs-tail path)))
        acc (reduce-tail acc opts)]
    (loop [acc acc [s & ss] (:segments cur-seg)]
      (if (nil? s)
        acc
        (let [next-state (state-name path s)
              new-path (conj path (:name cur-seg))
              btrs     (if coll (backward-transitions (:segments cur-seg) new-path {:stop-segment s :stop-path new-path}) {})
              acc (if-not (get acc next-state)
                    (-> acc
                        (assoc (state-name new-path s) transitions)
                        (reduce-machine (assoc opts
                                               :sm-state next-state
                                               :path (conj path (:name cur-seg))
                                               :backward-transitions (into bwt [transitions btrs])
                                               :current-seg s
                                               :segments-rest ss)))
                    acc)]
          (if-not (:required s) (recur acc ss) acc))))))

(defn state-machine [msg]
  (reduce-machine {}
                  {:sm-state :start
                   :current-seg {:seg :start}
                   :path []
                   :backward-transitions []
                   :segments-rest (into [] (conj (:segments msg) {:seg :end}))}))


(def mt (get-in metadata [:messages "ORU_R01"]))

(def sm (state-machine mt))

(dump {:msg mt :sm sm})

(:start sm)

(defn test-parse [segs sm]
  (reduce (fn [state s]
            (let [tr (get sm state)
                  next-tr (get tr s)]
              (println
               (str
                ;; (str/join (repeat (count (:path next-tr)) " "))
                #_(when (or (:collection next-tr) (:back next-tr)) "* ")
                (str/join "." (conj (:path next-tr) (name s)))
                " "
                (select-keys next-tr [:collection :back])
                ))
              (or (:next next-tr) state)))
          :start segs))


(def tmsg  [:MSH :PID :NK1 :PV1 :PV2
            :OBR :NTE :OBX :OBX :OBX
            :ORC :OBR :OBX :NTE :NTE
            :OBR :NTE :NTE :TQ1 :TQ2 :TQ2 :CTD
              :OBX :NTE :NTE :CTI :SPM :OBX :OBX :SPM
            :PID :OBR :OBX
            :PID :OBR :OBX :OBR :OBX
            :DSC
            ])


(test-parse tmsg sm)

(comment

  (defn dump [x] (spit "/tmp/hl7.yaml" (yaml/generate-string x)))

  (dump (state-machine (get-in metadata [:messages "ADT_A01"])))
  (defonce metadata (load "2.5.1"))

  #_{:sch (get-in metadata [:messages "ADT_A01"])}

  (dump (state-machine (get-in metadata [:messages "ADT_A01"])))
  (dump (get-in metadata [:messages "ADT_A01"]))

  ;; (dump (datatypes dts))
  ;; (dump (-> (segments segs flds)))
  (def res (xml/parse-str (slurp (io/resource "2.5.1/DFT_P03.xsd"))))

  (def res (xml/parse-str (slurp (io/resource "2.5.1/ADT_A05.xsd"))))

  ;; (def dts (xml/parse-str (slurp (io/resource "2.5.1/datatypes.xsd"))))
  (def segs (xml/parse-str (slurp (io/resource "2.5.1/segments.xsd"))))
  (def flds (xml/parse-str (slurp (io/resource "2.5.1/fields.xsd"))))



;; (dump (:messages metadata))


;; (-> (load "2.5.1") dump)


)
(comment

  (require '[cheshire.core])

  (def bndl (cheshire.core/parse-string (slurp "/Users/nicola/Downloads/fhir/Abbott657_Deann989_30.json") keyword))

  (spit "/tmp/abb.yaml" (clj-yaml.core/generate-string (mapv :resource (:entry bndl))))
  

  )
