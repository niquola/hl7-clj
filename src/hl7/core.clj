(ns hl7.core
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]))


(defn parse-subcomp [fs]
  (loop [acc {}, i 1
         [x & xs]  (str/split fs #"\^")]
    (if-not x
      acc
      (recur (if (not (str/blank? x)) (assoc acc i x) acc)
             (inc i) xs))))

(defn parse-comp [fs]
  (loop [acc {}, i 1
         [x & xs]  (str/split fs #"~")]
    (if-not x
      acc
      (recur (if (not (str/blank? x)) (assoc acc i (parse-subcomp x)) acc)
             (inc i) xs))))

(defn parse-fields [fs]
  (loop [acc {}, i 1
         [x & xs]  (str/split fs #"\|")]
    (if-not x
      acc
      (recur (if (not (str/blank? x))
               (assoc acc i (parse-comp x))
               acc)
             (inc i) xs))))

(defn generic-parse [m]
  (->> (str/split m #"(\r|\n)")
       (mapv (fn [s]
                 (let [[seg-name fs] (str/split s #"\|" 2)
                       fields (parse-fields fs)]
                   (assoc fields 0 seg-name))))))

  (def msg "MSH|^~\\&|GHH LAB|ELAB-3|GHH OE|BLDG4|200202150930||ORU^R01|CNTRL-3456|P|2.4
PID|||555-44-4444||EVERYWOMAN^EVE^E^^^^L|JONES|19620320|F|||153 FERNWOOD DR.^^STATESVILLE^OH^35292||(206)3345232|(206)752-121||||AC555444444||67-A4335^OH^20030520
OBR|1|845439^GHH OE|1045813^GHH LAB|1554-5^GLUCOSE|||200202150730|||||||||555-55-5555^PRIMARY^PATRICIA P^^^^MD^^|||||||||F||||||444-44-4444^HIPPOCRATES^HOWARD H^^^^MD
OBX|1|SN|1554-5^GLUCOSE^POST 12H CFST:MCNC:PT:SER/PLAS:QN||^182|mg/dl|70_105|H|||F")

(generic-parse msg)

(def metadata (yaml/parse-string (slurp (.getPath (io/resource "2.5.1-schema.yaml")))))

(keys metadata)

(defn find-meta [sn mss]
  (loop [[ms & mss] mss]
    (if-not ms nil
      (if (= sn (:seg ms)) ms (recur mss)))))

(defn parse [metadata msg-str]
  (let [msg (generic-parse msg-str)
        tp  (let [t (get-in msg [0 8 1])] (keyword (str (get t 1) "_" (get t 2))))
        msg-meta (get-in metadata [:messages tp])]
    msg-meta
    (loop [mss (:segments msg-meta)
           cur 0
           [s & ss] msg
           res {}]
      (if-not s
        res
        (let [sn (second (first s))
              mt (find-meta sn mss)]
          (recur mss (inc cur) ss
                 (assoc res sn {:data s :meta mt})))))))

(comment
  (dump
   (parse metadata msg)))



