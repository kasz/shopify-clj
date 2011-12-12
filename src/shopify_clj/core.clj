(ns shopify-clj.core
  (:require
   [clj-http.client :as client]
   [digest]
   [clojure.contrib.json :as json]))


;; Authentication stuff
(defn sig-digest [shared-secret pre-signature]
  (let [pre-sig-digest (reduce
                        (fn [res next]
                          (if (or (= nil next) (= "" next))
                            res
                            (str res next))) pre-signature)]
    (digest/md5 (str shared-secret pre-sig-digest))))

(defn pre-signature [response-map]
  (sort
   (map (fn [entry]
          (if (not= "signature" (first entry)) (str (first entry) "=" (last entry)) nil))
        response-map)))

(defn valid-signature? [app-credentials shopify-response]
  (let [calculated-signature (sig-digest (:shared-secret app-credentials) (pre-signature shopify-response))
        signature (get shopify-response "signature")]
    (= signature calculated-signature)))

(defn generate-password [app-credentials shopify-response]
  (if (valid-signature? app-credentials shopify-response)
    (let [secret (get app-credentials :shared-secret)
          token (get shopify-response "t")]
      (merge app-credentials {:password (digest/md5 (str secret token))}))
    nil))

;; API Calls
(defn construct
  "builds the route from the give arguments"
  ([] "")
  ([x] (str "/" x))
  ([x & more]
     (reduce #(str %1 (construct %2)) (construct x) more)))

(defn request-data [credentials]
  {:basic-auth [(:api-key credentials) (:password credentials)]
   :content-type :json
   :accept :json})

(defn endpoint [credentials]
  (str (:shop credentials) "/admin"))

(defn shopify-read [credentials route]
  (let [request (str (endpoint credentials) route)]
    (do
      (println request)
      (json/read-json (:body (client/get request (request-data credentials)))))))

(defn shopify-create [credentials route data]
  (let [request (str (endpoint credentials) route)
        args (merge {:body (json/json-str data)} (request-data credentials))]
    (json/read-json (:body (client/post request args)))))

(defn shopify-update [credentials route data]
  (let [request (str (endpoint credentials) route)
        args (merge {:body (json/json-str data)} (request-data credentials))]
    (json/read-json (:body (client/put request args)))))

(defn shopify-delete [credentials route]
  (let [request (str (endpoint credentials) route)]
    (:body (client/delete request (request-data credentials)))))
