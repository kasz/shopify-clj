(ns shopify-clj.core
  (:require [clj-http.client :as client]
            [digest]
            [clj-json.core :as json]))

;;; Constants
(def limit-time-reset 300)              ; in seconds

;; Authentication stuff
(defn- sig-digest [shared-secret pre-signature]
  (let [pre-sig-digest (reduce
                        (fn [res next]
                          (if (or (= nil next) (= "" next))
                            res
                            (str res next)))
                        pre-signature)]
    (digest/md5 (str shared-secret pre-sig-digest))))

(defn- pre-signature [response-map]
  (sort
   (map (fn [entry]
          (if (not= "signature" (first entry)) (str (first entry) "=" (last entry)) nil))
        response-map)))

(defn- valid-signature? [shared-secret shopify-response]
  (let [calculated-signature (sig-digest shared-secret (pre-signature shopify-response))
        signature (get shopify-response "signature")]
    (= signature calculated-signature)))

(defn generate-password
  "Function responsible for generating password. It requries shared secret of
   application and map of parameters received when user installed application 
   (shopify-response)."
  [shared-secret shopify-response]
  (if (valid-signature? shared-secret shopify-response)
    (let [token (get shopify-response "t")]
      (digest/md5 (str shared-secret token)))
    nil))

;; API Calls

;;; structure of that type should be passed as credentials parameter in the following functions
(defstruct request-credentials :shop :api-key :password)

(defn- construct
  "Builds the route from the given arguments."
  ([] "")
  ([x] (str "/" x))
  ([x & more]
     (reduce #(str %1 (construct %2)) (construct x) more)))

(defn- request-data [credentials]
  {:basic-auth [(:api-key credentials) (:password credentials)]
   :content-type :json
   :accept :json})

(defn- endpoint [credentials]
  (str "https://" (:shop credentials) "/admin"))

(defmacro def-shopify-api-fn
  "Macro responsible for creating functions calling Shopify API. 
   Created function will be named shopify-name, where name needs to be passed as first parameter.
   Second parameter must be a string containing HTTP verb (GET, POST, PUT or DELETE)."
  [name http-verb]
  (let [http-client-function @(resolve (symbol (str "client/" (.toLowerCase http-verb))))]
    (if (or (= http-client-function client/post) (= http-client-function client/put))
      `(defn ~(symbol (str "shopify-" name)) [credentials# route# data#]
         (let [request# (str (endpoint credentials#) route#)
               args# (merge {:body (json/generate-string data#)} (request-data credentials#))]
           (json/parse-string (:body (~http-client-function request# args#)))))
      `(defn ~(symbol (str "shopify-" name)) [credentials# route#]
         (let [request# (str (endpoint credentials#) route#)
               args# (request-data credentials#)]
           (json/parse-string (:body (~http-client-function request# args#))))))))

(def-shopify-api-fn "read" "get")

(def-shopify-api-fn "create" "post")

(def-shopify-api-fn "update" "put")

(def-shopify-api-fn "delete" "delete")

(comment (defn shopify-read [credentials route]
   (let [request (str (endpoint credentials) route)
         args (request-data credentials)]
     (json/parse-string (:body (client/get request args)))))

 (defn shopify-create [credentials route data]
   (let [request (str (endpoint credentials) route)
         args (merge {:body (json/generate-string data)} (request-data credentials))]
     (json/parse-string (:body (client/post request args)))))

 (defn shopify-update [credentials route data]
   (let [request (str (endpoint credentials) route)
         args (merge {:body (json/generate-string data)} (request-data credentials))]
     (json/parse-string (:body (client/put request args)))))

 (defn shopify-delete [credentials route]
   (let [request (str (endpoint credentials) route)]
     (json/parse-string (:body (client/delete request (request-data credentials))))))
 )