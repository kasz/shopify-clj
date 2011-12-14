(ns shopify-clj.core
  (:require [clj-http.client :as client]
            [digest]
            [clj-json.core :as json]))

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

(defn- valid-signature? [app-credentials shopify-response]
  (let [calculated-signature (sig-digest (:shared-secret app-credentials) (pre-signature shopify-response))
        signature (get shopify-response "signature")]
    (= signature calculated-signature)))

;;; structure of that type should be passed as app-credentials parameter in the following function
(defstruct password-credentials :shared-secret)

(defn generate-password
  "Function responsible for generating password. It requries shared secret of
   application, wrapped in password-credentials (app-credentials) struct nad
   map if parameters received when user installed application (shopify-response)."
  [app-credentials shopify-response]
  (if (valid-signature? app-credentials shopify-response)
    (let [secret (get app-credentials :shared-secret)
          token (get shopify-response "t")]
      (merge app-credentials {:password (digest/md5 (str secret token))}))
    nil))

;; API Calls

;;; structure of that type should be passed as credentials parameter in the following functions
(defstruct request-credentials :shop :api-key :password :shared-secret)

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

;;; maybe use macro for following functions?
(defn shopify-read [credentials route]
  (let [request (str (endpoint credentials) route)]
    (json/parse-string (:body (client/get request (request-data credentials))))))

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
    (:body (client/delete request (request-data credentials)))))
