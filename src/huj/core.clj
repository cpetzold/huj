(ns huj.core
  (:import (java.net MulticastSocket DatagramPacket InetAddress)
           (java.io ByteArrayInputStream))
  (:use [clojure.data.zip.xml :only (text xml->)])
  (:require [huj.utils :as utils]
            [clojure.string :as string]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clj-http.client :as client]
            [tinter.core :as tinter]))

(def +host+ "239.255.255.250")
(def +port+ 1900)
(def +broadcast+ "M-SEARCH * HTTP/1.1\r\nHOST: 239.255.255.250:1900\r\nMAN: ssdp:discover\r\nMX: 10\r\nST: ssdp:all\r\n")
(def +devicetype+ "huj")

(defn multicast-socket [host port]
  (let [sock (MulticastSocket. port)
        addr (InetAddress/getByName host)]
    (.joinGroup sock addr)
    sock))

(defn close-socket [sock]
  (.close sock))

(defn send-data [sock host port data]
  (let [addr (InetAddress/getByName host)
        bytes (.getBytes data)
        len (.length data)
        pack (DatagramPacket. bytes len addr port)]
    (.send sock pack)))

(defn receive-data [sock]
  (let [data (byte-array 1024)
        pack (DatagramPacket. data 1024)]
    (.receive sock pack)
    (String. (.getData pack) 0 (.getLength pack))))

(defn send-receive [sock host port data]
  (send-data sock host port data)
  (receive-data sock))

(defn parse-data [data]
  (into {} (map (fn [s]
                  (let [[key val] (string/split s #":" 2)]
                    {(keyword key)
                     (when val (string/trim val))}))
                  (string/split-lines data))))

(defn hub-data [data]
  (let [data (parse-data data)]
    (when-let [location (:LOCATION data)]
      (let [xml-str (:body (client/get location))
            xml-stream (ByteArrayInputStream. (.getBytes xml-str "UTF-8"))] 
        (when-let [hub (zip/xml-zip (xml/parse xml-stream))]
          {:url (str (first (xml-> hub :URLBase text)) "api")
           :name (first (xml-> hub :device :friendlyName text))
           :udn (first (xml-> hub :device :UDN text))})))))

(defn auth [hub username]
  (let [res (client/post (:url hub)
                         {:form-params {:username (utils/md5 username)
                                        :devicetype +devicetype+}
                          :content-type :json
                          :as :json})]
    (if-let [username (get-in res [:body 0 :success :username])]
      (assoc hub :username username)
      (print res))))

(defn connect [username]
  (let [sock (multicast-socket +host+ +port+)]
    (send-data sock +host+ +port+ +broadcast+)
    (let [hub (loop [data (receive-data sock)]
                (or (hub-data data) (recur (receive-data sock))))]
      (close-socket sock)
      (auth hub username))))

(defn request [hub method path & [params opts]]
  (let [params (if (= method :get)
                 {:query-params params}
                 {:form-params params})
        url (str (:url hub) (:username hub) "/" path)
        req (merge {:method method
                    :url url
                    :content-type :json
                    :as :json}
                   opts params)]
    (Thread/sleep (/ 1000 30))
    (:body (client/request req))))

(defn bulb-request [hub n method path & [params opts]]
  (request hub method (str "lights/" n "/" path) params opts))

(defn get-config [hub]
  (request hub :get "config"))

(defn replace-vals [m rmap]
  (into {} (for [[k v] m]
             [k
              (if (rmap k)
                ((rmap k) v)
                v)])))

(defn normalize-hue [h]
  (int (* (/ h 360.0) 65535)))
    
(defn normalize-state [state]
  (replace-vals state
                {:hue normalize-hue}))

(defn get-bulb-state [hub n]
  (bulb-request hub n :get ""))

(defn bulb-state [hub n state]
  (bulb-request hub n :put "state" (normalize-state state)))

(defn bulb-rgb [hub n rgb]
  (bulb-state hub n {:hue (tinter/hue rgb)}))

(defn hue-rainbow [hub n steps speed & [state]]
  (let [step-size (/ 360.0 steps)]
    (doseq [hue (range 0 360.0 step-size)]
      (bulb-state hub n (merge state {:hue hue}))
      (Thread/sleep speed))))

(defn strobe [hub n speed & [init-state]]
  (let [init-state (merge init-state {:transitiontime 0 :on true})
        !on (atom true)]
    (bulb-state hub n init-state)
    (while true
      (bulb-state hub n {:bri (if (swap! !on not) 254 0) :transitiontime 0})
      (Thread/sleep speed))))
    
    