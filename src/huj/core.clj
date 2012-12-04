(ns huj.core
  (:import (java.net MulticastSocket DatagramPacket InetAddress)
           (java.io ByteArrayInputStream))
  (:use [clojure.data.zip.xml :only (attr text xml->)])
  (:require [huj.utils :as utils]
            [clojure.string :as string]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clj-http.client :as client]))

(def +host+ "239.255.255.250")
(def +port+ 1900)
(def +broadcast+ "M-SEARCH * HTTP/1.1\r\nHOST: 239.255.255.250:1900\r\nMAN: ssdp:discover\r\nMX: 10\r\nST: ssdp:all\r\n")

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
      (zip/xml-zip (xml/parse xml-stream))))))


(defn connect [username]
  (let [sock (multicast-socket +host+ +port+)]
    (send-data sock +host+ +port+ +broadcast+)
    (loop [data (receive-data sock)]
      (or (hub-data data) (recur (receive-data sock))))))
    



         