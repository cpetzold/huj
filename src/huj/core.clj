(ns huj.core
  (:require [huj.utils :as utils]
            [lamina.core :as lamina]
            [aleph.udp :as udp]
            [aleph.http :as http]))

(defn connect [port]
  (let [socket (udp/udp-socket {:port port})]
    (type socket)))

(connect 1336)