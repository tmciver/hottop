(ns hottop.samples.contacts-app.server
  (:require [hottop.samples.contacts-app.resources :as resources]
            [hottop.core :as hottop]
            [hottop.resource :as resource]
            [ring.adapter.jetty :as ring-jetty]))

(def my-app (hottop/app ["contacts"] resources/contacts
                        ["create-contact"] resources/create-contact))

(defn -main
  [port]
  (ring-jetty/run-jetty my-app {:port (Integer/parseInt port)}))
