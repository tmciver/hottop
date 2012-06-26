(ns hottop.samples.contacts-app.server
  (:require [hottop.samples.contacts-app.resources :as resources]
            [hottop.core :as hottop]
            [hottop.resource :as resource]
            [ring.adapter.jetty :as ring-jetty]
            [ring.middleware.reload :as ring-reload]))

(def my-app (hottop/app ["contacts"] resources/contacts))

(defn -main
  [port]
  (ring-jetty/run-jetty my-app {:port (Integer/parseInt port)}))
