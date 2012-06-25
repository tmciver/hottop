(ns hottop.samples.server
  (:require [hottop.samples.resource.contacts :as contacts]
            [hottop.core :as hottop]
            [hottop.resource :as resource]
            [ring.adapter.jetty :as ring-jetty]
            [ring.middleware.reload :as ring-reload]))

(def my-app (hottop/app ["contacts"] contacts/resource))

(defn -main
  [port]
  (ring-jetty/run-jetty my-app {:port (Integer/parseInt port)}))
