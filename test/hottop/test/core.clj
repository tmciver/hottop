(ns hottop.test.core
  (:use [clojure.test])
  (:require [ring.mock.request :as ring-mock]
            [hottop.core :as core]
            [hottop.resource :as resource]))

(deftest test-handler
  (let [resources (conj [] (resource/create-readonly-html-resource "/hello" (constantly "Hello!")))
        resources (conj resources (resource/create-readonly-html-resource "/goodbye" (constantly "See ya!")))
        my-handler (core/handler resources)
        hello-request (-> (ring-mock/request :get "/hello")
                          (ring-mock/header "accept" "text/html"))
        goodbye-request (-> (ring-mock/request :get "/goodbye")
                          (ring-mock/header "accept" "text/html"))
        another-request (-> (ring-mock/request :get "/whatsup")
                          (ring-mock/header "accept" "text/html"))
        hello-status (:status (my-handler hello-request))
        goodbye-status (:status (my-handler goodbye-request))
        another-status (:status (my-handler another-request))]
    (is (= hello-status 200))
    (is (= goodbye-status 200))
    (is (= another-status 404))))
