(ns hottop.test.core
  (:use [clojure.test])
  (:require [ring.mock.request :as ring-mock]
            [hottop.core :as core]
            [hottop.resource :as resource]))

(deftest test-run-processors
  (let [hello-resource (resource/create-readonly-html-resource (constantly "Hello!"))
        hello-get-request (-> (ring-mock/request :get "/hello")
                          (ring-mock/header "accept" "text/html"))
        hello-put-request (-> (ring-mock/request :put "/hello")
                              (ring-mock/header "accept" "text/html"))]
    (is (= 200 (:status (core/run-processors hello-resource hello-get-request))))
    (is (= 405 (:status (core/run-processors hello-resource hello-put-request))))))
