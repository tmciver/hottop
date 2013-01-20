(ns hottop.test.proc
  (:use hottop.proc
        hottop.resource
        clojure.test
        ring.mock.request)
  (:require [clojure.string :as str]))

(deftest test-http-processors

  (testing "Test for OPTION handling"
    (let [request (request :options "/test")
          resource (-> base-resource
                       (assoc-in [:methods :get] (constantly "Hello!"))
                       (assoc-in [:methods :post] (constantly "Hello!"))
                       (assoc-in [:methods :put] (constantly "Hello!")))
          response ((process-options identity) request resource)
          option-strs (-> response
                          :headers
                          (get "Allow")
                          (str/split #",")
                          (#(map str/trim %))
                          set)]
      (is (and (= option-strs #{"GET" "PUT" "POST"})
               (= 200 (:status response))))))

  (testing "Test for method implemented"
    (let [request1 (request :get "/test")
          request2 (request :post "/test")
          resource (create-readonly-html-resource (constantly "Hello!") identity)
          response1 ((validate-method (constantly :handler1)) request1 resource)
          response2 ((validate-method identity) request2 resource)]
      (is (= response1 :handler1))
      (is (= 405 (:status response2)))))

  (testing "Test Authorization"
    (let [request (request :get "/test")
          resource1 base-resource
          resource2 (assoc base-resource :auth (constantly false))
          response1 ((check-authorization (constantly :handler1)) request resource1)
          response2 ((check-authorization identity) request resource2)]
      (is (= response1 :handler1))
      (is (= 401 (:status response2))))))
