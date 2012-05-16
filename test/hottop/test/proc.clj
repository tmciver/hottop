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
          [_ _ response _] (process-options resource request {} {})
          option-strs (-> response
                          :headers
                          (get "Allow")
                          (str/split #",")
                          (#(map str/trim %))
                          set)]
      (is (and (= option-strs #{"GET" "PUT" "POST" "OPTIONS"})
               (= 200 (:status response))))))

  (testing "Test for method implemented"
    (let [request1 {:request-method :get}
          request2 {:request-method :post}
          resource (update-in base-resource [:methods :get]
                              (fn [request] "Hello!"))
          [_ _ response1 _] (validate-method resource request1 {} {})
          [_ _ response2 _] (validate-method resource request2 {} {})]
      (is (nil? (:status response1)))
      (is (= 405 (:status response2)))))

  (testing "Test Authorization"
    (let [request {:request-method :get}
          [_ _ response1 _] (validate-authorization base-resource request {} {})
          [_ _ response2 _] (validate-authorization
                             (assoc base-resource :auth (constantly false))
                             request {} {})]
      (is (nil? (:status response1)))
      (is (= 401 (:status response2))))))