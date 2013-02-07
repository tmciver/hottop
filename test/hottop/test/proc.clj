(ns hottop.test.proc
  (:use hottop.proc
        hottop.resource
        clojure.test
        ring.mock.request)
  (:require [clojure.string :as str]
            [ring.util.response :as ring]
            [hottop.response :as response]))

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
      (is (= 401 (:status response2)))))

  (testing "Test Validate Accept"
    (let [request1 (-> (request :get "/test")
                       (header "Accept" "text/html"))
          request2 (-> (request :get "/test")
                       (header "Accept" "text/csv"))
          request3 (-> (request :get "/test")
                       (header "Accept" "*/*"))
          resource1 (create-readonly-html-resource (constantly "hello!"))
          resource2 (-> base-resource
                        (assoc-in [:methods :get] (constantly "hello."))
                        (assoc-in [:content-types-provided "text/csv"] identity))
          response1 ((validate-accept (fn [r _] r)) request1 resource1)
          response2 ((validate-accept (constantly :handler2)) request2 resource1)
          response3 ((validate-accept (fn [r _] r)) request2 resource2)
          response4 ((validate-accept (fn [r _] r)) request3 resource1)]
      (is (= (:optimal-ct response1) "text/html"))
      (is (= response2 (response/code 406)))
      (is (= (:optimal-ct response3) "text/csv"))
      (is (= (:optimal-ct response4) "text/html"))))

  (testing "Process GET"
    (let [request1 (-> (request :get "/test")
                       (header "Accept" "text/html"))
          request2 (request :post "/test")
          request3 (-> (request :get "/test")
                       (header "Accept" "text/plain"))
          resource (create-readonly-html-resource (constantly "Hello!") identity)
          response1 ((process-get (constantly :handler1)) request1 resource)
          response2 ((process-get (constantly :handler2)) request2 resource)
          response3 ((process-get (constantly :handler3)) request3 resource)]
      (is (= response1 (-> (ring/response "Hello!")
                           (ring/header "content-type" "text/html"))))
      (is (= response2 :handler2))
      (is (= response3 (response/code 406)))))

  (testing "Process POST"
    (let [request1 (-> (request :post "/test")
                       (header "Accept" "text/html"))
          request2 (-> (request :post "/test")
                       (header "Accept" "text/plain"))
          resource (-> base-resource
                       (assoc-in [:methods :post] (constantly :do-nothing))
                       (assoc :redirect-after-html-post (constantly "/foo"))
                       (assoc-in [:content-types-provided "text/html"] identity)
                       (assoc-in [:content-types-provided "text/plain"] identity))
          response1 ((process-post (constantly :handler1)) request1 resource)
          response2 ((process-post (constantly :handler2)) request2 resource)]
      (is (and (= (:status response1) 303)
               (= (get-in response1 [:headers "Location"]) "/foo")))
      (is (= response2 (response/code 200))))))
