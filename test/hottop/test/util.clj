(ns hottop.test.util
  (:use clojure.test
        ring.mock.request
        hottop.util
        hottop.resource))

(deftest test-accept-segment-to-map
  (let [segment "text/html;level=1;q=0.7"
        segment-map (#'hottop.util/accept-segment-to-map segment)]
    (is (= segment-map
           {:type "text"
            :subtype "html"
            :level "1"
            :q "0.7"}))))

(deftest test-accept-map-to-type-str
  (let [segment "text/html;level=1;q=0.7"
        segment-map (#'hottop.util/accept-segment-to-map segment)
        type-str (#'hottop.util/type-map-to-content-str segment-map)]
    (is (= type-str "text/html"))))

(deftest test-parse-accept-header
  (testing "Test Accept headers"
    (is (= (parse-accept-header "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")
           [{:q 0.3, :type "text" :subtype "*"}
            {:q 0.7, :type "text" :subtype "html"}
            {:q 1.0, :level "1", :type "text" :subtype "html"}
            {:q 0.4, :level "2", :type "text" :subtype "html"}
            {:q 0.5, :type "*" :subtype "*"}])))
  (testing "Test Accept-Encoding headers"
    (is (= (parse-accept-header "gzip;q=1.0, identity; q=0.5, *;q=0")
           [{:q 1.0, :type "gzip" :subtype nil}
            {:q 0.5, :type "identity" :subtype nil}
            {:q 0.0, :type "*" :subtype nil}]))))

(deftest test-provided-maps
  (let [res (-> base-resource
                (add-view "text/html" identity)
                (add-view "text/csv" identity)
                (add-view "application/json" identity))]
    (is (= (#'hottop.util/provided-maps res)
           #{{:type "text" :subtype "html"}
             {:type "text" :subtype "csv"}
             {:type "application" :subtype "json"}}))))

(deftest test-allow-header-str
  (let [resource (-> base-resource
                     (assoc-in [:methods :get] (constantly "Hello!"))
                     (assoc-in [:methods :post] (constantly "Hello!"))
                     (assoc-in [:methods :put] (constantly "Hello!")))]
    (is (= (allow-header-str resource)
           "PUT, POST, GET"))))

(deftest test-accepts-html?
  (let [req1 (-> (request :get "/test")
                 (header "Accept" "text/html"))
        req2 (-> (request :get "/test")
                 (header "Accept" "text/csv"))
        req3 (-> (request :get "/test")
                 (header "Accept" "application/xhtml+xml"))
        req4 (-> (request :get "/test")
                 (header "Accept" "text/*"))]
    (is (accepts-html? req1))
    (is (not (accepts-html? req2)))
    (is (accepts-html? req3))
    (is (not (accepts-html? req4)))))

(deftest test-types-and-subtypes
  (let [res (-> base-resource
                (add-view "text/html" identity)
                (add-view "text/csv" identity)
                (add-view "application/json" identity))]
    (is (= (#'hottop.util/types-and-subtypes res)
           {"text" #{"html" "csv"}
            "application" #{"json"}}))))

(deftest test-at-accepts-pt?
  (let [pts1 [{:type "text" :subtype "html"}
              {:type "text" :subtype "csv"}
              {:type "text" :subtype "plain"}]
        pts2 [{:type "text" :subtype "html"}
              {:type "image" :subtype "png"}
              {:type "application" :subtype "json"}]
        at1 {:type "text" :subtype "*"}
        at2 {:type "text" :subtype "html" :q 0.0}
        at3 {:type "*" :subtype "*"}]
    (is (every? (partial #'hottop.util/at-accepts-pt? at1) pts1))
    (is (#'hottop.util/at-accepts-pt? at2 (first pts1)))
    (is (every? (partial #'hottop.util/at-accepts-pt? at3) pts2))))

(deftest test-optimal-media-type
  (testing "Test Acceptable Media Types"
    (let [request1 (-> (request :get "/test")
                       (header "Accept" "text/html"))
          request2 (-> (request :get "/test")
                       (header "Accept" "text/csv"))
          request3 (-> (request :get "/test")
                       (header "Accept" "text/*"))
          request4 (-> (request :get "/test")
                       (header "Accept" "*/*"))
          request5 (-> (request :get "/test")
                       (header "Accept" "image/jpeg;q=0, image/*"))
          resource1 (create-readonly-html-resource (constantly "hello!"))
          resource2 (-> base-resource
                        (assoc-in [:methods :get] (constantly "hello."))
                        (assoc-in [:content-types-provided "text/csv"] identity))
          resource3 (-> base-resource
                        (add-view "image/jpeg" identity)
                        (add-view "image/png" identity))
          omt1 (optimal-media-type request1 resource1)
          omt2 (optimal-media-type request2 resource1)
          omt3 (optimal-media-type request2 resource2)
          omt4 (optimal-media-type request3 resource2)
          omt5 (optimal-media-type request5 resource3)
          omt6 (optimal-media-type request4 resource2)]
      (is (= omt1 "text/html"))
      (is (nil? omt2))
      (is (= omt3 "text/csv"))
      (is (= omt4 "text/csv"))
      (is (= omt5 "image/png"))
      (is (= omt6 "text/csv")))))

(deftest test-response?
  (let [resp1 {:status 200 :body "Hello World!"}
        resp2 {:a 1 :b 2}
        resp3 'hello]
    (is (response? resp1))
    (is (not (response? resp2)))
    (is (not (response? resp3)))))
