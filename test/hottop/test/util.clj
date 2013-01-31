(ns hottop.test.util
  (:use clojure.test
        ring.mock.request
        hottop.util
        hottop.resource))

(deftest test-parse-accept-header
  (testing "Test Accept headers"
    (is (= (parse-accept-header "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")
           [{:q 0.3, :type "text/*"}
            {:q 0.7, :type "text/html"}
            {:q 1.0, :level "1", :type "text/html"}
            {:q 0.4, :level "2", :type "text/html"}
            {:q 0.5, :type "*/*"}])))
  (testing "Test Accept-Encoding headers"
    (is (= (parse-accept-header "gzip;q=1.0, identity; q=0.5, *;q=0")
           [{:q 1.0, :type "gzip"}
            {:q 0.5, :type "identity"}
            {:q 0.0, :type "*"}]))))

(deftest test-allow-header-str
  (let [resource (-> base-resource
                     (assoc-in [:methods :get] (constantly "Hello!"))
                     (assoc-in [:methods :post] (constantly "Hello!"))
                     (assoc-in [:methods :put] (constantly "Hello!")))]
    (is (= (allow-header-str resource)
           "PUT, POST, GET"))))

(deftest test-optimal-media-type
  (testing "Test Acceptable Media Types"
    (let [request1 (-> (request :get "/test")
                       (header "Accept" "text/html"))
          request2 (-> (request :get "/test")
                       (header "Accept" "text/csv"))
          resource1 (create-readonly-html-resource (constantly "hello!"))
          resource2 (-> base-resource
                        (assoc-in [:methods :get] (constantly "hello."))
                        (assoc-in [:content-types-provided "text/csv"] identity))
          omt1 (optimal-media-type request1 resource1)
          omt2 (optimal-media-type request2 resource1)
          omt3 (optimal-media-type request2 resource2)]
      (is (= omt1 "text/html"))
      (is (nil? omt2))
      (is (= omt3 "text/csv")))))
