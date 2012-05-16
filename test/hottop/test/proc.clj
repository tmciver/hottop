(ns hottop.test.proc
  (:use [hottop.proc]
        [hottop.resource]
        [clojure.test]))

(deftest test-http-processors

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