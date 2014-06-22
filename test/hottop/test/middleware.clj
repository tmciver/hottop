(ns hottop.test.middleware
  (:use clojure.test
        ring.mock.request
        hottop.middleware))

(deftest test-wrap-accept-from-extension
  (let [handler (wrap-accept-from-extension (fn [req] req))
        req1 (request :get "/test")
        req2 (request :get "/test.zip")
        req3 (request :get "/test.html")
        req4 (request :get "/test.csv")
        req5 (request :get "/test.abc")
        get-accept (fn [req] (get-in req [:headers "accept"]))]
    (is (= (handler req1) req1))
    (is (= (get-accept (handler req2)) "application/zip"))
    (is (= (get-accept (handler req3)) "text/html"))
    (is (= (get-accept (handler req4)) "text/csv"))
    (is (= (:status (handler req5)) 406))))