(ns hottop.test.middleware
  (:use clojure.test
        ring.mock.request
        hottop.middleware))

(deftest test-wrap-accept-from-extension
  (let [handler (-> (fn [req]
                      ;; just return the request as response
                      req)
                    wrap-accept-from-extension)
        req1 (request :get "/test")
        req2 (request :get "/test.zip")
        req3 (request :get "/test.html")
        req4 (request :get "/test.csv")
        req5 (request :get "/test.abc")
        get-accept (fn [req] (get-in req [:headers "accept"]))]
    (is (let [resp (handler req1)]
          (and (= resp req1)
               (= (:ext resp) nil))))
    (is (let [resp (handler req2)]
          (and (= (get-accept resp) "application/zip")
               (= (:ext resp) "zip"))))
    (is (let [resp (handler req3)]
          (and (= (get-accept resp) "text/html")
               (= (:ext resp) "html"))))
    (is (let [resp (handler req4)]
          (and (= (get-accept resp) "text/csv")
               (= (:ext resp) "csv"))))
    (is (= (handler req5) {:status 406 :body "Unsupported file extension"}))))