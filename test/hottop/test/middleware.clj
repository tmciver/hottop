(ns hottop.test.middleware
  (:use clojure.test
        ring.mock.request
        hottop.middleware))

(deftest test-wrap-accept-from-extension
  (let [handler (-> (fn [req]
                      ;; return the accept headers
                      (get-in req [:headers "accept"]))
                    wrap-accept-from-extension)
        req1 (request :get "/test")
        req2 (request :get "/test.zip")
        req3 (request :get "/test.html")
        req4 (request :get "/test.csv")
        req5 (request :get "/test.abc")]
    (is (handler req1) [])
    (is (handler req2) #{"application/zip"})
    (is (handler req3) #{"text/html"})
    (is (handler req4) #{"text/csv"})
    (is (handler req5) {:status 406 :body "Unsupported file extension"})))