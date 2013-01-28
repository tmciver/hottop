(ns hottop.response)

(def code {200 {:status 200},
           401 {:status 401, :body "Unauthorized"},
           404 {:status 404, :body "Not Found"},
           405 {:status 405, :body "Method Not Allowed"},
           406 {:status 406, :body "Not Acceptable"},
           500 {:status 500, :body "Internal Server Error"}})