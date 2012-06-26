(ns hottop.samples.contacts-app.resources
  (:require [hottop.resource :as res]
            [hiccup.core :as hiccup])
  (:refer-clojure :exclude [get]))

(def ^:private db (ref {1 {:fname "Dorothy"
                           :lname "Gale"
                           :phone "867-5309"
                           :desc "She's not in Kansas anymore."}
                        2 {:fname "Scarecrow"
                           :lname ""
                           :phone "555-1234"
                           :desc "If he only had a brain."}
                        3 {:fname "Tinman"
                           :lname ""
                           :phone "555-5678"
                           :desc "If he only had a heart."}}))

;; model

(defn- get
  "Returns a seq of all the contacts in the database."
  [request]
  (vals @db))

;; view

(defn- html-template
  "HTML template to be used for all HTML pages."
  [subtitle body]
  (hiccup/html
   [:head
    [:title (str "Hottop Sample App - " subtitle)]]
   [:body
    body]))

(defn- contact-to-table-row
  "Formats the given contact as an HTML table row."
  [contact]
  (hiccup/html
   [:tr
    [:td (:lname contact)]
    [:td (:fname contact)]
    [:td (:phone contact)]
    [:td (:desc contact)]]))

(defn- contacts-to-table
  "Formats the given seq of contacts as an HTML table."
  [contacts]
  (hiccup/html
   [:table {:border "1"}
    [:tr
     [:th "Last Name"] [:th "First Name"] [:th "Phone Number"] [:th "Description"]]
    (map contact-to-table-row contacts)]))

(defn- contacts-to-html
  "Formats the given seq of contacts as a page of HTML."
  [contacts]
  (html-template "Contacts" (contacts-to-table contacts)))

(def contacts (res/create-readonly-html-resource get contacts-to-html))
