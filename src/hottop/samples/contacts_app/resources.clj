(ns hottop.samples.contacts-app.resources
  (:require [hottop.resource :as res]
            [hiccup.core :as hiccup]
            [hiccup.form :as form])
  (:refer-clojure :exclude [get]))

;; our database
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

;; contacts model

(defn- get
  "Returns a seq of all the contacts in the database."
  [request]
  (vals @db))

;; contacts view

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

;; contacts resource

(def contacts (res/create-readonly-html-resource get contacts-to-html))

;; create-contact view

(defn- create-contact-fn
  [_]
  (html-template "Create Contact"
                 (hiccup/html [:h2 "Create a new contact"]
                              (form/form-to [:post "/contacts"]
                                            [:lable "First name: "]
                                            (form/text-field "fname")
                                            [:br]
                                            [:lable "Last name: "]
                                            (form/text-field "lname")
                                            [:br]
                                            [:lable "Phone number: "]
                                            (form/text-field "number")))))

;; create-contact resource

(def create-contact (res/create-readonly-html-resource create-contact-fn identity))
