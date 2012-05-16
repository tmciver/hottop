(ns hottop.core
  (:use hottop.proc))

(def ^:private processors [process-options
                           validate-method
                           validate-authorization])
