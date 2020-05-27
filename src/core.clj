(ns core
 (:require
  [liq.core :as liquid]
  [timesheet]))

(defn -main
 []
 (liquid/-main)
 (timesheet/init)
 )
 
 
