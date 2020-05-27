(ns timesheet
 (:require
  [clojure.edn :as edn]
  [clojure.string :as s]
  [liq.editor :as e]
  [liq.util :as u]
  [tick.alpha.api :as t]
  [time-literals.read-write :as time-literals]))

(defn read-string
  [string]
  (edn/read-string {:readers time-literals/tags} string))

(def state
 (atom {::current-task nil
        ::date (t/date)
        ::billing-increment (t/new-duration 15 :minutes) 
        ::tasks (if-let [tasks (read-string
                                (u/read-file 
                                   (u/resolve-home 
                                    (str "~/.liq.d/timesheets/" 
                                     (t/year) "-" (t/month) ".edn"))))]
                  tasks 
                  {})}))

(defn start-task
 ([state task now]
  (e/message (str "task-started: " task " at " now))
  (if-let [entry (get-in state [::tasks task])]
   (assoc state ::current-task (merge {:task task :start now}
                                (dissoc entry :times)))
   (assoc state ::current-task {:task task :start now}))))

(comment (reset! state (start-task @state :liquid-dev (t/now)))
  (reset! state (start-task @state :timesheet-dev (t/now))))

(defn end-task
 [{:keys [::current-task] :as state} now]
 (e/message (str "task-ended: " (:task current-task) " at " now))
 (let [{:keys [task start description notes]} current-task
       entry (get-in state [::tasks task])]
  (if-not current-task
   state
   (dissoc 
    (if entry
     (update-in state [::tasks task]
      (fn [{:keys [times] :as params}]
        (-> params
         (merge (dissoc current-task :start :notes :task))
         (update :times #(conj %
                         (merge (select-keys current-task 
                                 [:start :notes]) 
                                {:end now}))))))
     (assoc-in state [::tasks task]
      (assoc (dissoc current-task :start :notes :task)
       :times [(merge (select-keys current-task [:start :notes]) 
                      {:end now})])))
    ::current-task))))

(comment (reset! state (end-task @state (t/now))))

(defn describe-task
 [& description]
 (e/message "description-added!")
 (swap! state
  (fn [state]
   (assoc-in state [::current-task :description] 
    (s/join " " description)))))

(defn note-task 
 [& notes]
 (e/message "note-added!")
 (swap! state
  (fn [{:keys [::current-task] :as state}]
   (assoc-in state [::current-task :notes]
    (into [] 
     (conj (:notes current-task) 
           (s/join " " notes)))))))

(defn switch-task
 ([task]
  (swap! state
   (fn [state]
    (let [now (t/now)]
     (-> state
      (end-task now)
      (start-task task now))))))
 ([task & description]
  (switch-task task)
  (apply describe-task description)))
     
(defn stop-work
 []
 (swap! state (fn [state] (end-task state (t/now)))))

(defn start-work
 [task]
 ;; TODO add prompt to check whether previously running task is valid
 ;; TODO add a TODO list sort-of like org-mode
 (swap! state (fn [state] (start-task state task (t/now))))) 

(defn current-task
 []
 (e/message (str "current-task: " (get-in @state [::current-task :task]))))

(defn save
 [state]
 (u/write-file 
   (u/resolve-home 
    (str "~/.liq.d/timesheets/" (t/year) "-" (t/month) ".edn"))
   (pr-str (::tasks state))))

(defn last-of-the-month
 [instant]
 (let [m (t/month instant)]
  (loop [ret (t/truncate instant :days)]
   (if (= m (t/month ret))
    (recur (t/+ ret (t/new-duration 1 :days)))
    ret))))

(add-watch state ::watcher
 (fn [key atom old-state new-state]
  (when (and (:start (::curent-task old-state))
             (not= (::tasks old-state) (::tasks new-state))
             (not= (t/month (get-in old-state [::current-task :start])) (t/month)))
   (save (end-task @state (last-of-the-month (get-in old-state [::current-task :start]))))
   (e/message (str "Starting new timesheet for " (t/month)))
   (reset! state (assoc new-state ::tasks 
                  (-> old-state
                   (assoc-in [::current-task :start] 
                    (last-of-the-month (get-in old-state [::current-task :start])))
                   (end-task (t/now))
                   ::tasks))))
  (when (and (:start (::current-task old-state))
             (= (t/month (get-in old-state [::current-task :start])) (t/month))
             (not= (::tasks old-state) (::tasks new-state)))
   (save new-state))
  (when (not= (t/today) (::date new-state))
   (swap! state (fn [state] (assoc state ::date (t/today)))))))

(defn init 
  []
  (swap! e/state 
   (fn [state] 
    (update state ::e/commands
     (partial merge {:ct current-task
                     :current-task current-task
                     :dt describe-task
                     :describe-task describe-task
                     :nt note-task
                     :note-task note-task
                     :st switch-task
                     :switch-task switch-task
                     :work start-work
                     :chill stop-work})))))
(comment
 (start-work :liquid-dev)
 (switch-task :timesheet-dev)
 (describe-task "just a buncha hooey")
 (current-task)
 (note-task "malarkey")
 (:notes (::current-task @state))
 (switch-task :liquid-dev)
 (stop-work))

(comment (init))