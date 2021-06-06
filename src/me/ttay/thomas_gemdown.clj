(ns me.ttay.thomas-gemdown
  (:require
   [me.ttay.thomas-gemdown.emit :as emit]
   [me.ttay.thomas-gemdown.watcher :as watcher])
  (:gen-class))

;; (defn run-file [filename]
;;   (println (emit/file->gemtext (slurp filename))))

(defn -main [& args]
  ;; (run-file (first args))
  (watcher/make-watcher (into [] args)))

(comment
  (spit "resources/test2.gmi" (emit/file->gemtext "resources/test2.md")))
