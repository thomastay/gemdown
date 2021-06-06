(ns me.ttay.thomas-gemdown.watcher
  (:require [hawk.core :as hawk]
            [me.ttay.thomas-gemdown.emit :as emit]))

;;; Watches a directory for changes

(defn markdown-file? [_ctx {:keys [file]}]
  (and (.isFile file)
       (.endsWith (.getName file) ".md")))

(def chars-in-md-extension 3)

(defn handle-change [ctx {:keys [kind file]}]
  (when (= :modify kind)
    (let [filename (.getAbsolutePath file)
          bare-filename (subs filename 0 (- (count filename) chars-in-md-extension))
          gem-filename (str bare-filename ".gmi")]
      (spit gem-filename (emit/file->gemtext (slurp filename)))))
  ctx)

(defn make-watcher
  "expects a vector of paths to directories / files to watch."
  [dirs-to-watch]
  (hawk/watch! [{:paths dirs-to-watch
                 :filter markdown-file?
                 :handler #'handle-change}])) ; TODO no vars in prod

(comment
  (def w (hawk/watch! [{:paths ["resources/"]
                        :filter markdown-file?
                        :handler #'handle-change}]))
  (hawk/stop! w))
