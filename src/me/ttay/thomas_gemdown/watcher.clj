(ns me.ttay.thomas-gemdown.watcher
  (:require [hawk.core :as hawk]
            [me.ttay.thomas-gemdown.emit :as emit])
  (:import (java.nio.file Path Files)
           (java.util.stream Stream)))

(set! *warn-on-reflection* true)
;;; Watches a directory for changes

(defn markdown-file?
  ([^java.nio.file.Path path]
   (.endsWith (.toString path) ".md"))
  ([_ctx {:keys [file]}]
   (let [file ^java.io.File file]
     (and (.isFile file)
          (.endsWith (.getName file) ".md")))))

(def chars-in-md-extension (count ".md"))

(defn stream->seq [^Stream my-stream]
  (-> my-stream .iterator iterator-seq))

(defn path-of [& paths]
  {:pre [(<= 2 (count paths))]}
  (Path/of (first paths) (into-array String (rest paths))))

(defn parse-and-emit
  "Given a Java.io.File object that points to a markdown file, and a out directory as a string,
   parse the file at the location, and spit it out to the given output directory, with the .gmi extension.
   If out-dir is nil, uses the same directory as the File."
  [^java.io.File file
   out-dir]
  (let [filename (.getAbsolutePath file)
        bare-filename #(subs % 0 (- (count %) chars-in-md-extension))
        to-gmi-filename #(str (bare-filename %) ".gmi")
        relative-filename (.getName file)
        gem-filename (if out-dir
                       (to-gmi-filename (.toString (path-of "." out-dir relative-filename)))
                       (to-gmi-filename filename))]
    (spit gem-filename (emit/file->gemtext (slurp filename)))))

(defn parse-and-emit-dir [directory out-dir]
  (->> (path-of "." directory)
       Files/list
       stream->seq
       (filter markdown-file?)
       (map (fn [^Path path] (.toFile path)))
       (run! #(parse-and-emit % out-dir))))

(defn handle-change [out-dir]
  (fn [ctx {:keys [kind file]}]
    (when (#{:modify :create} kind)
      (parse-and-emit file out-dir))
    ctx))

(defn make-watcher
  "expects a vector of paths to directories / files to watch."
  [dirs-to-watch out-dir]
  (hawk/watch! [{:paths dirs-to-watch
                 :filter markdown-file?
                 :handler (handle-change out-dir)}])) ; TODO no vars in prod

(comment
  (def w (hawk/watch! [{:paths ["resources/"]
                        :filter markdown-file?
                        :handler (handle-change "gemtext")}]))
  (hawk/stop! w)
  (add-tap println)
  (parse-and-emit-dir "resources" nil)
  (filter #(.endsWith (.toString %) ".md") (stream->seq (Files/list (Path/of "." (into-array String ["resources"]))))))
