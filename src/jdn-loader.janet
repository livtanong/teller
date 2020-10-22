(import jdn)

(defn load-jdn2
  [path args]
  @{'value @{:value (jdn/decode (slurp path))}})

(defn load-jdn
  [path args]
  (let [jdn-string (slurp path)
        jdn-value (jdn/decode jdn-string)
        # We want to quote the jdn data structure so that it doesn't get evaluated
        # We do, however, want the `def` evaluated.
        wrapped-value (quasiquote (def value '(unquote jdn-value)))
        serialized-value (string/format "%j" wrapped-value)
        temp-file (file/write (file/temp) serialized-value)]
    # Necessary to seek back to 0 otherwise, dofile will try to read from the very end, and get nothing.
    (file/seek temp-file :set 0)
    # dofile automatically closes the file
    (dofile temp-file (splice args))
    ))

(defn load-jdns
  "given a path to a directory, find all jdn files and create bindings"
  [path args]
  (let [{:jdn-loader/binding-type binding-type} (table (splice args))]
    (case binding-type
      :map (let [dir-filenames (os/dir path)
                 jdn-files (->> dir-filenames
                             (filter (partial string/has-suffix? ".jdn"))
                             (map (fn [filename]
                                    [filename
                                     (jdn/decode
                                       (slurp
                                         (string path "/" filename)))])))
                 jdn-map (table (splice (array/concat @[] (splice jdn-files))))]
             @{'jdns @{:value jdn-map}})
      :env {} #(print "bind individual files to env itself. Namespaced to the directory.")
      {})))

(defn check-jdns-dir
  [path]
  (if (= path "statement-formats")
    path))

(module/add-paths ".jdn" :jdn)
(array/push module/paths [check-jdns-dir :jdns])
(put module/loaders :jdn load-jdn2)
(put module/loaders :jdns load-jdns)

# test
# (import statement-formats :fresh true :jdn-loader/binding-type :map)
# statement-formats/jdns ## should give the map
