(def format
  '{:date :mm-dd-yy
   :description :simple-phrase
   :amount :financial-figure
   :simple-entry (sequence
                   (capture :date :date1)
                   :s+
                   (capture :date :date2)
                   :s+
                   (capture :description :description)
                   :s+
                   (capture :amount :amount))
   :multiline-entry  (sequence
                       :simple-entry
                       :s*
                       (if-not :simple-entry (capture :description)))
   :entry (thru (group (choice :multiline-entry :simple-entry)))
   :main (thru (some :entry))
   })
