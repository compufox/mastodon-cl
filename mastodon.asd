;;;; mastodon.asd

(asdf:defsystem #:mastodon
  :description "Describe mastodon here"
  :author "zactepps@gmail.com"
  :license "GPLv3"
  :serial t
  :components ((:file "package")
	       (:file "statuses")
	       (:file "timelines")
	       (:file "notifications")
	       (:file "reports")
	       (:file "search")
               (:file "mastodon")))

