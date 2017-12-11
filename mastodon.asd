;;;; mastodon.asd

(asdf:defsystem #:mastodon
  :description "Describe mastodon here"
  :version "0.0.1"
  :author "zactepps@gmail.com"
  :license "GPLv3"
  :serial t
  :depends-on (:dexador
	       :cl-json)
  :components ((:file "mastodon")
	       (:module "api"
		:components
		((:file "api")
		 (:file "util")
		 (:file "apps")
		 (:file "follows")
		 (:file "errors")
		 (:file "accounts")
		 (:file "statuses")
		 (:file "timelines")
		 (:file "notifications")
		 (:file "reports")
		 (:file "search")))))

