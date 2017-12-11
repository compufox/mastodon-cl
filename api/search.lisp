(in-package :mastodon.api)

(defun mastodon-search (query &key (resolve-non-local nil))
  (decode-json-from-string
   (masto--perform-request `(:get "search?q="
				 ,query
				 ,(if resolve-non-local "&resolve=true")))))
