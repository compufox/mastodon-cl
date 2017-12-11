(in-package :mastodon.api)

(defun get-reports ()
  (decode-json-from-string
   (masto--perform-request '(:get "reports"))))

(defun report-user (account-id status-ids comment)
  (masto--perform-request `(:post "reports"
				 :content (("account_id" . ,account-id)
					   ("status_ids" . ,status-ids)
					   ("comment" . ,comment)))))
