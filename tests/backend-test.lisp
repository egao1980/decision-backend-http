(in-package #:decision-backend-http/tests)

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (null v)
            do (setf (gethash k h) v))
    h))

(defun %choice-q ()
  (decision-protocol:make-choice-question
   :id :risk
   :instructions "Allow this effect?"
   :criteria '((:allow . "proceed") (:deny . "stop"))))

(defun %binary-q ()
  (decision-protocol:make-binary-question
   :id :ok
   :instructions "yes?"))

(defun %ordinal-q ()
  (decision-protocol:make-ordinal-question
   :id :sev
   :instructions "How bad?"
   :criteria '("low" "mid" "high")))

(defun %req (&key (questions (list (%choice-q))) (state "tenant=acme") model)
  (decision-protocol:make-decision-request
   :state state
   :questions questions
   :model model))

(defun %ok-body (&key (model "kev-4b") (p-allow 0.8d0) (p-deny 0.2d0))
  (stack-json:encode
   (%ht "model" model
        "answers" (%ht "risk" (%ht "type" "choice"
                                   "choice" "allow"
                                   "confidence" 0.6
                                   "probabilities" (%ht "allow" p-allow
                                                        "deny" p-deny)))
        "usage" (%ht "input_tokens" 12 "output_tokens" 3))))

(defun %capture-fn (box &optional body)
  (lambda (method url &key headers content)
    (setf (car box) (list method url headers content))
    (values 200 (or body (%ok-body)))))

(deftest encode-questions-are-a-map
  (let* ((req (%req :questions (list (%choice-q) (%binary-q))))
         (body (decision-backend-http:encode-systemone-request req :model "kev-4b"))
         (qs (gethash "questions" body)))
    (ok (hash-table-p qs))
    (ok (hash-table-p (gethash "risk" qs)))
    (ok (equal "choice" (gethash "type" (gethash "risk" qs))))
    (ok (equal "noul" (gethash "type" (gethash "ok" qs))))
    (ok (null (search "risk" (gethash "instructions" (gethash "risk" qs)))))))

(deftest decode-keeps-full-mass-drops-confidence
  (let* ((req (%req))
         (payload (stack-json:decode (%ok-body)))
         (result (decision-backend-http:decode-systemone-response
                  req payload :fallback-model "kev-4b"))
         (answer (first (decision-protocol:decision-result-answers result)))
         (mass (decision-protocol:distribution-mass
                (decision-protocol:decision-answer-distribution answer))))
    (ok (equal "kev-4b" (decision-protocol:decision-result-model result)))
    (ok (= 0.8d0 (cdr (assoc :allow mass))))
    (ok (= 0.2d0 (cdr (assoc :deny mass))))
    (ok (eq :allow (decision-protocol:distribution-winner
                    (decision-protocol:decision-answer-distribution answer))))
    (ng (find :confidence mass :key #'car))))

(deftest decode-noul-and-score
  (let* ((req (%req :questions (list (%binary-q) (%ordinal-q))))
         (payload (%ht "model" "kev-4b"
                       "answers" (%ht "ok" (%ht "type" "noul" "noul" 0.75)
                                      "sev" (%ht "type" "score"
                                                 "score" 1.2
                                                 "confidence" 0.9
                                                 "probabilities" (%ht "0" 0.2
                                                                      "1" 0.4
                                                                      "2" 0.4)))))
         (result (decision-backend-http:decode-systemone-response req payload))
         (answers (decision-protocol:decision-result-answers result))
         (noul-mass (decision-protocol:distribution-mass
                     (decision-protocol:decision-answer-distribution (first answers))))
         (score-mass (decision-protocol:distribution-mass
                      (decision-protocol:decision-answer-distribution (second answers)))))
    (ok (= 0.75d0 (cdr (assoc :true noul-mass))))
    (ok (= 0.25d0 (cdr (assoc :false noul-mass))))
    (ok (= 0.2 (cdr (assoc "low" score-mass :test #'equal))))
    (ok (= 1.2 (decision-protocol:decision-answer-score (second answers))))))

(deftest decide-pins-server-model
  (let* ((box (list nil))
         (backend (decision-backend-http:make-http-decision-backend
                   :request-fn (%capture-fn box (%ok-body :model "kev-4b"))
                   :default-model "kev-latest"))
         (result (decision-protocol:decide backend (%req :model :kev-latest)))
         (sent (stack-json:decode (fourth (car box)))))
    (ok (equal "kev-4b" (decision-protocol:decision-result-model result)))
    (ok (equal "kev-4b" (gethash "model" sent)))
    (ok (search "/v1/systemone" (second (car box))))
    (ok (= 12 (decision-protocol:decision-usage-input-tokens
               (decision-protocol:decision-result-usage result))))))

(deftest decide-schema-error-on-422
  (let ((backend (decision-backend-http:make-http-decision-backend
                  :request-fn (lambda (&rest args)
                                (declare (ignore args))
                                (values 422 (stack-json:encode
                                             (%ht "error" "empty choice")))))))
    (ok (signals (decision-protocol:decide backend (%req))
                 'decision-protocol:decision-schema-error))))

(deftest decide-unavailable-on-503
  (let ((backend (decision-backend-http:make-http-decision-backend
                  :request-fn (lambda (&rest args)
                                (declare (ignore args))
                                (values 503 (stack-json:encode
                                             (%ht "error" "down")))))))
    (ok (signals (decision-protocol:decide backend (%req))
                 'decision-protocol:decision-unavailable))))

(deftest bearer-from-secret-ref-not-in-body
  (let* ((box (list nil))
         (store (secrets-protocol:make-in-memory-secret-store
                 :secrets '(("typesafe" "api-key" "tok-secret"))))
         (ref (secrets-protocol:make-secret-ref :name "typesafe" :key "api-key"))
         (backend (decision-backend-http:make-http-decision-backend
                   :request-fn (%capture-fn box)
                   :secret-ref ref
                   :secret-store store)))
    (decision-protocol:decide backend (%req))
    (let* ((headers (third (car box)))
           (auth (cdr (assoc "authorization" headers :test #'equal)))
           (content (fourth (car box))))
      (ok (equal "Bearer tok-secret" auth))
      (ng (search "tok-secret" content)))))

(deftest permute-and-separate-paths
  (let* ((box (list nil))
         (perm-body (stack-json:encode
                     (%ht "model" "kev-4b"
                          "answers" (vector (%ht "type" "choice"
                                                 "choice" "allow"
                                                 "probabilities" (%ht "allow" 0.7
                                                                      "deny" 0.3))
                                            (%ht "type" "choice"
                                                 "choice" "deny"
                                                 "probabilities" (%ht "allow" 0.4
                                                                      "deny" 0.6))))))
         (backend (decision-backend-http:make-http-decision-backend
                   :request-fn (%capture-fn box perm-body)))
         (answers (decision-protocol:permute backend (%req) :risk :n-perm 2 :seed 1)))
    (ok (= 2 (length answers)))
    (ok (search "/v1/systemone/permute" (second (car box))))
    (let ((sent (stack-json:decode (fourth (car box)))))
      (ok (equal "risk" (gethash "question_id" sent)))
      (ok (= 2 (gethash "n_perm" sent)))))
  (let* ((box (list nil))
         (backend (decision-backend-http:make-http-decision-backend
                   :request-fn (%capture-fn box)))
         (result (decision-protocol:decide-separate backend (%req))))
    (ok (decision-protocol:decision-result-p result))
    (ok (search "/v1/systemone/separate" (second (car box))))))

(deftest concentration-is-not-sent-or-required
  (let* ((req (%req))
         (payload (stack-json:decode (%ok-body)))
         (answer (first (decision-backend-http:decode-systemone-answers req payload)))
         (dist (decision-protocol:decision-answer-distribution answer))
         (p-allow (cdr (assoc :allow (decision-protocol:distribution-mass dist)))))
    (ok (numberp (decision-protocol:concentration dist)))
    (ng (= (decision-protocol:concentration dist) p-allow))))
