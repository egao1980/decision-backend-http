;;;; System One client demo. Default: fixture request-fn (no sidecar).
;;;; Live Kev: DECISION_LIVE=1 and a sidecar on DECISION_BASE_URL (default 127.0.0.1:8009).
;;;;   sbcl --load examples/systemone.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :decision-backend-http)
    (require :asdf)
    (asdf:load-system "decision-backend-http")))

(defpackage #:decision-backend-http/demo
  (:use #:cl)
  (:local-nicknames (#:dec #:decision-protocol)
                    (#:http.d #:decision-backend-http)
                    (#:json #:json-protocol))
  (:export #:run #:live-p))

(in-package #:decision-backend-http/demo)

(defun live-p ()
  (let ((v (uiop:getenv "DECISION_LIVE")))
    (and v (plusp (length v))
         (not (member (string-downcase v) '("0" "false" "no") :test #'string=)))))

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (null v)
            do (setf (gethash k h) v))
    h))

(defun %fixture (method url &key headers content)
  (declare (ignore method headers))
  (let ((body (json:decode content)))
    (assert (equal "kev-4b" (gethash "model" body)))
    (assert (hash-table-p (gethash "questions" body)))
    (values 200
            (json:encode
             (%ht "model" "kev-4b"
                  "answers" (%ht "risk" (%ht "type" "choice"
                                             "choice" "allow"
                                             "confidence" 0.6
                                             "probabilities" (%ht "allow" 0.8
                                                                  "deny" 0.2)))
                  "usage" (%ht "input_tokens" 12 "output_tokens" 3))))))

(defun %request ()
  (dec:make-decision-request
   :state "tenant=acme ticket=chargeback"
   :questions (list (dec:make-choice-question
                     :id :risk
                     :instructions "Allow this refund?"
                     :criteria '((:allow . "yes") (:deny . "no"))))
   :model :kev-latest))

(defun run (&optional (stream *standard-output*))
  "POST /v1/systemone via fixture or a live sidecar. Returns DECISION-RESULT."
  (let* ((backend (if (live-p)
                      (http.d:make-http-decision-backend)
                      (http.d:make-http-decision-backend :request-fn #'%fixture)))
         (result (dec:decide backend (%request)))
         (answer (first (dec:decision-result-answers result)))
         (mass (dec:distribution-mass (dec:decision-answer-distribution answer)))
         (conc (dec:concentration (dec:decision-answer-distribution answer))))
    (format stream "~&; ~a model=~s mass=~s concentration=~s (not P(correct))~%"
            (if (live-p) "live" "fixture")
            (dec:decision-result-model result) mass conc)
    (assert (equal "kev-4b" (dec:decision-result-model result)))
    (assert (/= conc (cdr (assoc :allow mass :test #'eql))))
    result))

#+sbcl
(when (and *load-truename*
           (equal (pathname-name *load-truename*) "systemone")
           (find "examples/systemone.lisp" sb-ext:*posix-argv* :test #'search))
  (run)
  (uiop:quit 0))
