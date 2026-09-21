(in-package #:decision-backend-http)

;;; One HTTP client, two endpoints: local Kev sidecar and hosted Jev.
;;; POST {base}/v1/systemone. Optional /permute and /separate.
;;; Bearer material is resolved at the call boundary; never journaled.
;;; Wire confidence is discarded — policies consume full mass.

(defparameter +default-systemone-base-url+ "http://127.0.0.1:8009")
(defparameter +default-systemone-model+ "kev-4b")
(defparameter +default-jev-base-url+ "https://api.typesafe.ai")

(defclass http-decision-backend (dec:decision-backend)
  ((base-url :initarg :base-url :accessor http-decision-base-url
             :initform +default-systemone-base-url+)
   (api-key :initarg :api-key :accessor http-decision-api-key :initform nil)
   (default-model :initarg :default-model :accessor http-decision-default-model
                  :initform +default-systemone-model+)
   (secret-ref :initarg :secret-ref :accessor http-decision-secret-ref
               :initform nil)
   (secret-store :initarg :secret-store :accessor http-decision-secret-store
                 :initform nil)
   (request-fn :initarg :request-fn :accessor http-decision-request-fn
               :initform nil)
   (timeout :initarg :timeout :accessor http-decision-timeout :initform 60)
   (capabilities :initarg :capabilities :accessor http-decision-capabilities
                 :initform '(:batch :permute :separate))))

(defun http-decision-backend-p (object)
  (typep object 'http-decision-backend))

(defun %env (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defun make-http-decision-backend (&key base-url api-key default-model
                                     secret-ref secret-store request-fn
                                     (timeout 60)
                                     (capabilities '(:batch :permute :separate)))
  (make-instance 'http-decision-backend
                 :base-url (or base-url
                               (%env "DECISION_BASE_URL")
                               (%env "KEV_BASE_URL")
                               +default-systemone-base-url+)
                 :api-key (or api-key
                              (%env "TYPESAFE_API_KEY")
                              (%env "KEV_API_KEY"))
                 :default-model (or default-model
                                    (%env "DECISION_MODEL")
                                    +default-systemone-model+)
                 :secret-ref secret-ref
                 :secret-store secret-store
                 :request-fn request-fn
                 :timeout timeout
                 :capabilities capabilities))

(defun use-http-decision-backend (&rest args &key &allow-other-keys)
  (setf dec:*decision-backend* (apply #'make-http-decision-backend args)))

(defun make-jev-decision-backend (&key base-url api-key (default-model "jev-latest")
                                    secret-ref secret-store request-fn
                                    (timeout 60)
                                    (capabilities '(:batch :permute :separate)))
  "Same HTTP client, hosted Jev defaults. Pin DEFAULT-MODEL; resolve-model
   journals the concrete name. Bearer from SECRET-REF, never inline in the body."
  (make-http-decision-backend
   :base-url (or base-url
                 (%env "JEV_BASE_URL")
                 +default-jev-base-url+)
   :api-key api-key
   :default-model default-model
   :secret-ref secret-ref
   :secret-store secret-store
   :request-fn request-fn
   :timeout timeout
   :capabilities capabilities))

(defun %model-key (model)
  (cond
    ((null model) "")
    ((stringp model) (string-downcase model))
    ((symbolp model) (string-downcase (symbol-name model)))
    (t (string-downcase (princ-to-string model)))))

(defmethod dec:resolve-model ((backend http-decision-backend) model)
  (let ((key (%model-key (or model (http-decision-default-model backend)))))
    (cond
      ((or (string= key "")
           (string= key "kev-latest")
           (string= key "kev"))
       "kev-4b")
      ((or (string= key "jev-latest")
           (string= key "jev"))
       "jev-latest")
      ((stringp model) model)
      ((and (null model) (http-decision-default-model backend))
       (http-decision-default-model backend))
      ((symbolp model) (string-downcase (symbol-name model)))
      (t key))))

(defmethod dec:backend-supports-p ((backend http-decision-backend) capability)
  (and (member capability (http-decision-capabilities backend)) t))

(defun %join (base path)
  (format nil "~a~a" (string-right-trim "/" (or base "")) path))

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit) (null v))
            do (setf (gethash k h) v))
    h))

(defun %wire-id (id)
  (cond
    ((stringp id) id)
    ((symbolp id) (string-downcase (symbol-name id)))
    (t (princ-to-string id))))

(defun %wire-option-key (key)
  (cond
    ((stringp key) key)
    ((symbolp key) (string-downcase (symbol-name key)))
    (t (princ-to-string key))))

(defun %jget (object key)
  (cond
    ((hash-table-p object)
     (or (gethash key object)
         (and (stringp key) (gethash (intern (string-upcase key) :keyword) object))))
    ((and (consp object) (keywordp (car object)))
     (getf object (if (keywordp key)
                      key
                      (intern (string-upcase key) :keyword))))
    (t nil)))

(defun %as-list (object)
  (cond
    ((null object) nil)
    ((vectorp object) (coerce object 'list))
    ((listp object) object)
    (t (list object))))

(defun %as-alist (object)
  (cond
    ((null object) nil)
    ((hash-table-p object)
     (let ((out nil))
       (maphash (lambda (k v) (push (cons k v) out)) object)
       (nreverse out)))
    ((and (consp object) (consp (car object)))
     object)
    (t nil)))

(defun %question-criteria-wire (question)
  (etypecase question
    (dec:binary-question
     (let ((pairs (loop for k in (dec:question-options question)
                        for desc = (let ((c (dec:question-criteria question)))
                                     (or (and (listp c) (cdr (assoc k c :test #'eql)))
                                         (and (listp c) (getf c k))))
                        when desc
                          collect (cons (%wire-option-key k) desc))))
       (if pairs
           (let ((h (make-hash-table :test 'equal)))
             (dolist (p pairs h)
               (setf (gethash (car p) h) (cdr p))))
           :omit)))
    (dec:choice-question
     (let ((h (make-hash-table :test 'equal)))
       (dolist (p (dec:question-criteria question) h)
         (setf (gethash (%wire-option-key (car p)) h)
               (if (stringp (cdr p)) (cdr p) (princ-to-string (cdr p)))))))
    (dec:ordinal-question
     (coerce (dec:question-criteria question) 'vector))))

(defun encode-systemone-question (question)
  "Map a DECISION-QUESTION to a System One question object (hash-table)."
  (let ((type (string-downcase (symbol-name (dec:question-wire-type question))))
        (instructions (dec:question-instructions question))
        (criteria (%question-criteria-wire question)))
    (%ht "type" type
         "instructions" (or instructions "")
         "criteria" criteria)))

(defun encode-systemone-request (request &key model extras)
  "Map DECISION-REQUEST to a System One body hash-table.
   Question ids are map keys — they are not placed in question text."
  (let* ((request (dec:coerce-decision-request request))
         (questions (make-hash-table :test 'equal)))
    (dolist (q (dec:decision-request-questions request))
      (setf (gethash (%wire-id (dec:question-id q)) questions)
            (encode-systemone-question q)))
    (let ((body (%ht "model" model
                     "state" (dec:decision-request-state request)
                     "questions" questions)))
      (when extras
        (loop for (k v) on extras by #'cddr
              do (setf (gethash (substitute #\_ #\- (%wire-option-key k)) body) v)))
      body)))

(defun %match-option (wire-key options)
  (or (find wire-key options :test #'equal)
      (find wire-key options
            :test (lambda (a b)
                    (equal (%wire-option-key a) (%wire-option-key b))))
      (cond
        ((stringp wire-key)
         (intern (string-upcase wire-key) :keyword))
        (t wire-key))))

(defun %noul-mass (question p)
  (let ((options (dec:question-options question))
        (p (float p 1d0)))
    (cond
      ((= (length options) 2)
       (list (cons (first options) p)
             (cons (second options) (- 1d0 p))))
      (t
       (list (cons :true p) (cons :false (- 1d0 p)))))))

(defun %choice-mass (question probabilities)
  (let ((options (dec:question-options question))
        (alist (%as-alist probabilities)))
    (if alist
        (mapcar (lambda (p)
                  (cons (%match-option (car p) options) (cdr p)))
                alist)
        (mapcar (lambda (k) (cons k 0)) options))))

(defun %score-mass (question probabilities)
  (let ((levels (dec:question-criteria question))
        (alist (%as-alist probabilities)))
    (loop for level in levels
          for i from 0
          for p = (or (cdr (assoc (princ-to-string i) alist :test #'equal))
                      (cdr (assoc i alist :test #'equal))
                      (cdr (assoc level alist :test #'equal))
                      (cdr (assoc (%wire-option-key level) alist :test #'equal))
                      0)
          collect (cons level p))))

(defun decode-systemone-answer (question payload)
  "PAYLOAD is one System One answer object. Confidence is ignored."
  (let* ((type (or (%jget payload "type")
                   (string-downcase (symbol-name (dec:question-wire-type question)))))
         (mass (cond
                 ((member type '("noul" "binary") :test #'equal)
                  (%noul-mass question (or (%jget payload "noul") 0)))
                 ((string= type "choice")
                  (%choice-mass question (%jget payload "probabilities")))
                 ((member type '("score" "ordinal") :test #'equal)
                  (%score-mass question (%jget payload "probabilities")))
                 (t
                  (error 'dec:decision-schema-error
                         :question question
                         :message (format nil "unknown answer type: ~s" type)))))
         (dist (dec:make-probability-distribution
                :mass (dec:normalize-mass mass)))
         (score (or (%jget payload "score")
                    (dec:decision-answer-score
                     (dec:make-decision-answer :question question
                                               :distribution dist)))))
    (dec:make-decision-answer
     :question question
     :distribution dist
     :score score)))

(defun decode-systemone-answers (request payload)
  "Map a System One answers object onto REQUEST questions, in request order."
  (let ((answers-obj (%jget payload "answers")))
    (mapcar (lambda (q)
              (let ((cell (or (%jget answers-obj (%wire-id (dec:question-id q)))
                              (%jget answers-obj (dec:question-id q)))))
                (unless cell
                  (error 'dec:decision-schema-error
                         :request request
                         :question q
                         :message (format nil "missing answer for question ~s"
                                          (dec:question-id q))))
                (decode-systemone-answer q cell)))
            (dec:decision-request-questions request))))

(defun decode-systemone-usage (payload)
  (let ((usage (%jget payload "usage")))
    (dec:make-decision-usage
     :input-tokens (or (%jget usage "input_tokens") 0)
     :output-tokens (or (%jget usage "output_tokens") 0))))

(defun decode-systemone-response (request payload &key fallback-model)
  (dec:make-decision-result
   :model (or (%jget payload "model") fallback-model)
   :answers (decode-systemone-answers request payload)
   :usage (decode-systemone-usage payload)))

(defun %permute-cell (question item)
  (cond
    ((hash-table-p item)
     (or (%jget item (%wire-id (dec:question-id question)))
         (%jget item "answer")
         item))
    (t item)))

(defun decode-permute-answers (question payload)
  "Accept answers as a map, an array of maps, or permutations[]."
  (let ((answers (%jget payload "answers"))
        (perms (%jget payload "permutations")))
    (cond
      ((or (vectorp answers)
           (and (consp answers) (not (hash-table-p answers))))
       (mapcar (lambda (item)
                 (decode-systemone-answer question (%permute-cell question item)))
               (%as-list answers)))
      ((or (vectorp perms) (consp perms))
       (mapcar (lambda (item)
                 (let ((cell (or (%jget item "answers") item)))
                   (decode-systemone-answer question (%permute-cell question cell))))
               (%as-list perms)))
      (answers
       (list (decode-systemone-answer question
                                      (%permute-cell question answers))))
      (t
       (error 'dec:decision-schema-error
              :question question
              :message "permute response has no answers")))))

(defun %bearer (backend)
  (or (http-decision-api-key backend)
      (let ((ref (http-decision-secret-ref backend))
            (store (http-decision-secret-store backend)))
        (when (and ref store)
          (sec:resolve-secret store ref)))))

(defun %headers (backend)
  (let ((h `(("content-type" . "application/json")
             ("accept" . "application/json"))))
    (let ((token (%bearer backend)))
      (when (and token (plusp (length token)))
        (push (cons "authorization" (format nil "Bearer ~a" token)) h)))
    h))

(defun %body-string (response)
  (let ((b (http.p:response-body response)))
    (cond
      ((stringp b) b)
      ((and (vectorp b) (not (stringp b)))
       (babel:octets-to-string b :encoding :utf-8))
      (t ""))))

(defun %http-request (backend method url &key headers content)
  (unless http.p:*http-backend*
    (error 'dec:decision-unavailable
           :message "*http-backend* is nil — bind an http-protocol backend"))
  (handler-case
      (let ((res (apply #'http:request method url
                        :headers headers
                        :timeout (http-decision-timeout backend)
                        (and content (list :content content)))))
        (values (http.p:response-status res)
                (%body-string res)))
    (error (c)
      (error 'dec:decision-unavailable
             :message (format nil "http request failed: ~a" c)))))

(defun %decode-json (body)
  (handler-case (json:decode body)
    (error (c)
      (error 'dec:decision-schema-error
             :message (format nil "invalid JSON: ~a" c)))))

(defun %request (backend path object)
  (let* ((fn (or (http-decision-request-fn backend)
                 (lambda (method url &key headers content)
                   (%http-request backend method url :headers headers :content content))))
         (url (%join (http-decision-base-url backend) path))
         (content (json:encode object))
         (headers (%headers backend)))
    (multiple-value-bind (status body)
        (funcall fn :post url :headers headers :content content)
      (values status body (%decode-json body)))))

(defun %raise-http (status body obj)
  (declare (ignore body))
  (let ((msg (or (%jget obj "error")
                 (%jget (%jget obj "error") "message")
                 (format nil "HTTP ~a" status))))
    (cond
      ((= status 422)
       (error 'dec:decision-schema-error :message (princ-to-string msg)))
      ((or (= status 408) (= status 504))
       (error 'dec:decision-timeout :limit nil :message (princ-to-string msg)))
      ((<= 400 status 499)
       (error 'dec:decision-error :message (princ-to-string msg)))
      (t
       (error 'dec:decision-unavailable :message (princ-to-string msg))))))

(defun %post-systemone (backend path request &key extras)
  (let* ((request (dec:coerce-decision-request request))
         (alias (or (dec:decision-request-model request)
                    (http-decision-default-model backend)))
         (resolved (dec:resolve-model backend alias))
         (object (encode-systemone-request request :model resolved :extras extras)))
    (multiple-value-bind (status body obj)
        (%request backend path object)
      (unless (<= 200 status 299)
        (%raise-http status body obj))
      (let ((result (decode-systemone-response request obj :fallback-model resolved)))
        ;; Server model is the journaled pin; alias never leaks through.
        (dec:make-decision-result
         :model (or (dec:decision-result-model result) resolved)
         :answers (dec:decision-result-answers result)
         :usage (dec:decision-result-usage result))))))

(defmethod dec:decide ((backend http-decision-backend) request)
  (%post-systemone backend "/v1/systemone" request))

(defmethod dec:permute ((backend http-decision-backend) request question-id
                        &key (n-perm 2) seed)
  (unless (dec:backend-supports-p backend :permute)
    (error 'dec:decision-unsupported
           :capability :permute
           :message "http backend is not configured for :permute"))
  (let* ((request (dec:coerce-decision-request request))
         (question (find question-id (dec:decision-request-questions request)
                         :key #'dec:question-id :test #'equal)))
    (unless question
      (error 'dec:decision-schema-error
             :request request
             :message (format nil "no question with id ~s" question-id)))
    (let* ((alias (or (dec:decision-request-model request)
                      (http-decision-default-model backend)))
           (resolved (dec:resolve-model backend alias))
           (object (encode-systemone-request
                    request :model resolved
                    :extras (list :question-id (%wire-id question-id)
                                  :n-perm n-perm
                                  :seed seed))))
      (multiple-value-bind (status body obj)
          (%request backend "/v1/systemone/permute" object)
        (unless (<= 200 status 299)
          (%raise-http status body obj))
        (decode-permute-answers question obj)))))

(defmethod dec:decide-separate ((backend http-decision-backend) request)
  (unless (dec:backend-supports-p backend :separate)
    (error 'dec:decision-unsupported
           :capability :separate
           :message "http backend is not configured for :separate"))
  (%post-systemone backend "/v1/systemone/separate" request))
