(defsystem "decision-backend-http"
  :version "0.1.0"
  :description "HTTP System One backend for decision-protocol (Kev sidecar / hosted Jev)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("decision-protocol" "http-protocol" "json-protocol" "json-backend-jzon"
               "secrets-protocol/store" "babel")
  :properties (:cl-repo
               (:ci (:with ("http-backend-dexador"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "decision-backend-http/tests"))))

(defsystem "decision-backend-http/tests"
  :depends-on ("decision-backend-http" "secrets-protocol/store" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
