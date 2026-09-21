(in-package #:decision-backend-http/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (asdf:system-relative-pathname "decision-backend-http" "examples/systemone.lisp")))

(deftest systemone-demo-runs
  (let ((result (decision-backend-http/demo:run (make-broadcast-stream))))
    (ok (decision-protocol:decision-result-p result))
    (ok (equal "kev-4b" (decision-protocol:decision-result-model result)))))
