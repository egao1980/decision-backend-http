(in-package #:decision-backend-http/tests)

(deftest systemone-demo-runs
  (load (asdf:system-relative-pathname "decision-backend-http" "examples/systemone.lisp"))
  (let ((result (decision-backend-http/demo:run (make-broadcast-stream))))
    (ok (decision-protocol:decision-result-p result))
    (ok (equal "kev-4b" (decision-protocol:decision-result-model result)))))
