(defpackage #:decision-backend-http
  (:use #:cl)
  (:local-nicknames (#:dec #:decision-protocol)
                    (#:http.p #:http-protocol)
                    (#:json #:json-protocol)
                    (#:sec #:secrets-protocol))
  (:export #:+default-systemone-base-url+
           #:+default-systemone-model+
           #:http-decision-backend
           #:http-decision-backend-p
           #:make-http-decision-backend
           #:make-jev-decision-backend
           #:use-http-decision-backend
           #:+default-jev-base-url+
           #:http-decision-base-url
           #:http-decision-api-key
           #:http-decision-default-model
           #:http-decision-secret-ref
           #:http-decision-secret-store
           #:http-decision-request-fn
           #:http-decision-timeout
           #:encode-systemone-request
           #:decode-systemone-response
           #:decode-systemone-answers))

(in-package #:decision-backend-http)
