# decision-backend-http

HTTP System One backend for [`decision-protocol`](https://github.com/egao1980/decision-protocol). One client, two endpoints:

| Target | Base | Auth |
|--------|------|------|
| Local Kev sidecar | `http://127.0.0.1:8009` | none / optional bearer |
| Hosted Jev | `https://api.typesafe.ai` | `TYPESAFE_API_KEY` or a `secret-ref` |

`POST {base}/v1/systemone`. Optional ` /permute` and `/separate`. Wire `confidence` is discarded; answers keep the full mass.

```lisp
(asdf:load-system "decision-backend-http")

(decision-backend-http:use-http-decision-backend
 :base-url "http://127.0.0.1:8009"
 :default-model "kev-4b")

(decision-protocol:decide
 decision-protocol:*decision-backend*
 (decision-protocol:make-decision-request
  :state "tenant=acme"
  :questions (list (decision-protocol:make-choice-question
                    :id :risk
                    :instructions "Allow this effect?"
                    :criteria '((:allow . "proceed") (:deny . "stop"))))
  :model :kev-latest))
```

Tests inject `request-fn` — no live sidecar required.

```bash
sbcl --load examples/systemone.lisp          # fixture
DECISION_LIVE=1 sbcl --load examples/systemone.lisp   # local Kev
```

```lisp
(asdf:test-system "decision-backend-http")
```

## License

MIT
