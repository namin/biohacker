(define j (create-jtms "hello"))
(tms-create-node j 'a)

(create-jtms "hello again" #:debugging #t)
