;;;; PLAIN-TEXT system definition
;;;; (c) 2017 Vsevolod Dyomkin

(asdf:defsystem #:plain-text
  :version "0.1.0"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "MIT"
  :description "A tool to extract plain text from HTML pages.

Currently, a loose lisp implementation of the arc90labs readability algorithm."
  :depends-on (#:rutilsx #:cl-ppcre #:lquery
               #+dev #:should-test)
  :serial t
  :components ((:file "plain-text")
               ;; #+dev (:file "test")
               ))
