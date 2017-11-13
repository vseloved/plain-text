;;;;; PLAIN-TEXT implementation (based on the arc90labs readability algorithm)
;;;;; (c) 2017 Vsevolod Dyomkin

(cl:defpackage #:plain-text
  (:nicknames #:ptxt)
  (:use #:common-lisp #:rutilsx)
  (:local-nicknames (#:re #:ppcre))
  (:import-from #:lquery #:$ #:$1)
  (:export #:extract))

(in-package #:ptxt)
(named-readtables:in-readtable rutilsx-readtable)

(defparameter *regs*
  (pairs->ht
   (mapcar ^(pair (lt %) (re:create-scanner (reduce 'strcat (rt %))
                                            :case-insensitive-mode t))
           '((:unlikely-candidates
              ("banner|breadcrumbs|button|combx|comment|community|cover-wrap|"
               "disqus|extra|foot|header|legends|menu|menupop|menubutton|modal|"
               "related|remark|replies|rss|shoutbox|sidebar|skyscraper|social|"
               "sponsor|supplemental|ad-break|agegate|pagination|pager|popup|"
               "yom-remote|popup|tweet|twitter"))
             (:ok-candidates
              ("and|article|body|column|main|shadow"))
             (:positive
              ("article|body|content|entry|h-?entry|main|page|pagination|post|"
               "text|blog|story"))
             (:negative
              ("hidden|^hid$| hid$| hid |^hid |banner|combx|comment|com-|"
               "contact|foot|footer|footnote|masthead|media|modal|"
               "meta|outbrain|promo|related|scroll|shoutbox|sidebar|sponsor|"
               "shopping|tags|tool|widget|print|archive|comment|discuss|"
               "e[\\-]?mail|share|reply|all|login|sign|single|skyscraper"))))))

;;; patches to plump

(plump:define-tag-dispatcher (plump:comment plump:*tag-dispatchers*
                              plump:*xml-tags* plump:*html-tags*)
    (name)
    (and (<= 3 (length name))
         (string= name "!--" :end1 3))
  (plump:make-comment
   plump:*root*
   (plump:decode-entities
    (if (and (ends-with "--" name)
             (char= (or (plump:peek) #\!) #\>))
        (prog1 (if (> (length name) 5)
                   (subseq name 3 (- (length name) 2))
                   "")
          (plump:advance))
        (prog1 (concatenate
                'string (subseq name 3)
                (plump:consume-until
                 (plump:make-matcher (is "-->"))))
          (plump:advance-n 3))))))



;;; individual node manipulation

(defun node-name (node)
  (? node 'lquery::%tag-name))

(defun node-score (node)
  (+ (case (mkeyw (node-name node))
       (:div 5)
       (:p 5)
       (:blockquote 3)
       (:li -1)
       (:form -3)
       (:th -5)
       (otherwise 0))
      (class-score node)))

(defun class-score (node)
  (let ((score 0)
        (id+class (fmt "~A ~A"
                       (? ($ node (attr "id")) 0)
                       (? ($ node (attr "class")) 0))))
      (when (re:scan (? *regs* :negative) id+class)
        (:- score 25))
      (when (re:scan (? *regs* :positive) id+class)
        (:+ score 25))
     score))

(defun link-density (node)
  (let ((links ($ node "a"))
        (text-len (length (? ($ node (text)) 0))))
    (if (zerop text-len)
        0
        (/ (sum ^(length (? ($ % (text)) 0))
                links)
           text-len))))


;;; extraction

(defun uniqify-newlines (text)
  (flet ((newlinep (char) (member char '(#\Newline #\Return #\Linefeed)))
         (whitep (char) (member char '(#\Space #\Tab))))
    (with-output-to-string (rez)
      (let ((off 0))
        (loop :for pos := (position-if #'newlinep text)
                :then (position-if #'newlinep text :start off)
              :while pos :do
           (write-line (slice text off (position-if #'whitep text
                                                    :end pos :from-end t))
                       rez)
           (:= off (position-if-not ^(or (newlinep %) (whitep %))
                                    text :start pos))
           :finally (write-line (slice text off)
                                rez))))))

(declaim (inline unlikely-candidate? possible-text-blocks))

(defun unlikely-candidate? (node)
  (let ((id+class (strcat (? ($ node (attr "id")) 0) " "
                          (? ($ node (attr "class")) 0))))
    (and (re:scan (? *regs* :unlikely-candidates) id+class)
         (not (re:scan (? *regs* :ok-candidates) id+class)))))

(defun possible-text-blocks (node)
  (concatenate 'vector
               ($ node "p")
               ($ node "div")
               ($ node "blockquote")
               ($ node "a")
               ($ node "li")
               ($ node "th")
               ($ node "td")
               ($ node "article")
               ($ node "h1")
               ($ node "h2")
               ($ node "h3")
               ($ node "h4")
               ($ node "h5")
               ($ node "h6")))

(defun clean-text (node)
  (if node
      (progn
        (dovec (p (possible-text-blocks node))
          (when (unlikely-candidate? p)
            ($ p (detach))))
        (-> ($ node (text) (node))
            (re:regex-replace-all "<[^>]+>" % " ")
            (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed) %)
            uniqify-newlines
            (re:regex-replace-all " +" % " ")))
      ""))

(defun extract (html-text)
  (if-it (or (re:scan-to-strings "(?s:<body.*body>)" html-text)
             (re:scan-to-strings "(?s:<body.*)" html-text))
         (with ((body ($ (lquery:initialize it)))
                (scores #h())
                (max 0)
                (winner nil))
           ($ body "script" (detach))
           ($ body "noscript" (detach))
           ($ body "style" (detach))
           ($ body "object" (detach))
           ($ body "form" (detach))
           ($ body "iframe" (detach))
           ($ body "aside" (detach))
           (dovec (p (possible-text-blocks body))
             (when ($1 p (parents))
               (with ((parents ($ p (parents)))
                      (parent (? parents 0))
                      (grandparent (when (> (length parents) 1) (? parents 1)))
                      (text (? ($ p (text)) 0))
                      (char-count (count #\Space text :test-not 'eql))
                      (score (+ (count #\, text)
                                (min 3 (floor char-count 100))
                                1)))
                 (when (or (zerop char-count)
                           (unlikely-candidate? p))
                   ($ p (detach))
                   (return))
                 (unless (in# parent scores)
                   (:= (? scores parent) (node-score parent)))
                 (:+ (? scores parent) score)
                 (when grandparent
                   (unless (in# grandparent scores)
                     (:= (? scores grandparent) (node-score grandparent)))
                   (:+ (? scores grandparent) (/ score 2))))))
           (dotable (node score scores)
             (when (> score max)
               (:= max score
                   winner node)))
           (values
            (fmt "~A~%~%~A"
                (with ((_ match (re:scan-to-strings "<title[^>]*>([^<]*)</title>"
                                                    html-text)))
                  (if match (? match 0) ""))
                (clean-text winner))
            body))
         (values "" nil)))

(defmethod plump:text ((node plump:nesting-node))
  "Compiles all text nodes within the nesting-node into one string."
  (with-output-to-string (stream)
    (labels ((r (node)
               (loop for child across (plump:children node)
                     do (typecase child
                          (plump:textual-node (write-string (plump:text child)
                                                            stream))
                          (plump:nesting-node (r child)))
                        (write-string " " stream))))
      (r node))))

(defmethod plump:text ((node plump:comment))
  "")
