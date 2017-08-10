;;;;; PLAIN-TEXT implementation (based on the arc90labs readability algorithm)
;;;;; (c) 2017 Vsevolod Dyomkin

(cl:defpackage #:plain-text
  (:nicknames #:ptxt)
  (:use #:common-lisp #:rutilsx)
  (:local-nicknames (#:re #:ppcre))
  (:import-from #:lquery #:$)
  (:export #:extract))

(in-package #:ptxt)
(named-readtables:in-readtable rutilsx-readtable)

(defparameter *regs*
  (pairs->ht
   (mapcar ^(pair (lt %) (re:create-scanner (reduce 'strcat (rt %))
                                            :case-insensitive-mode t))
           '((:unlikely-candidates
              '("banner|breadcrumbs|combx|comment|community|cover-wrap|disqus|"
                "extra|foot|header|legends|menu|modal|related|remark|replies|"
                "rss|shoutbox|sidebar|skyscraper|social|sponsor|supplemental|"
                "ad-break|agegate|pagination|pager|popup|yom-remote|"
                "popup|tweet|twitter"))
             (:ok-candidates
              '("and|article|body|column|main|shadow"))
             (:positive
              '("article|body|content|entry|h-?entry|main|page|pagination|post|"
                "text|blog|story"))
             (:negative
              '("hidden|^hid$| hid$| hid |^hid |banner|combx|comment|com-|"
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
       (:blockquote 3)
       (:form -3)
       (:th -5)
       (otherwise 0))
      (class-score node)))

(defun class-score (node)
  (let ((score 0))
    (when-it (? ($ node (attr "class")) 0)
      (when (re:scan (? *regs* :negative) it)
        (:- score 25))
      (when (re:scan (? *regs* :positive) it)
        (:+ score 25))
      (when-it (? ($ node (attr "id")) 0)
        (when (re:scan (? *regs* :negative) it)
          (:- score 25))
        (when (re:scan (? *regs* :positive) it)
          (:+ score 25))))
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

(defun clean-text (node)
  (-> ($ node (text) (node))
      (re:regex-replace-all "<[^>]+>" % " ")
      (re:regex-replace-all "^\\s+|\\s+$" % "")
      (re:regex-replace-all " +" % " ")
      (re:regex-replace-all (re:create-scanner
                             (fmt "\\s*[~C~C~C]+" #\Newline #\Return #\Linefeed)
                             :multi-line-mode t)
                            % (fmt "~%"))
      (re:regex-replace-all (re:create-scanner
                             (fmt "[~C~C~C]+\\s*" #\Newline #\Return #\Linefeed)
                             :multi-line-mode t)
                            % (fmt "~%"))))

(defun extract (html-text)
  (with ((html ($ (lquery:initialize html-text)))
         (body ($ html "body"))
         (scores #h())
         (max 0)
         (winner nil))
    ($ body "script" (detach))
    ($ body "style" (detach))
    ($ body "object" (detach))
    ($ body "form" (detach))
    ($ body "iframe" (detach))
    (dovec (p (concatenate 'vector
                           ($ body "p")
                           ($ body "div")
                           ($ body "blockquote")
                           ($ body "h1")
                           ($ body "h2")
                           ($ body "h3")
                           ($ body "h4")
                           ($ body "h5")
                           ($ body "h6")))
      (with ((id+class (strcat (? ($ p (attr "id")) 0)
                               (? ($ p (attr "class")) 0)))
             (parents ($ p (parents)))
             (parent (? parents 0))
             (grandparent (when (> (length parents) 1) (? parents 1)))
             (text ($ p (text)))
             (score (+ 1 (count #\, text)
                       (min 3 (floor (length text) 100)))))
        (when (and (re:scan (? *regs* :unlikely-candidates) id+class)
                   (not (re:scan (? *regs* :ok-candidates) id+class)))
          ($ p (detach))
          (return))
        (unless (in# parent scores)
          (:= (? scores parent) (node-score parent)))
        (:+ (? scores parent) score)
        (when grandparent
          (unless (in# grandparent scores)
            (:= (? scores grandparent) (node-score grandparent)))
          (:+ (? scores grandparent) (/ score 2)))))
    (dotable (node score scores)
      (when (> score max)
        (:= max score
            winner node)))
    (fmt "~A~%~%~A"
         (? ($ html "title" (text)) 0)
         (clean-text winner))))

(defmethod plump:text ((node plump:comment))
  "")
