(in-package :cl-user)
(defpackage fast-http.parser
  (:use :cl
        :fast-http.http
        :fast-http.error)
  (:import-from :fast-http.byte-vector
                :+space+
                :+tab+
                :+cr+
                :+lf+
                :simple-byte-vector
                :digit-byte-char-p
                :digit-byte-char-to-integer)
  (:import-from :fast-http.util
                :defun-insane
                :defun-speedy
                :casev=
                :case-byte)
  (:import-from :alexandria
                :format-symbol
                :with-gensyms
                :hash-table-alist
                :define-constant
                :when-let)
  (:export :callbacks

           :make-callbacks

           :parse-request
           :parse-response

           ;; Conditions
           :eof

           ;; Types
           :pointer))
(in-package :fast-http.parser)

;;
;; Variables

(declaim (type fixnum +max-header-line+))
(defconstant +max-header-line+ 1024
  "Maximum number of header lines allowed.

This restriction is for protecting users' application
against denial-of-service attacks where the attacker feeds
us a never-ending header that the application keeps buffering.")


;;
;; Types

(deftype pointer () 'integer)


;;
;; Callbacks

(defstruct callbacks
  (message-begin nil :type (or null function))     ;; 1 arg
  (url nil :type (or null function))
  (first-line nil :type (or null function))
  (status nil :type (or null function))
  (header-field nil :type (or null function))
  (header-value nil :type (or null function))
  (headers-complete nil :type (or null function))  ;; 1 arg
  (body nil :type (or null function))
  (message-complete nil :type (or null function)))

(defmacro callback-data (name http callbacks data start end)
  (with-gensyms (callback e)
    `(when-let (,callback (,(format-symbol t "~A-~A" :callbacks name) ,callbacks))
       (handler-bind ((error
                        (lambda (,e)
                          (unless (typep ,e 'fast-http-error)
                            (error ',(format-symbol t "~A-~A" :cb name)
                                   :error ,e)
                            (abort ,e)))))
         (funcall ,callback ,http ,data ,start ,end)))))

(defmacro callback-notify (name http callbacks)
  (with-gensyms (callback e)
    `(when-let (,callback (,(format-symbol t "~A-~A" :callbacks name) ,callbacks))
       (handler-bind ((error
                        (lambda (,e)
                          (unless (typep ,e 'fast-http-error)
                            (error ',(format-symbol t "~A-~A" :cb name)
                                   :error ,e)
                            (abort ,e)))))
         (funcall ,callback ,http)))))


;;
;; Parser utilities

(define-condition eof () ())

(defmacro check-eof ()
  `(when (= p end)
     (error 'eof)))

(defmacro advance (&key (if-eof '(error 'eof)))
  `(progn
     (incf p)
     (when (= p end)
       ,if-eof)
     (setq byte (aref data p))))

(defmacro advance-to (to)
  `(progn
     (setq p ,to)
     (check-eof)
     (setq byte (aref data p))))

(defmacro skip-while (form)
  `(loop while (progn ,form)
         do (advance)))

(defmacro skip-while-spaces ()
  '(skip-while (or (= byte +space+) (= byte +tab+))))

(defmacro skip-until (form)
  `(loop until (progn ,form)
         do (advance)))

(defmacro skip-until-crlf ()
  `(loop if (= byte +cr+)
           do (progn (expect-byte +lf+)
                     (return))
         else
           do (advance)))

(define-condition expect-failed (parsing-error)
  ((form :initarg :form
         :initform nil))
  (:report (lambda (condition stream)
             (format stream "Expectation failed: ~S"
                     (slot-value condition 'form)))))

(defmacro expect (form &optional (error ''expect-failed) (advance T))
  `(progn
     ,@(and advance `((advance)))
     ,(if error
          `(unless ,form
             (error ,error
                    ,@(if (equal error ''expect-failed)
                          `(:form ',form)
                          nil)))
          form)))

(defmacro expect-byte (byte &optional (error ''expect-failed) (advance T))
  "Advance the pointer and check if the next byte is BYTE."
  `(expect (= byte ,byte) ,error ,advance))

(defmacro expect-char (char &optional (error ''expect-failed) (advance T) (case-sensitive T))
  (if case-sensitive
      `(expect-byte ,(char-code char) ,error ,advance)
      `(expect (or (= byte ,(char-code char))
                   (= byte ,(if (lower-case-p char)
                                (- (char-code char) 32)
                                (+ (char-code char) 32))))
           ,error
           ,advance)))

(defmacro expect-string (string &optional (error ''expect-failed) (advance T) (case-sensitive T))
  (if advance
      `(,(if error 'progn 'and)
        ,@(loop for char across string
                collect `(expect-char ,char ,error ,advance ,case-sensitive)))
      (when (/= 0 (length string))
        `(,(if error 'progn 'and)
          (expect-char ,(aref string 0) ,error nil ,case-sensitive)
          (expect-string ,(subseq string 1) ,error T ,case-sensitive)))))

(defmacro expect-crlf (&optional (error ''expect-failed))
  `(casev= byte
     (+cr+ (expect-byte +lf+ ,error))
     ,@(and error
            `((otherwise (error ,error))))))

(defmacro expect-one-of (strings &optional (error ''expect-failed) (case-sensitive T))
  (let ((expect-block (gensym "EXPECT-BLOCK")))
    (labels ((grouping (i strings)
               (let ((map (make-hash-table)))
                 (loop for string in strings
                       when (< 0 (- (length (string string)) i))
                         do (push string
                                  (gethash (aref (string string) i) map)))
                 (alexandria:hash-table-alist map)))
             (build-case-byte (i strings)
               (let ((alist (grouping i strings)))
                 (if alist
                     `(case-byte byte
                        ,@(loop for (char . candidates) in alist
                                if (cdr candidates)
                                  collect `(,(if case-sensitive
                                                 char
                                                 (if (lower-case-p char)
                                                     `(,char ,(code-char (- (char-code char) 32)))
                                                     `(,char ,(code-char (+ (char-code char) 32)))))
                                            (advance) ,(build-case-byte (1+ i) candidates))
                                else
                                  collect `(,(if case-sensitive
                                                 char
                                                 (if (lower-case-p char)
                                                     `(,char ,(code-char (- (char-code char) 32)))
                                                     `(,char ,(code-char (+ (char-code char) 32)))))
                                            (expect-string ,(subseq (string (car candidates)) (1+ i)) ,error T ,case-sensitive)
                                            (return-from ,expect-block ,(car candidates))))
                        (otherwise ,(if error
                                        `(error ,error)
                                        `(return-from ,expect-block nil))))
                     `(return-from ,expect-block ,(car strings))))))
      `(block ,expect-block
         ,(build-case-byte 0 strings)))))

(defmacro case-expect-header-field (strings &body clauses)
  (with-gensyms (expect-block)
    (labels ((grouping (i strings)
               (let ((map (make-hash-table)))
                 (loop for string in strings
                       when (< 0 (- (length (string string)) i))
                         do (push string
                                  (gethash (aref (string string) i) map)))
                 (alexandria:hash-table-alist map)))
             (build-case (i strings)
               (let ((alist (grouping i strings)))
                 (if alist
                     `(case (the character (svref +tokens+ byte))
                        (#\Nul (error 'invalid-header-token))
                        ,@(loop for (char . candidates) in alist
                                collect `(,char (advance) ,(build-case (1+ i) candidates)))
                        (otherwise (go otherwise)))
                     `(if (= byte (char-code #\:))
                          (return-from ,expect-block
                            (progn ,@(cdr (assoc (intern (car strings) :keyword) clauses))))
                          (go otherwise))))))
      `(block ,expect-block
         (tagbody
            ,(build-case 0 strings)

          otherwise
            (return-from ,expect-block
              (progn ,@(cdr (assoc 'otherwise clauses)))))))))


;;
;; Tokens

(declaim (type (simple-array character (128)) +tokens+))
(define-constant +tokens+
    #( #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul   #\!   #\Nul   #\#    #\$    #\%    #\&    #\'
       #\Nul  #\Nul   #\*    #\+   #\Nul    #\-   #\.   #\Nul
        #\0    #\1    #\2    #\3    #\4    #\5    #\6    #\7
        #\8    #\9   #\Nul  #\Nul  #\Nul  #\Nul  #\Nul  #\Nul
       #\Nul   #\a    #\b    #\c    #\d    #\e    #\f    #\g
        #\h    #\i    #\j    #\k    #\l    #\m    #\n    #\o
        #\p    #\q    #\r    #\s    #\t    #\u    #\v    #\w
        #\x    #\y    #\z   #\Nul  #\Nul  #\Nul   #\^    #\_
        #\`    #\a    #\b    #\c    #\d    #\e    #\f    #\g
        #\h    #\i    #\j    #\k    #\l    #\m    #\n    #\o
        #\p    #\q    #\r    #\s    #\t    #\u    #\v    #\w
        #\x    #\y    #\z   #\Nul   #\|   #\Nul   #\~   #\Nul )
  :test 'equalp)

(declaim (type (simple-array fixnum (128)) +unhex+))
(define-constant +unhex+
    #(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
       0  1  2  3  4  5  6  7  8  9 -1 -1 -1 -1 -1 -1
      -1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 10 11 12 13 14 15 -1 -1 -1 -1 -1 -1 -1 -1 -1
      -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
  :test 'equalp)

(defun-insane unhex-byte (byte)
  (aref +unhex+ byte))

;;
;; Main

(defun-insane parse-method (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p))
    (values
     (prog1 (expect-one-of
                (:CONNECT
                 :COPY
                 :CHECKOUT
                 :DELETE
                 :GET
                 :HEAD
                 :LOCK
                 :MKCOL
                 :MKCALENDAR
                 :MKACTIVITY
                 :MOVE
                 :MERGE
                 :M-SEARCH
                 :NOTIFY
                 :OPTIONS
                 :POST
                 :PROPFIND
                 :PROPPATCH
                 :PUT
                 :PURGE
                 :PATCH
                 :REPORT
                 :SEARCH
                 :SUBSCRIBE
                 :TRACE
                 :UNLOCK
                 :UNSUBSCRIBE)
                'invalid-method)
       (expect-byte +space+ 'invalid-method))
     p)))

(defun-insane parse-url (http callbacks data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p))
    (skip-while (or (<= (char-code #\!) byte (char-code #\~))
                    (<= 128 byte)))
    (callback-data :url http callbacks data start p)
    p))

(defun-insane parse-http-version (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p))
         major minor)
    (declare (type (unsigned-byte 8) byte)
             (type pointer p end))
    (expect-string "HTTP/" 'expect-failed nil)
    ;; Expect the HTTP major is only once digit.
    (expect (digit-byte-char-p byte))
    (setq major (digit-byte-char-to-integer byte))
    (expect-byte (char-code #\.))
    ;; Expect the HTTP minor is only once digit.
    (expect (digit-byte-char-p byte))
    (setq minor (digit-byte-char-to-integer byte))

    (values major minor p)))

(defun-insane parse-status-code (http callbacks data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p end))
    (unless (digit-byte-char-p byte)
      (error 'invalid-status))
    (setf (http-status http) (digit-byte-char-to-integer byte))
    (loop
      (advance)
      (cond
        ((digit-byte-char-p byte)
         (setf (http-status http)
               (+ (the fixnum (* 10 (http-status http)))
                  (digit-byte-char-to-integer byte)))
         (when (< 999 (http-status http))
           (error 'invalid-status :status-code (http-status http))))
        ((= byte +space+)
         ;; Reading the status text
         (skip-while (= byte +space+))
         (let ((status-text-start p))
           (skip-until-crlf)
           (callback-data :status http callbacks data status-text-start (- p 1)))
         (advance)
         (return))
        ((= byte +cr+)
         ;; No status text
         (expect-byte +lf+)
         (advance)
         (return))
        (T (error 'invalid-status))))
    p))

(defmacro parse-header-field-and-value ()
  `(macrolet ((skip-until-field-end ()
                `(do ((char (svref +tokens+ byte)
                            (svref +tokens+ byte)))
                     ((= byte (char-code #\:)))
                   (declare (type character char))
                   (when (char= char #\Nul)
                     (error 'invalid-header-token))
                   (advance)))
              (skip-until-value-start-and (&body body)
                `(progn
                    ;; skip #\: and leading spaces
                   (advance)
                   (skip-while-spaces)
                   (casev= byte
                     (+cr+
                      ;; continue to the next line
                      (expect-byte +lf+)
                      (advance)
                      (casev= byte
                        ((+space+ +tab+)
                         (skip-while-spaces)
                         (if (= byte +cr+)
                             ;; empty body
                             (progn
                               (expect-byte +lf+)
                               (advance)
                               (callback-data :header-field http callbacks data field-start field-end)
                               (callback-data :header-value http callbacks data p p))
                             (progn ,@body)))
                        (otherwise
                         ;; empty body
                         (callback-data :header-field http callbacks data field-start field-end)
                         (callback-data :header-value http callbacks data p p))))
                     (otherwise ,@body)))))
     (let ((field-start p) field-end)
       (declare (dynamic-extent field-start field-end))
       (case-expect-header-field ("content-length" "transfer-encoding" "upgrade")
         (:|content-length|
           (setq field-end p)
           (skip-until-value-start-and
            (multiple-value-bind (value-start value-end next content-length)
                (parse-header-value-content-length data p end)
              (declare (type pointer next))
              (setf (http-content-length http) content-length)
              (advance-to next)
              (callback-data :header-field http callbacks data field-start field-end)
              (callback-data :header-value http callbacks data value-start value-end))))
         (:|transfer-encoding|
           (setq field-end p)
           (skip-until-value-start-and
            (multiple-value-bind (value-start value-end next chunkedp)
                (parse-header-value-transfer-encoding data p end)
              (declare (type pointer next))
              (setf (http-chunked-p http) chunkedp)
              (advance-to next)
              (callback-data :header-field http callbacks data field-start field-end)
              (callback-data :header-value http callbacks data value-start value-end))))
         (:|upgrade|
           (setq field-end p)
           (setf (http-upgrade-p http) T)
           (skip-until-value-start-and
            (let ((value-start p))
              (skip-until-crlf)
              (advance)
              (callback-data :header-field http callbacks data field-start field-end)
              (callback-data :header-value http callbacks data value-start (- p 2)))))
         (otherwise (skip-until-field-end)
                    (setq field-end p)
                    (skip-until-value-start-and
                     (parse-header-value field-start field-end)))))))

(defmacro parse-header-value (&optional field-start field-end)
  `(let ((value-start p))
     (skip-until-crlf)
     (advance)
     ,@(and field-start field-end
            `((callback-data :header-field http callbacks data ,field-start ,field-end)))
     (callback-data :header-value http callbacks data value-start (- p 2))))

(defun-speedy parse-header-value-transfer-encoding (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p))

    (if (expect-string "chunked" nil nil nil)
        (progn
          (advance)
          (casev= byte
            (+cr+ (expect-byte +lf+)
                  (values start (1- p) (1+ p) t))
            (otherwise
             (skip-until-crlf)
             (advance)
             (values start (- p 2) p nil))))
        (progn
          (skip-until-crlf)
          (advance)
          (values start (- p 2) p nil)))))

(defun-speedy parse-header-value-content-length (data start end)
  (declare (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p))
         (content-length 0))
    (declare (type pointer p)
             (type (unsigned-byte 8) byte)
             (type integer content-length))
    (unless (digit-byte-char-p byte)
      (error 'invalid-content-length))
    (setq content-length (digit-byte-char-to-integer byte))
    (loop
      (advance)
      (cond
        ((digit-byte-char-p byte)
         (setq content-length
               (+ (* 10 content-length)
                  (digit-byte-char-to-integer byte))))
        ((= byte +cr+)
         (expect-byte +lf+)
         (return (values start (1- p) (1+ p) content-length)))
        ((= byte +space+)
         ;; Discard spaces
         )
        (T (error 'invalid-content-length))))))

(defmacro case-header-line-start (&body clauses)
  `(casev= byte
     ((+tab+ +space+)
      ,@(cdr (assoc :value clauses)))
     (+cr+ (expect-byte +lf+)
           (incf p)
           ,@(cdr (assoc :last clauses)))
     (otherwise ,@(cdr (assoc :field clauses)))))

(defmacro parse-header-line ()
  `(case-header-line-start
    (:field (parse-header-field-and-value))
    ;; folding value
    (:value (parse-header-value))
    (:last (return))))

(defun-speedy parse-headers (http callbacks data start end)
  (declare (type http http)
           (type simple-byte-vector data)
           (type pointer start end))
  (let* ((p start)
         (byte (aref data p)))
    (declare (type pointer p)
             (type (unsigned-byte 8) byte))
    ;; empty headers
    (when (= byte +cr+)
      (expect-byte +lf+)
      (return-from parse-headers (1+ p)))
    (parse-header-field-and-value)
    (setf (http-mark http) p)
    (loop
      (when (= +max-header-line+ (the fixnum (incf (http-header-read http))))
        (error 'header-overflow))
      (parse-header-line)
      (setf (http-mark http) p))
    (setf (http-mark http) p)
    (setf (http-state http) +state-body+)
    p))

(defun-speedy read-body-data (http callbacks data start end)
  (declare (type http http)
           (type simple-byte-vector data)
           (type pointer start end))
  (let ((readable-count (the pointer (- end start))))
    (declare (dynamic-extent readable-count)
             (type pointer readable-count))
    (if (<= (http-content-length http) readable-count)
        (let ((body-end (+ start (http-content-length http))))
          (declare (dynamic-extent body-end))
          (setf (http-content-length http) 0)
          (callback-data :body http callbacks data start body-end)
          (setf (http-mark http) body-end)
          (values body-end t))
        ;; still needs to read
        (progn
          (decf (http-content-length http) readable-count)
          (callback-data :body http callbacks data start end)
          (setf (http-mark http) end)
          (values end nil)))))

(defun-speedy http-message-needs-eof-p (http)
  (let ((status-code (http-status http)))
    (declare (type status-code status-code))
    (when (= status-code 0) ;; probably request
      (return-from http-message-needs-eof-p nil))

    (when (and (< 99 status-code 200) ;; 1xx e.g. Continue
               (= status-code 204)    ;; No Content
               (= status-code 304))   ;; Not Modified
      (return-from http-message-needs-eof-p nil))

    (when (or (http-chunked-p http)
              (http-content-length http))
      (return-from http-message-needs-eof-p nil))
    T))

(defun-speedy parse-body (http callbacks data start end requestp)
  (declare (type http http)
           (type simple-byte-vector data)
           (type pointer start end))
  (macrolet ((message-complete ()
               `(progn
                  (callback-notify :message-complete http callbacks)
                  (setf (http-state http) +state-first-line+))))
    (case (http-content-length http)
      (0
       ;; Content-Length header given but zero: Content-Length: 0\r\n
       (message-complete)
       start)
      ('nil
       (if (or requestp
               (not (http-message-needs-eof-p http)))
           ;; Assume content-length 0 - read the next
           (progn
             (message-complete)
             end)
           ;; read until EOF
           (progn
             (callback-data :body http callbacks data start end)
             (setf (http-mark http) end)
             end)))
      (otherwise
       ;; Content-Length header given and non-zero
       (multiple-value-bind (next completedp)
           (read-body-data http callbacks data start end)
         (when completedp
           (message-complete))
         next)))))

(defmacro parse-chunked-size ()
  `(let ((unhex-val (unhex-byte byte)))
     (declare (type fixnum unhex-val)
              (dynamic-extent unhex-val))
     (when (= unhex-val -1)
       (error 'invalid-chunk-size))
     (setf (http-content-length http) unhex-val)

     (loop
       (advance)
       (cond
         ((= byte +cr+)
          (expect-byte +lf+)
          (advance :if-eof (return t))
          (return nil))
         (T
          (setq unhex-val (unhex-byte byte))
          (cond
            ((= unhex-val -1)
             (case-byte byte
               ((#\; #\Space)
                ;; skipping chunk parameters
                (skip-until-crlf)
                (advance :if-eof (return t))
                (return nil))
               (otherwise
                (error 'invalid-chunk-size))))
            (T
             (setf (http-content-length http)
                   (+ (* 16 (http-content-length http)) unhex-val)))))))))

(defun-speedy parse-chunked-body (http callbacks data start end)
  (declare (type http http)
           (type simple-byte-vector data)
           (type pointer start end))

  (when (= start end)
    (return-from parse-chunked-body start))

  (let* ((p start)
         (byte (aref data p)))
    (declare (type pointer p)
             (type (unsigned-byte 8) byte))

    (tagbody
       (cond
         ((= (http-state http) +state-chunk-size+)
          (go chunk-size))
         ((= (http-state http) +state-body+)
          (go body))
         ((= (http-state http) +state-chunk-body-end-crlf+)
          (go body-end-crlf))
         ((= (http-state http) +state-trailing-headers+)
          (go trailing-headers))
         (T (error 'invalid-internal-state)))

     chunk-size
       (let ((eofp (parse-chunked-size)))
         (setf (http-state http) +state-body+)
         (if eofp
             (return-from parse-chunked-body p)
             (setf (http-mark http) p)))

     body
       (cond
         ((zerop (http-content-length http))
          ;; trailing headers
          (setf (http-state http) +state-trailing-headers+)
          (go trailing-headers))
         (T
          (multiple-value-bind (next completedp)
              (read-body-data http callbacks data p end)
            (declare (type pointer next))
            (unless completedp
              (return-from parse-chunked-body p))
            (setf (http-state http) +state-chunk-body-end-crlf+)
            (advance-to next))))

     body-end-crlf
       (expect-crlf)
       (setf (http-state http) +state-chunk-size+)
       (advance :if-eof (return-from parse-chunked-body p))
       (setf (http-mark http) p)
       (go chunk-size)

     trailing-headers
       (return-from parse-chunked-body
         (prog1 (parse-headers http callbacks data p end)
           (callback-notify :message-complete http callbacks))))))

(defun-speedy parse-request (http callbacks data &key (start 0) end)
  (declare (type http http)
           (type simple-byte-vector data))
  (let* ((p start)
         (byte (aref data p))
         (end (or end (length data))))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p end))
    (setf (http-mark http) start)
    (check-eof)

    (tagbody
       (let ((state (http-state http)))
         (declare (type fixnum state))
         (cond
           ((= +state-first-line+ state)
            (go first-line))
           ((= +state-headers+ state)
            (go headers))
           ((<= +state-chunk-size+ state +state-trailing-headers+)
            (go body))
           (T (error 'invalid-internal-state))))

     first-line
       ;; skip first empty line (some clients add CRLF after POST content)
       (when (= byte +cr+)
         (expect-byte +lf+)
         (advance :if-eof (return-from parse-request p)))

       (setf (http-mark http) p)
       (callback-notify :message-begin http callbacks)

       (multiple-value-bind (method next)
           (parse-method data p end)
         (declare (type pointer next))
         (setf (http-method http) method)
         (advance-to next))
       (skip-while (= byte +space+))
       (let ((next (parse-url http callbacks data p end)))
         (declare (type pointer next))
         (advance-to next))

       (skip-while (= byte +space+))

       (cond
         ;; No HTTP version
         ((= byte +cr+)
          (expect-byte +lf+)
          (advance))
         (T (multiple-value-bind (major minor next)
                (parse-http-version data p end)
              (declare (type pointer next))
              (setf (http-major-version http) major
                    (http-minor-version http) minor)
              (advance-to next))

            (advance)
            (expect-crlf)
            (advance)))

       (setf (http-mark http) p)
       (setf (http-state http) +state-headers+)
       (callback-notify :first-line http callbacks)

     headers
       (setq p (parse-headers http callbacks data p end))

       (callback-notify :headers-complete http callbacks)
       (setf (http-header-read http) 0)

       ;; Exit, the rest of the connect is in a different protocol.
       (when (http-upgrade-p http)
         (setf (http-state http) +state-first-line+)
         (callback-notify :message-complete http callbacks)
         (return-from parse-request p))

       (setf (http-state http)
             (if (http-chunked-p http)
                 +state-chunk-size+
                 +state-body+))

     body
       (if (http-chunked-p http)
           (setq p (parse-chunked-body http callbacks data p end))
           (progn
             (setq p (parse-body http callbacks data p end t))
             (unless (= p end)
               (setq byte (aref data p))
               (go first-line))))
       (return-from parse-request p))))

(defun-speedy parse-response (http callbacks data &key (start 0) end)
  (declare (type http http)
           (type simple-byte-vector data))
  (let* ((p start)
         (byte (aref data p))
         (end (or end (length data))))
    (declare (type (unsigned-byte 8) byte)
             (type pointer p end))
    (setf (http-mark http) start)
    (check-eof)

    (tagbody
       (let ((state (http-state http)))
         (declare (type fixnum state))
         (cond
           ((= +state-first-line+ state)
            (go first-line))
           ((= +state-headers+ state)
            (go headers))
           ((<= +state-chunk-size+ state +state-trailing-headers+)
            (go body))
           (T (error 'invalid-internal-state))))

     first-line
       (setf (http-mark http) p)
       (callback-notify :message-begin http callbacks)

       (multiple-value-bind (major minor next)
           (parse-http-version data p end)
         (declare (type pointer next))
         (setf (http-major-version http) major
               (http-minor-version http) minor)
         (advance-to next))

       (advance)

       (cond
         ((= byte +space+)
          (advance)
          (advance-to (parse-status-code http callbacks data p end)))
         ((= byte +cr+)
          (expect-byte +lf+)
          (advance))
         (T (error 'invalid-version)))

       (setf (http-mark http) p)
       (setf (http-state http) +state-headers+)
       (callback-notify :first-line http callbacks)

     headers
       (setq p (parse-headers http callbacks data p end))

       (callback-notify :headers-complete http callbacks)
       (setf (http-header-read http) 0)
       (setf (http-state http)
             (if (http-chunked-p http)
                 +state-chunk-size+
                 +state-body+))

     body
       (if (http-chunked-p http)
           (setq p (parse-chunked-body http callbacks data p end))
           (progn
             (setq p (parse-body http callbacks data p end nil))
             (unless (= p end)
               (setq byte (aref data p))
               (go first-line))))
       (return-from parse-response p))))


(defun parse-header-value-parameters (data &key
                                             header-value-callback
                                             header-parameter-key-callback
                                             header-parameter-value-callback)
  (declare (type simple-string data)
           (optimize (speed 3) (safety 2)))

  (let* ((header-name-mark 0)
         parameter-key-mark
         parameter-value-mark
         parsing-quoted-string-p
         (p 0)
         (end (length data))
         (char (aref data p)))
    (declare (type character char))

    (when (= end 0)
      (return-from parse-header-value-parameters 0))

    (macrolet ((go-state (state &optional (advance 1))
                   `(locally (declare (optimize (speed 3) (safety 0)))
                      (incf p ,advance)
                      (when (= p end)
                        (go eof))
                      (setq char (aref data p))
                      (go ,state))))
      (flet ((tokenp (char)
               (declare (optimize (speed 3) (safety 0)))
               (let ((byte (char-code char)))
                 (and (< byte 128)
                      (not (char= (the character (aref +tokens+ byte)) #\Nul))))))
        (tagbody
         parsing-header-value-start
           (case char
             ((#\Space #\Tab)
              (go-state parsing-header-value))
             (otherwise
              (unless (tokenp char)
                (error 'invalid-header-value))
              (setq header-name-mark p)
              (go-state parsing-header-value 0)))

         parsing-header-value
           (case char
             (#\;
              (when header-value-callback
                (funcall (the function header-value-callback)
                         data header-name-mark p))
              (setq header-name-mark nil)
              (go-state looking-for-parameter-key))
             (otherwise (go-state parsing-header-value)))

         looking-for-parameter-key
           (case char
             ((#\Space #\Tab #\; #\Newline #\Return)
              (go-state looking-for-parameter-key))
             (otherwise
              (unless (tokenp char)
                (error 'invalid-parameter-key))
              (setq parameter-key-mark p)
              (go-state parsing-parameter-key)))

         parsing-parameter-key
           (case char
             (#\=
              (assert parameter-key-mark)
              (when header-parameter-key-callback
                (funcall (the function header-parameter-key-callback)
                         data parameter-key-mark p))
              (setq parameter-key-mark nil)
              (go-state parsing-parameter-value-start))
             (otherwise
              (unless (tokenp char)
                (error 'invalid-parameter-key))
              (go-state parsing-parameter-key)))

         parsing-parameter-value-start
           (case char
             (#\"
              ;; quoted-string
              (setq parameter-value-mark (1+ p))
              (setq parsing-quoted-string-p t)
              (go-state parsing-parameter-quoted-value))
             ((#.+space+ #.+tab+)
              (go-state parsing-parameter-value-start))
             (otherwise
              (setq parameter-value-mark p)
              (go-state parsing-parameter-value 0)))

         parsing-parameter-quoted-value
           (if (char= char #\")
               (progn
                 (assert parameter-value-mark)
                 (setq parsing-quoted-string-p nil)
                 (when header-parameter-value-callback
                   (funcall (the function header-parameter-value-callback)
                            data parameter-value-mark p))
                 (setq parameter-value-mark nil)
                 (go-state looking-for-parameter-key))
               (go-state parsing-parameter-quoted-value))

         parsing-parameter-value
           (case char
             (#\;
              (assert parameter-value-mark)
              (when header-parameter-value-callback
                (funcall (the function header-parameter-value-callback)
                         data parameter-value-mark p))
              (setq parameter-value-mark nil)
              (go-state looking-for-parameter-key))
             (otherwise
              (go-state parsing-parameter-value)))

         eof
           (when header-name-mark
             (when header-value-callback
               (funcall (the function header-value-callback)
                        data header-name-mark p)))
           (when parameter-key-mark
             (error 'invalid-eof-state))
           (when parameter-value-mark
             (when parsing-quoted-string-p
               (error 'invalid-eof-state))
             (when header-parameter-value-callback
               (funcall (the function header-parameter-value-callback)
                        data parameter-value-mark p))))))
    p))
