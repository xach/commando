;;;; commando.lisp

(in-package #:commando)

;;; "commando" goes here. Hacks and glory await!

(defvar *command-output* (make-synonym-stream '*standard-output*))

(defun stringify-command-argument (argument)
  (typecase argument
    (string argument)
    (pathname (native-namestring argument))
    (keyword (format nil "--~(~A~)" argument))
    (t (princ-to-string argument))))

(defun run (command &rest arguments)
  (let ((process (run-program command (mapcar #'stringify-command-argument arguments)
                              :search t
                              :wait t
                              :output *command-output*)))
    (let ((code (process-exit-code process)))
      (if (zerop code)
          t
          (error "Command exited with non-zero status ~D" code)))))

(defmacro with-run-output ((stream (command &rest args)) &body body)
  `(let* ((*command-output* (make-string-output-stream)))
     (run ,command ,@args)
     (with-input-from-string (,stream (get-output-stream-string  *command-output*))
       ,@body)))

(defun native-directory-string (pathname)
  ;; FIXME: directory-namestring fails on Windows due to lack of drive
  ;; info. Maybe I care.
  (native-namestring (directory-namestring (probe-file pathname))))

(defmacro with-posix-cwd (new-directory &body body)
  "Evaluate BODY with *DEFAULT-PATHNAME-DEFAULTS* and the POSIX
working directory set to NEW-DIRECTORY."
  ;; fchdir thing from Linux's getcwd(3)
  (let ((fd (gensym))
        (new (gensym)))
    `(let ((,fd nil)
           (,new (native-directory-string ,new-directory)))
       (unwind-protect
            (let ((*default-pathname-defaults* (probe-file ,new)))
              (setf ,fd (sb-posix:open "." 0))
              (sb-posix:chdir ,new)
              ,@body)
         (when ,fd
           (sb-posix:fchdir ,fd)
           (ignore-errors (sb-posix:close ,fd)))))))

(defmacro with-binary-run-output (pathname &body body)
  `(with-open-file (*command-output* ,pathname :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
     ,@body))

(defmacro without-run-output (&body body)
  `(let ((*command-output* nil))
     ,@body))

(defun run-output-lines (command &rest args)
  (let ((output (with-output-to-string (*command-output*)
                  (apply #'run command args))))
    (with-input-from-string (stream output)
      (loop for line = (read-line stream nil)
            while line collect line))))


;;; Temporary directory work

(defvar *random-alphanumeric*
  (concatenate 'string
               "abcdefghijklmnopqrstuvwxyz"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               "0123456789"))

(defun random-element (vector)
  (aref vector (random (length vector))))

(defun random-char ()
  (random-element *random-alphanumeric*))

(defun random-string (length)
  (map-into (make-string length) 'random-char))

(defun native (pathname)
  (native-namestring (merge-pathnames pathname)))

(defun call-in-temporary-directory (template-pathname fun)
  (flet ((random-temporary ()
           (let* ((parts (pathname-directory template-pathname))
                  (last (first (last parts)))
                  (randomized (format nil "~A-~A" last (random-string 8))))
             (make-pathname :directory (nconc (butlast parts) (list randomized))
                            :defaults template-pathname))))
    (block nil
      (tagbody
       retry
         (let ((path (random-temporary)))
           (handler-case
               (progn
                 (sb-posix:mkdir (native path) #o700)
                 (unwind-protect
                      (with-posix-cwd path
                        (return (funcall fun)))
                   (ignore-errors (run "rm" "-rf" (native path)))))
             (sb-posix:syscall-error (condition)
               (when (= (sb-posix:syscall-errno condition)
                        sb-posix:eexist)
                 (go retry))
               (error condition))))))))

(defmacro in-temporary-directory (template-pathname &body body)
  `(call-in-temporary-directory ,template-pathname (lambda () ,@body)))

(defun copy-file (from to)
  (run "cp" (native (truename from)) (native to)))
