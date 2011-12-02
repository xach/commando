;;;; commando.lisp

(in-package #:commando)

;;; "commando" goes here. Hacks and glory await!

(defvar *command-output* (make-synonym-stream '*standard-output*))
(defvar *command-error-output* (make-synonym-stream '*error-output*))

;;; FIXME: Should be a generic function.
(defun stringify-command-argument (argument)
  "Convert ARGUMENT to a string suitable for passing to RUN."
  (typecase argument
    (string argument)
    (pathname (native-namestring argument))
    (keyword (format nil "--~(~A~)" argument))
    (t (princ-to-string argument))))

(defun run (command &rest arguments)
  "Run shell-command COMMAND with ARGUMENTS as arguments. Searches the
PATH environment for the right command to run. Arguments are converted
to strings with STRINGIFY-COMMAND-ARGUMENT. If the command exits with
nonzero status, signals an error."
  (let ((process (run-program command
                              (mapcar #'stringify-command-argument
                                      (flatten arguments))
                              :search t
                              :wait t
                              :error *command-error-output*
                              :output *command-output*)))
    (let ((code (process-exit-code process)))
      (if (zerop code)
          t
          ;; FIXME: Raise a proper condition that can be handled.
          (error "Command exited with non-zero status ~D" code)))))

(defmacro with-run-output ((stream (command &rest args)) &body body)
  "Bind STREAM to the output stream of COMMAND and evaluate BODY."
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
  "Evaluate BODY in an environment that binds *COMMAND-OUTPUT* to a
binary output stream."
  `(with-open-file (*command-output* ,pathname :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
     ,@body))

(defmacro without-run-output (&body body)
  "Evaluates BODY in an environment that discards all command output."
  `(let ((*command-output* nil))
     ,@body))

(defun run-output-lines (command &rest args)
  "Return the output of COMMAND as a list of one string per line."
  (let ((output (with-output-to-string (*command-output*)
                  (apply #'run command args))))
    (with-input-from-string (stream output)
      (loop for line = (read-line stream nil)
            while line collect line))))


;;; Temporary directory work

(defvar *default-temporary-template*
  #p"/tmp/commando/"
  "This directory is used as the basis of IN-TEMPORARY-DIRECTORY.")

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

(defun call-with-temporary-directory (template-pathname fun)
  "Call FUN with one argument, a temporary directory that is
unconditionally deleted when FUN returns, either normally or via a
non-local exit."
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
                      (funcall fun path)
                   (ignore-errors (run "rm" "-rf" (native path)))))
             (sb-posix:syscall-error (condition)
               (when (= (sb-posix:syscall-errno condition)
                        sb-posix:eexist)
                 (go retry))
               (error condition))))))))

(defmacro with-temporary-directory ((var &optional
                                         (template-pathname
                                          '*default-temporary-template*))
                                    &body body)
  "Macro-ized version of CALL-WITH-TEMPORARY-DIRECTORY."
  `(call-with-temporary-directory ,template-pathname (lambda (,var) ,@body)))

(defun call-in-temporary-directory (template-pathname fun)
  "Call FUN with the POSIX cwd and *DEFAULT-PATHNAME-DEFAULTS* set to
a temporary directory that is unconditionally deleted when FUN
returns, either normally or via a non-local exit."
  (call-with-temporary-directory
   template-pathname
   (lambda (path)
     (with-posix-cwd path
       (funcall fun)))))

(defmacro in-temporary-directory (&body body)
  "Macro-ized version of CALL-IN-TEMPORARY-DIRECTORY."
  `(call-in-temporary-directory *default-temporary-template*
                                (lambda () ,@body)))

(defmacro in-specific-temporary-directory (template-pathname &body body)
  `(call-in-temporary-directory ,template-pathname
                                (lambda () ,@body)))

(defun copy-file (from to)
  "Copy the file FROM to the file TO."
  (run "cp" (native (truename from)) (native to)))
