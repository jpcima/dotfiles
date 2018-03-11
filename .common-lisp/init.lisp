;;; -*- mode: lisp; -*-

#-asdf
(let ((asdf-file (merge-pathnames ".common-lisp/asdf.lisp" (user-homedir-pathname))))
  (when (probe-file asdf-file)
    (load asdf-file)))
(require "ASDF")
(format *error-output* "** Using ASDF ~A~%" asdf/upgrade:*asdf-version*)

#-quicklisp
(let ((quicklisp-init (merge-pathnames ".common-lisp/quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(format *error-output* "** Using Quicklisp ~A~%" ql-info:*version*)

(asdf:initialize-source-registry
 `(:source-registry
   (:directory (:home ,(format nil ".common-lisp/impl-systems/~(~A~)/" (uiop:implementation-type)) :*/))
   (:directory (:home "Documents/Projects/common-lisp/" :*/))
   :inherit-configuration))

#+sbcl
(when (interactive-stream-p *standard-input*)
  (asdf:load-system '#:sbcl-readline)
  (format *error-output* "** Using Readline~%")
  (set (intern "*HISTORY-FILE*" "READLINE")
       (uiop:native-namestring (uiop:null-device-pathname))))
#+ecl
(when (interactive-stream-p *standard-input*)
  (asdf:load-system '#:ecl-readline)
  (funcall (intern "ENABLE" "ECL-READLINE"))
  (format *error-output* "** Using Readline~%"))
