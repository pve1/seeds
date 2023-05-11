(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (make-package :seeds :use '(:cl)))
  (in-package :seeds)
  (setf *readtable* (capitalized-export:make-capitalized-export-readtable)))

(defvar *Seed-directory* nil)
(defvar *Seed-template-directory* nil)
(defvar *Depends-on* '())
(defvar *Default-template* nil)
(defvar *If-exists* :ask)

(defun ensure-suffix (suffix string)
  (if (alexandria:ends-with-subseq suffix string)
      string
      (concatenate 'string string suffix)))

(defun check-seed-directory (seed-directory)
  (if seed-directory
      seed-directory
      (progn (cerror "Use ~S instead."
                     "*SEED-DIRECTORY* has not been specified."
                     *default-pathname-defaults*)
             *default-pathname-defaults*)))

(defun interpret-template-to-path (template)
  (etypecase template
    (symbol (concatenate 'string
                         (string-downcase (string template))
                         "/"))
    (string (ensure-suffix "/" (string template)))
    (pathname (ensure-suffix "/" (namestring template)))))

(defun Create-seed (name &key (depends-on '(:alexandria))
                              (template *default-template*)
                              load
                              (seed-directory *seed-directory*)
                              (template-directory *seed-template-directory*)
                              (if-exists *if-exists*))
  (setf seed-directory (check-seed-directory seed-directory))
  (when (null template)
    (setf template quickproject:*template-directory*
          template-directory #P""))
  (let* ((template-path (interpret-template-to-path template))
         (the-template-dir (merge-pathnames template-path
                                            template-directory))
         (target-dir (merge-pathnames (ensure-suffix "/" name)
                                      seed-directory))
         (parameter-function (lambda ()
                               (list :dependencies-string-no-list
                                     (format nil "~{#:~(~a~)~^ ~}" depends-on))))
         (quickproject:*template-parameter-functions*
           (cons parameter-function quickproject:*template-parameter-functions*))
         (quickproject:*template-directory* the-template-dir)
         (*default-pathname-defaults* (pathname seed-directory)))
    (unless (probe-file the-template-dir)
      (error "Template directory ~S does not exist." the-template-dir))
    (when (probe-file target-dir)
      (case if-exists
        (:ask (y-or-n-p "Target directory ~S already exists, continue?" target-dir))
        (:error (error "Target directory ~S already exists." target-dir))
        (t nil)))
    (format t "Using template: ~S~%" template-path)
    (quickproject:make-project (ensure-suffix "/" name)
                               :depends-on depends-on)
    (when load
      (funcall (find-symbol "LOAD-SYSTEM" :asdf) name))))
