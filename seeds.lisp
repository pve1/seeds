(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (make-package :seeds :use '(:cl)))
  (in-package :seeds)
  (setf *readtable* (capitalized-export:make-capitalized-export-readtable)))

(defvar *Seed-directory* nil
  "Newly created seed projects go here.")

(defvar *Seed-template-directory* nil
  "A directory containing quickproject templates.")

(defvar *Depends-on* '()
  "The default dependencies of new projects.")

(defvar *Default-template* nil
  "The default template when creating a new project.")

(defvar *If-exists* :ask
  "What to do if the target project directory exists. One of:

:ASK -> Ask the user how to proceed.
:ERROR -> Signal an error.
NIL -> Proceed silently, possibly overwriting some files.")

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
    (symbol (ensure-suffix "/" (string-downcase (string template))))
    (string (ensure-suffix "/" (string template)))
    (pathname (ensure-suffix "/" (namestring template)))))

(defun Create-seed (name &key (depends-on '(:alexandria))
                              (template *default-template*)
                              load
                              (seed-directory *seed-directory*)
                              (template-directory *seed-template-directory*)
                              (if-exists *if-exists*))
  "Creates a new seed project called NAME in the directory
SEED-DIRECTORY. The project will be created using a template named
TEMPLATE, which should be a sub-directory of the directory
TEMPLATE-DIRECTORY (which can contain many templates). TEMPLATE should
be a valid quickproject template. If TEMPLATE is NIL, the default
quickproject template will be used.

Example:

  (seeds:create-seed \"my-app\" :template :basic)
"
  ;; Check seed-directory
  (setf seed-directory (check-seed-directory seed-directory))
  ;; Use quicklisp default template if none was specified.
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
    ;; Check template-dir
    (unless (probe-file the-template-dir)
      (error "Template directory ~S does not exist." the-template-dir))
    ;; Check target-dir
    (when (probe-file target-dir)
      (case if-exists
        (:ask (y-or-n-p "Target directory ~S already exists, continue?" target-dir))
        (:error (error "Target directory ~S already exists." target-dir))
        (t nil)))
    ;; Go!
    (format t "Using template: ~S~%" template-path)
    (quickproject:make-project (ensure-suffix "/" name)
                               :depends-on depends-on)
    ;; Load the new system?
    (when load
      (funcall (find-symbol "LOAD-SYSTEM" :asdf) name))))
