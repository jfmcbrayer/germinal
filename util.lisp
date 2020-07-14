;;;; Utility functions for the Germinal server

(in-package :germinal)

(defun de-prefix (path &optional (prefix *germinal-root*))
  "Strip *germinal-root* from a pathname"
  (str:replace-all prefix "" path))

(defun vector-to-list (the-vector)
  (loop for i below (array-dimension the-vector 0)
        collect (aref the-vector i)))

;; From Reddit /u/ambrevar
;; https://www.reddit.com/r/lisp/comments/c3nfzo/humanreadable_file_size_in_lisp/
;; This is mostly inspired by Emacs 26.2.
(defun file-size-human-readable (file-size &optional flavor)
  "Produce a string showing FILE-SIZE in human-readable form.
Optional second argument FLAVOR controls the units and the display format:
 If FLAVOR is nil or omitted, each kilobyte is 1024 bytes and the produced
    suffixes are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `si', each kilobyte is 1000 bytes and the produced suffixes
    are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `iec', each kilobyte is 1024 bytes and the produced suffixes
    are \"KiB\", \"MiB\", \"GiB\", \"TiB\", etc."
  (let ((power (if (or (null flavor) (eq flavor 'iec))
                   1024.0
                   1000.0))
        (post-fixes
          ;; none, kilo, mega, giga, tera, peta, exa, zetta, yotta
          (list "" "k" "M" "G" "T" "P" "E" "Z" "Y"))
        (format-string "~d~a~a"))
    (loop while (and (>= file-size power) (rest post-fixes))
          do (setf file-size (/ file-size power)
                   post-fixes (rest post-fixes)))
    (if (> (abs (- file-size (round file-size))) 0.05)
        (setf format-string "~,1f~a~a")
        (setf file-size (round file-size)))
    (format nil format-string
            file-size
            (if (and (eq flavor 'iec) (string= (first post-fixes) "k"))
                "K"
                (first post-fixes))
            (cond
              ((and (eq flavor 'iec)
                    (string= (first post-fixes) ""))
               "B")
              ((eq flavor 'iec) "iB")
              (t "")))))
