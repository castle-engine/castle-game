;; This is a file with a couple of Emacs-Lisp functions
;; to do some useful post-processing of some VRML files.

(defun kam-simple-replace (from-string to-string from-pos
  &optional end-pos fixedcase)
  (save-excursion
    (goto-char from-pos)
    (while (search-forward from-string end-pos t)
      (replace-match to-string fixedcase t)
    ))
)

(defun kam-simple-replace-buffer (from-string to-string)
  "Simple replace in whole buffer. Without asking user."
  (interactive)
  (kam-simple-replace from-string to-string (point-min))
)

(defun kam-fix-blender-filename ()
  (interactive)
  (kam-simple-replace-buffer
    "filename //../../textures/"
    "filename ../../textures/")
)

(defun kam-process-flask-red ()
  (interactive)

  (kam-simple-replace-buffer "Material {
			diffuseColor [
				 1.000000 1.000000 1.000000,
				 1.000000 1.000000 1.000000,
				 1.000000 1.000000 1.000000,
				 1.000000 1.000000 1.000000,
				 1.000000 1.000000 1.000000,
				 1.000000 1.000000 1.000000,
				 1.000000 1.000000 1.000000,
				 1.000000 1.000000 1.000000,
			]
		}
		MaterialBinding { value PER_VERTEX_INDEXED }"
                "USE MatInside")
  (kam-fix-blender-filename)
  (write-file "flask_red_processed.wrl")
)