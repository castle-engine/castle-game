;; This is a file with a couple of Emacs-Lisp functions
;; to do some useful post-processing of some VRML files.
;;
;; "kam-" is my (Michalis Kamburelis) personal prefix.

;; Replacing utilities ---------------------------------------------------------

(defun kam-simple-replace (from-string to-string from-pos
  &optional end-pos fixedcase)
  (save-excursion
    (goto-char from-pos)
    (while (search-forward from-string end-pos t)
      (replace-match to-string fixedcase t)
    ))
)

(defun kam-simple-re-replace (from-string to-string from-pos
  &optional end-pos fixedcase)
  "Replace text in current buffer from FROM-STRING to TO-STRING,
starting from position FROM-POS to END-POS (END-POS = nil means
to the end of buffer).

Current position of cursor is not changed by this function.
Uses regexps. FIXEDCASE has the same meaning as in `replace-match'."
  (save-excursion
    (goto-char from-pos)
    (while (re-search-forward from-string end-pos t)
      (replace-match to-string fixedcase nil)))
)

(defun kam-simple-replace-buffer (from-string to-string)
  "Simple replace in whole buffer. Without asking user."
  (interactive)
  (kam-simple-replace from-string to-string (point-min))
)

(defun kam-simple-re-replace-buffer (from-string to-string)
  "Simple regexp replace in whole buffer. Without asking user."
  (interactive)
  (kam-simple-re-replace from-string to-string (point-min))
)

;; Processing VRML files -------------------------------------------------------

(defun kam-fix-blender-filename ()
  (interactive)
  (kam-simple-replace-buffer
    "filename //../../textures/"
    "filename ../../textures/")

  (kam-simple-replace-buffer
    "filename //../textures/"
    "filename ../textures/")
)

(defun kam-blender-texture2-node-regexp (texture-filename)
  "Texture2 VRML 1.0 node as written by Blender exporter."
  (concat
  "Texture2 {
			filename \\(" texture-filename "\\)
			wrapS \\(.+\\)
			wrapT \\(.+\\)
		}")
)

(defconst kam-blender-texture2-node-regexp-replacement
  "Texture2 {
			filename \\1
			wrapS \\2
			wrapT \\3
		}")

(defun kam-remove-vertex-col-material ()
  (interactive)
  (kam-simple-re-replace-buffer (concat
    (kam-blender-texture2-node-regexp ".+") "
		Material {
			diffuseColor \\[\\([.0-9 ,\t\n]+\\)\\]
		}
		MaterialBinding { value PER_VERTEX_INDEXED }")
    kam-blender-texture2-node-regexp-replacement)
)

(defun kam-add-material-for-mesh (mesh-name material-name)
  (kam-simple-replace-buffer
    (concat "DEF " mesh-name "
	Separator {")
    (concat "DEF " mesh-name "
	Separator { USE " material-name))
)

;; Processing specific VRML files ----------------------------------------------

(defun kam-process-castle-hall ()
  (interactive)
  (kam-fix-blender-filename)
  (kam-remove-vertex-col-material)
  (kam-add-material-for-mesh "MeshDome" "MatUnderTexture")
  (kam-add-material-for-mesh "MeshGround" "MatGround")
  (kam-add-material-for-mesh "MeshGroundDown" "MatGroundDown")
  (kam-add-material-for-mesh "MeshTunnel" "MatUnderTexture")
  (kam-add-material-for-mesh "MeshTunnelAround" "MatUnderTexture")
  (write-file "castle_hall_processed.wrl")
)

(defun kam-process-life-potion ()
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
  (write-file "life_potion_processed.wrl")
)