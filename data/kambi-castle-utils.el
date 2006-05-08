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

(defun kam-blender-texture2-node-regexp (texture-filename-regexp)
  "Texture2 VRML 1.0 node as written by Blender exporter."
  (concat
  "Texture2 {
			filename \\(" texture-filename-regexp "\\)
			wrapS \\(.+\\)
			wrapT \\(.+\\)
		}"))

(defun kam-blender-texture2-node-regexp-replacement (replacement-num)
  (concat
    "Texture2 {
			filename \\" (number-to-string replacement-num) "
			wrapS \\" (number-to-string (+ 1 replacement-num)) "
			wrapT \\" (number-to-string (+ 2 replacement-num)) "
		}"))

(defun kam-blender-mesh-node-start-regexp (mesh-name-regexp)
  (concat "DEF \\(" mesh-name-regexp "\\)
	Separator {
		"))

(defun kam-blender-mesh-node-start-regexp-replacement (replacement-num)
  (concat
    "DEF \\" (number-to-string replacement-num) "
	Separator {
		"))

(defun kam-remove-vertex-col-material-one-mesh (mesh-name-regexp)
  "When UV mapping was used for the mesh, blender will export this model
with Materal properties taken from vertex painting.
It will do this even if you did not actually use vertex painting.
This means that blender's material values that you set for this
object will be ignored. This function fixes the situation by removing
VRML Material node that corresponds to vertex painting.

In other words: use this when you used UV mapping for your mesh,
but you still want the mesh to use \"normal\" material defined
in blender, not the vertex painting."
  (interactive)
  (kam-simple-re-replace-buffer (concat
    (kam-blender-mesh-node-start-regexp mesh-name-regexp)
    (kam-blender-texture2-node-regexp ".+") "
		Material {
			diffuseColor \\[\\([.0-9 ,\t\n]+\\)\\]
		}
		MaterialBinding { value PER_VERTEX_INDEXED }")
    (concat
      (kam-blender-mesh-node-start-regexp-replacement 1)
      (kam-blender-texture2-node-regexp-replacement 2)))
)

(defun kam-remove-vertex-col-material ()
  "Like `kam-remove-vertex-col-material-one-mesh' but for all
meshes in the model."
  (interactive)
  (kam-simple-re-replace-buffer (concat
    (kam-blender-texture2-node-regexp ".+") "
		Material {
			diffuseColor \\[\\([.0-9 ,\t\n]+\\)\\]
		}
		MaterialBinding { value PER_VERTEX_INDEXED }")
    (kam-blender-texture2-node-regexp-replacement 1))
)

(defun kam-fix-vertex-col-material (mesh-name)
  "When UV mapping was used for the mesh, blender will export this model
with Materal properties taken from vertex painting.
However, Material property will be wrong (PER_VERTEX_INDEXED instead of
PER_VERTEX). This fixes the error.

In other words: use this if you want your vertex painting
on MESH-NAME to be correctly interpreted."
  (interactive)
  (kam-simple-re-replace-buffer (concat
    (kam-blender-mesh-node-start-regexp mesh-name)
    (kam-blender-texture2-node-regexp ".+") "
		Material {
			diffuseColor \\(\\[\\([.0-9 ,\t\n]+\\)\\]\\)
		}
		MaterialBinding { value PER_VERTEX_INDEXED }")
    (concat
      (kam-blender-mesh-node-start-regexp-replacement 1)
      (kam-blender-texture2-node-regexp-replacement 2) "
		Material {
			diffuseColor \\5
		}
		MaterialBinding { value PER_VERTEX }")
  )
)

(defun kam-add-material-for-mesh (mesh-name material-name)
  (kam-simple-replace-buffer
    (concat "DEF " mesh-name "
	Separator {")
    (concat "DEF " mesh-name "
	Separator { USE " material-name))
)

(defun kam-add-for-mesh (mesh-name-regexp vrml-code)
  "Add any VRML code before meshes matching MESH-NAME-REGEXP."
  (interactive)
  (kam-simple-re-replace-buffer
    (kam-blender-mesh-node-start-regexp mesh-name-regexp)
    (concat
      (kam-blender-mesh-node-start-regexp-replacement 1)
      vrml-code))
)

;; Processing specific VRML files ----------------------------------------------

(defun kam-process-castle-hall ()
  (interactive)
  (kam-fix-blender-filename)
  (kam-fix-vertex-col-material "MeshStairsExit")
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

(defun kam-process-gate ()
  (interactive)
  (kam-fix-vertex-col-material "MeshRocks")
  (kam-remove-vertex-col-material-one-mesh "MeshWater")
  (kam-add-material-for-mesh "MeshWater" "MatWater")
  (kam-simple-replace-buffer "DEF MatWater
	Material {" "DEF MatWater
	Material {
          fogImmune TRUE")
  (kam-fix-vertex-col-material "MeshGate")

  ;; I don't know why blender refuses to write here relative paths...
  ;; Yes, I checked "relative paths" button when loading.
  (kam-simple-replace-buffer
    "/win/mojepasy/openGL/castle/trunk/data/textures/"
    "../textures/")

  (write-file "gate_processed.wrl")
)

(defun kam-process-cages ()
  (interactive)
  (kam-fix-vertex-col-material "MeshCage")
  (kam-fix-vertex-col-material "MeshCageKnife")
  (kam-fix-vertex-col-material "MeshCageOnlyDown")
  (kam-fix-vertex-col-material "MeshCageUpSlide")
  (kam-fix-vertex-col-material "MeshStairsEntrance")

  (kam-fix-vertex-col-material "MeshWalls")

  (kam-remove-vertex-col-material-one-mesh "MeshGround")
  (kam-add-material-for-mesh "MeshGround" "MatNormal")

  (kam-remove-vertex-col-material-one-mesh "MeshCageBar")
  (kam-add-material-for-mesh "MeshCageBar" "MatNormal")
  (kam-remove-vertex-col-material-one-mesh "MeshCageBarsKnife")
  (kam-add-material-for-mesh "MeshCageBarsKnife" "MatNormal")
  (kam-remove-vertex-col-material-one-mesh "MeshCageBarsKnife")
  (kam-add-material-for-mesh "MeshCageBarsKnife" "MatNormal")
  (kam-remove-vertex-col-material-one-mesh "MeshCageBarsDown")
  (kam-add-material-for-mesh "MeshCageBarsDown" "MatNormal")
  (kam-remove-vertex-col-material-one-mesh "MeshCageBarsUpSlide")
  (kam-add-material-for-mesh "MeshCageBarsUpSlide" "MatNormal")

  (kam-remove-vertex-col-material-one-mesh "MeshRandomBlock")
  (kam-add-material-for-mesh "MeshRandomBlock" "MatNormal")
  (kam-add-for-mesh "MeshNaturalRockSide" "ShapeHints { creaseAngle 4 }")
  (kam-add-for-mesh "MeshLair" "ShapeHints { creaseAngle 4 }")

  ;; I don't know why blender refuses to write these as relative paths...
  ;; Yes, I checked "relative paths" button when loading.
  (kam-simple-replace-buffer
    "/win/mojepasy/openGL/castle/trunk/data/textures/"
    "../../textures/")

  (write-file "cages_processed.wrl")
)

(defun kam-process-end-sequence ()
  (interactive)
  (kam-remove-vertex-col-material-one-mesh "Water")
  (kam-add-material-for-mesh "Water" "MatWater")

  (kam-simple-replace-buffer
    "/win/mojepasy/openGL/castle/trunk/data/textures/"
    "../textures/")

  (write-file "end_sequence_processed.wrl")
)