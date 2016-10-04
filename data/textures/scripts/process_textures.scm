;; Simple GIMP scripts to process all textures:
;; - into normal maps inside normal_maps subdirectories (to test bump mapping)
;; - into compressed DDS images (to test DDS compression)
;;
;; This is the first GIMP batch mode script of Kambi, thanks for useful
;; [http://www.gimp.org/tutorials/Basic_Batch/].
;;
;; ---------------------------------------------------------------------------
;;   Copyright 2007,2009 Michalis Kamburelis.
;;
;;   This file is part of "castle".
;;
;;   "castle" is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   "castle" is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with "castle"; if not, write to the Free Software
;;   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
;;

;;;; Filename functions ------------------------------------------------------

;; Removes extension part of the FILENAME (".ext" part).
(define (kam-file-name-delete-ext filename)

  ;; * I swear I'm going to stab in the face next guy that tries to persuade
  ;;   me that Lisp syntax is readable. *
  ;;
  ;; This cludge below means (in normal, human-readable language:)
  ;;
  ;; for i := length(filename) - 1 downto 0 do
  ;;   if filename[i] = '.' then
  ;;     Break;

  (let* ((len (string-length filename))
         (i (- len 1)))
     (while (and (>= i 0)
                 (not (char=? (string-ref filename i) #\.)))
       (set! i (- i 1)))

    ;; We're here, so i < 0 (no '.' in filename) or filename[i] = '.'
    (if (< i 0)
        filename
      (substring filename 0 i)))
)

;; Replaces extension part of the FILENAME (".ext" part) with NEW-EXTENSION.
;; In the usual case, you want to include trailing dot in NEW-EXTENSION.
;; If filename has no extension part, just adds NEW-EXTENSION to it.
(define (kam-file-name-change-ext filename new-extension)
  (string-append (kam-file-name-delete-ext filename) new-extension))

;; Assuming FILENAME is a nice file name, inserts another subdirectory name
;; at the end of it's path. (Note: assumes path delimiter is '/',
;; i.e. Unix-like; I don't know how GIMP filenames behave on Windows).
(define (kam-file-name-insert-subdir filename subdir)
  (let* ((len (string-length filename))
         (i (- len 1)))
     (while (and (>= i 0)
                 (not (char=? (string-ref filename i) #\/)))
       (set! i (- i 1)))

    ;; We're here, so i < 0 (no '/' in filename) or filename[i] = '.'
    (if (< i 0)
        (string-append subdir "/" filename)
      (string-append (substring filename 0 i)
                     "/" subdir
                     (substring filename i len))))
)

;;;; normal map generation -----------------------------------------------------

(define (kam-normalmap input-filename)
  (let* ((output-filename
          ;; Place output in normal_maps subdir, and always in PNG format.
          (kam-file-name-change-ext
            (kam-file-name-insert-subdir input-filename "normal_maps") ".png"))

         (image (car (gimp-file-load RUN-NONINTERACTIVE
                                         input-filename input-filename)))
         (drawable (car (gimp-image-get-active-layer image))))

    ;; tests:
    ;; (gimp-message (string-append input-filename " goes to " output-filename))

    (plug-in-normalmap RUN-NONINTERACTIVE
      image drawable
        0 0.0 60.0
        1 ;; wrap true
        0 0 0 0 0
        1 ;; y-invert true
        0 0.0
        drawable)

    (gimp-file-save RUN-NONINTERACTIVE
      image drawable output-filename output-filename)
    (gimp-image-delete image))
)

(define (kam-batch-normalmap)

  (let* ((filelist (cadr (file-glob "../*.png" 1))))
    (while (not (null? filelist))
       (let* ((filename (car filelist)))
         (kam-normalmap filename)
         (set! filelist (cdr filelist)))))

  (let* ((filelist (cadr (file-glob "../*.jpg" 1))))
    (while (not (null? filelist))
      (let* ((filename (car filelist)))
        (kam-normalmap filename)
        (set! filelist (cdr filelist)))))

  (let* ((filelist (cadr (file-glob "../doom/*.png" 1))))
    (while (not (null? filelist))
      (let* ((filename (car filelist)))
        (kam-normalmap filename)
        (set! filelist (cdr filelist)))))
)

;;;; dds generation -----------------------------------------------------

(define (kam-dds input-filename)
  (let* ((output-filename
          ;; Place output always in .dds format.
          (kam-file-name-change-ext input-filename ".dds"))

         (image (car (gimp-file-load RUN-NONINTERACTIVE
                                         input-filename input-filename)))
         (drawable (car (gimp-image-get-active-layer image))))

    ;; tests:
    (gimp-message (string-append input-filename " goes to " output-filename))

    ;; Save with default settings:
    ;; (gimp-file-save RUN-NONINTERACTIVE
    ;;   image drawable output-filename output-filename)

    ;; No longer wanted, our engine handles compressed DDS without
    ;; the ugly need for inversion.
    ;; (gimp-image-flip image 1)

    (file-dds-save RUN-NONINTERACTIVE
      image drawable output-filename output-filename
      ;; 1 = DXT1
      1
      ;; gen mipmaps
      0
      0 0 -1 0 0 0)

    (gimp-image-delete image))
)

;; globs is a list of strings = globs to match input filenames to process
(define (kam-batch-dds-globs globs)

  (while (not (null? globs))

    (let* ((filelist (cadr (file-glob (car globs) 1))))
      (while (not (null? filelist))
         (let* ((filename (car filelist)))
           (kam-dds filename)
           (set! filelist (cdr filelist)))))

    (set! globs (cdr globs))))


(define (kam-batch-dds)
  (kam-batch-dds-globs '("../*.png"  "../*.jpg"  "../doom/*.png")))

(define (kam-batch-dds-acts)
  (kam-batch-dds-globs '(
    "/home/michalis/3dmodels/tremulous-vrml/kambifun/textures/atcs/*.png"
    "/home/michalis/3dmodels/tremulous-vrml/kambifun/textures/atcs/*.jpg"
    "/home/michalis/3dmodels/tremulous-vrml/kambifun/textures/atcs/*.tga"
    "/home/michalis/3dmodels/tremulous-vrml/kambifun/textures/atcs/news/*.tga"
    )))
