;;; rcirc-color.el --- color nicks

;; Copyright (C) 2005-2013 Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Version: 0.2
;; Package-Version: 20151130.758
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Use /COLOR to list all colored nicks with their color
;; Use /COLOR NICK COLOR to color NICK using COLOR

;;; Code:

(require 'rcirc)

(defun rcirc-color-distance (color1 color2)
  "Compute the difference between two colors
using the weighted Euclidean distance formula proposed on
<http://www.compuphase.com/cmetric.htm>.
Remember that every component for the formula is in the range of 0-xFF
and `color-values' will return a range of 0-FFFF. Thus, divide everything
by 256. This also helps preventing integer overflow."
  (let* ((dr (/ (- (nth 0 (color-values color1))
		   (nth 0 (color-values color2))) 256))
	 (dg (/ (- (nth 1 (color-values color1))
		   (nth 1 (color-values color2))) 256))
	 (db (/ (- (nth 2 (color-values color1))
		   (nth 2 (color-values color2))) 256))
	 (red-mean (/ (+ (nth 0 (color-values color1))
			 (nth 0 (color-values color2)))
		      2 256)))
    (sqrt (+ (ash (* (+ 512 red-mean) dr dr) -8)
	     (* 4 dg dg)
	     (ash (* (- 767 red-mean) db db) -8)))))

(defvar rcirc-colors
  (let ((min-distance 200); heuristics
	(bg (face-background 'default))
	(fg (face-foreground 'rcirc-my-nick))
	candidates)
    (dolist (item color-name-rgb-alist)
      (let ((color (car item)))
	(when (and (not (color-gray-p color))
		   (> (rcirc-color-distance color bg) min-distance)
		   (> (rcirc-color-distance color fg) min-distance))
	  (setq candidates (cons color candidates)))))
    candidates)
  "Colors to use for nicks in rcirc.
By default, all the non-grey colors that are very different from
the default background are candidates.  This uses `rcirc-color-distance'
to compute distance between colors.

To check out the list, evaluate (list-colors-display rcirc-colors).")

(defvar rcirc-color-mapping (make-hash-table :test 'equal)
  "Hash-map mapping nicks to color names.")

(defvar rcirc-color-is-deterministic nil
  "Normally rcirc just assigns random colors to nicks.
These colors are based on the list in `rcirc-colors'.
If you set this variable to a non-nil value, an md5 hash is
computed based on the nickname and the first twelve bytes are
used to determine the color: #rrrrggggbbbb.")

(defadvice rcirc-facify (before rcirc-facify-colors last activate)
  "Add colors to other nicks based on `rcirc-colors'."
  (when (and (eq face 'rcirc-other-nick)
             (not (string= string "")))
    (let ((cell (gethash string rcirc-color-mapping)))
      (unless cell
	(setq cell (cons 'foreground-color
			 (if rcirc-color-is-deterministic
			     (concat "#" (substring (md5 string) 0 12))
			   (elt rcirc-colors (random (length rcirc-colors))))))
        (puthash (substring-no-properties string) cell rcirc-color-mapping))
      (setq face (list cell)))))

(defun rcirc-markup-nick-colors (sender response)
  (with-syntax-table rcirc-nick-syntax-table
    (while (re-search-forward "\\w+" nil t)
      (let ((face (gethash (match-string-no-properties 0) rcirc-color-mapping)))
	(when face
	  (rcirc-add-face (match-beginning 0) (match-end 0) face))))))

(add-to-list 'rcirc-markup-text-functions 'rcirc-markup-nick-colors)

(defun-rcirc-command color (args)
  "Change one of the nick colors."
  (interactive)
  (setq args (split-string args))
  (rcirc-do-color (car args) (cadr args) process target))

(defun rcirc-do-color (nick color process target)
  "Implement /COLOR."
  (if (not nick)
      (let (names)
        (maphash (lambda (key value)
                   (add-text-properties
                    0 (length key)
                    `(face (,value) help-echo ,(cdr value))
                    key)
                   (setq names (cons key names)))
                 rcirc-color-mapping)
        (rcirc-print process (rcirc-nick process) "NOTICE" target
                     (mapconcat 'identity names " ")))
    (unless color
      (error "Use what color?"))
    (puthash nick (cons 'foreground-color color) rcirc-color-mapping)))

(defadvice rcirc-handler-NICK (before rcirc-handler-NICK-colors activate)
  "Update colors in `rcirc-color-mapping'."
  (let* ((old-nick (rcirc-user-nick sender))
         (cell (gethash old-nick rcirc-color-mapping))
         (new-nick (car args)))
    ;; don't delete the old mapping
    (when cell
      (puthash new-nick cell rcirc-color-mapping))))

;;; --- Alternative ----------------------------------------------------

;; An alternate method proposed here:
;; http://defanor.uberspace.net/notes/random-foreground-color.html

;; Let's not confuse Circe users. Only define this stuff when it
;; hasn't been defined.

(when (not (fboundp 'circe-w3-contrast-generate-contrast-color))
  
  (defsubst circe-w3-contrast-c-to-l (c)
    (if (<= c 0.03928)
	(/ c 12.92)
      (expt (/ (+ c 0.055) 1.055) 2.4)))

  (defsubst circe-w3-contrast-relative-luminance (rgb)
    (apply '+
	   (cl-mapcar (lambda (color coefficient)
			(* coefficient
			   (circe-w3-contrast-c-to-l color)))
		      rgb
		      '(0.2126 0.7152 0.0722))))

  (defsubst circe-w3-contrast-contrast-ratio (color1 color2)
    (let ((l1 (+ 0.05 (circe-w3-contrast-relative-luminance color1)))
	  (l2 (+ 0.05 (circe-w3-contrast-relative-luminance color2))))
      (if (> l1 l2)
	  (/ l1 l2)
        (/ l2 l1))))

  (defsubst circe-w3-contrast-rand ()
    (/ (random 42001) 42000.0))

  (defsubst circe-w3-contrast-l-to-c (m)
    (if (<= m (/ 0.03928 12.92))
	(* m 12.92)
      (- (* (expt m (/ 1 2.4))
            1.055)
         0.055)))

  (defsubst circe-w3-contrast-nn (n)
    (cond ((< n 0) 0)
	  ((> n 1) 1)
	  (t n)))

  (defsubst circe-w3-contrast-color-with-luminance-higher-than (N)
    (let* ((Rc 0.2126)
	   (Gc 0.7152)
	   (Bc 0.0722)

	   (R-min-lum (circe-w3-contrast-nn (/ (- N Gc Bc) Rc)))
	   (R-min-color (circe-w3-contrast-l-to-c R-min-lum))
	   (R-color (+ R-min-color (* (circe-w3-contrast-rand) (- 1 R-min-color))))
	   (R-lum (* Rc (circe-w3-contrast-c-to-l R-color)))

	   (G-min-lum (circe-w3-contrast-nn (/ (- N R-lum Bc) Gc)))
	   (G-min-color (circe-w3-contrast-l-to-c G-min-lum))
	   (G-color (+ G-min-color (* (circe-w3-contrast-rand) (- 1 G-min-color))))
	   (G-lum (* Gc (circe-w3-contrast-c-to-l G-color)))

	   (B-min-lum (circe-w3-contrast-nn (/ (- N R-lum G-lum) Bc)))
	   (B-min-color (circe-w3-contrast-l-to-c B-min-lum))
	   (B-color (+ B-min-color (* (circe-w3-contrast-rand) (- 1 B-min-color))))
	   (B-lum (* Bc (circe-w3-contrast-c-to-l B-color))))
      (list R-color G-color B-color)))

  (defsubst circe-w3-contrast-color-with-luminance-lower-than (N)
    (let* ((Rc 0.2126)
	   (Gc 0.7152)
	   (Bc 0.0722)

	   (R-max-lum (circe-w3-contrast-nn (/ N Rc)))
	   (R-max-color (circe-w3-contrast-l-to-c R-max-lum))
	   (R-color (* R-max-color (circe-w3-contrast-rand)))
	   (R-lum (* Rc (circe-w3-contrast-c-to-l R-color)))

	   (G-max-lum (circe-w3-contrast-nn (/ (- N R-lum) Gc)))
	   (G-max-color (circe-w3-contrast-l-to-c G-max-lum))
	   (G-color (* G-max-color (circe-w3-contrast-rand)))
	   (G-lum (* Gc (circe-w3-contrast-c-to-l G-color)))

	   (B-max-lum (circe-w3-contrast-nn (/ (- N R-lum G-lum) Bc)))
	   (B-max-color (circe-w3-contrast-l-to-c B-max-lum))
	   (B-color (* B-max-color (circe-w3-contrast-rand)))
	   (B-lum (* Bc (circe-w3-contrast-c-to-l B-color))))
      (list R-color G-color B-color)))

  (defsubst circe-w3-contrast-generate-contrast-color (color ratio)
    (let ((color-lum (circe-w3-contrast-relative-luminance color)))
      (if (< color-lum (- (/ 1.0 ratio) 0.05))
	  (circe-w3-contrast-color-with-luminance-higher-than (+ (* (+ color-lum 0.05) ratio) 0.05))
        (circe-w3-contrast-color-with-luminance-lower-than (- (/ (+ color-lum 0.05) ratio) 0.05))))))

(defun rcirc-colors-reset-using-g18 ()
  "This resets the random colors used.
`rcirc-colors' is set to 1000 random colors with a contrast ratio
of 4.5 compared to the default background color. If you call this
function, you might want to reset `rcirc-color-mapping' as well."
  (interactive)
  (setq rcirc-colors nil)
  (dotimes (n 1000)
    (let ((rgb (circe-w3-contrast-generate-contrast-color
		(face-background 'default) 7)))
      (setq rcirc-colors (cons (apply 'color-rgb-to-hex rgb)
			       rcirc-colors)))))

;; Test the colors generated:
;; (rcirc-colors-reset-using-g18)
;; (list-colors-display rcirc-colors)

(provide 'rcirc-color)

;;; rcirc-color.el ends here
