;;; Letters.el --- ASCII Banner LETTERS -*- lexical-binding: t; -*-

(defvar +banner--width 80)
(defun +banner--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

;; Define a list of colors
(defun random-color ()
  "Generate a random hex color code that is not too dark."
  (let ((r (+ (random 100) (random 100)))
        (g (+ (random 100) (random 100)))
        (b (+ (random 100) (random 100))))
    (format "#%02X%02X%02X" r g b)))

(defun draw-ascii-banner-fn ()
  (let* ((banner
          '("     ___           ___           ___           ___           ___ "
            "    /\\  \\         /\\  \\         /\\  \\         /\\  \\         /\\  \\ "
            "   /::\\  \\       /||\\  \\       /::\\  \\       /::\\  \\       /||\\  \\ "
            "  /:/\\:\\  \\     /|/\\|\\  \\     /:/\\:\\  \\     /:/\\:\\  \\     /|/\\|\\  \\ "
            " /::\\~\\:\\  \\   /|/ /||\\  \\   /::\\~\\:\\  \\   /:/  \\:\\  \\   _\\|\\~\\|\\  \\ "
            "/:/\\:\\ \\:\\__\\ /|/ /|/\\|\\__\\ /:/\\:\\ \\:\\__\\ /:/__/ \\:\\__\\ /\\ \\|\\ \\|\\__\\ "
            "\\:\\~\\:\\ \\/__/ \\/_/|/ /|/  / \\/__\\:\\/:/  / \\:\\  \\  \\/__/ \\|\\ \\|\\ \\/__/ "
            " \\:\\ \\:\\__\\     /|/ /|/  /       \\::/  /   \\:\\  \\        \\|\\ \\|\\__\\  "
            "  \\:\\ \\/__/     \\/_/|/  /        /:/  /     \\:\\  \\        \\|\\/|/  / "
            "   \\:\\__\\         /|/  /        /:/  /       \\:\\__\\        \\||/  / "
            "    \\/__/         \\/__/         \\/__/         \\/__/         \\/__/ "))
         (longest-line (apply #'max (mapcar #'length banner)))
         (current-width (window-width))
         (banner-height (length banner))
         (padding-top (max 0 (floor (/ (- (window-height) banner-height) 2))))
         (padding-string (make-string longest-line ?\s)))
    (let ((inhibit-read-only t) (color (random-color)))
      (erase-buffer)
      ;; Add top padding
      (dotimes (_ padding-top)
        (insert padding-string "\n"))
      (dolist (line banner)
        (let ((colored-line (propertize line 'face `(:foreground ,color))))
          (insert (+banner--center current-width
                  (concat colored-line (make-string (max 0 (- longest-line (length line))) 32))) "\n"))))
    (read-only-mode 1)))

(defun setup-ascii-banner ()
  "Setup the ASCII banner and hook it to window resizing."
  (lambda ()
    (let ((buffer (get-buffer-create "*Letters*")))
      (with-current-buffer buffer
        (draw-ascii-banner-fn))
      buffer)
    (switch-to-buffer "*Letters*")))

(provide 'ascii-banner)

;;; Letters.el ends here
