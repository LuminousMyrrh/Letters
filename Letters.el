;;; Letters.el --- ASCII Banner LETTERS -*- lexical-binding: t; -*-

(defvar +banner--width 80)
(defun +banner--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

;; Define a list of colors
(defun random-color ()
  "Generate a random hex color code that is not too dark."
  (let ((r (+ (random 100) (random 120)))
        (g (+ (random 100) (random 120)))
        (b (+ (random 100) (random 120))))
    (format "#%02X%02X%02X" r g b)))

(defun get-loaded-packages-count ()
  "Return the count of currently loaded packages."
  (length (mapcar #'symbol-name (mapcar #'car package-alist))))

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
         ;; 3 makes it higher, 2 makes it perfectly in then middle
         (padding-top (max 0 (floor (/ (- (window-height) banner-height) 3))))
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

    (let ((loaded-packages (get-loaded-packages-count)))
      (insert padding-string "\n")
      (insert (+banner--center current-width (propertize (format "Loaded Packages: %d" loaded-packages) 'face `(:foreground "orange" :height 0.8))) "\n"))
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
