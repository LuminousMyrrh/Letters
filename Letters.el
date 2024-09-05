;;; Letters.el --- ASCII Banner LETTERS -*- lexical-binding: t; -*-

(defvar +banner--width 80)
(defun +banner--center (len s)
  (let ((padding (max 0 (/ (- len (length s)) 2))))
    (concat (make-string padding ? ) s)))

(defvar +banner--top-pos 3
  "2 - Perfect center; 3 - A bit higher from the buffer center.")

(defun get-loaded-packages-count ()
  "Return the count of currently loaded packages."
  (length (mapcar #'symbol-name (mapcar #'car package-alist))))

(defun get-not-installed-packages-count ()
  "Return the count of available packages that are not installed."
  (let ((available-packages (mapcar #'car package-alist))
        (all-packages (mapcar #'car package-archive-contents)))
    (length (cl-set-difference all-packages available-packages))))

(defun random-color ()
  "Generate a random hex color code that is not too dark."
  (let ((r (+ (random 256) 64))
        (g (+ (random 256) 64))
        (b (+ (random 256) 64)))
    (format "#%02X%02X%02X" r g b)))

(defvar +separate-banner
  '(
    (letter-e .
              ("     ___      "
               "    /\\  \\     "
               "   /::\\  \\    "
               "  /:/\\:\\  \\   "
               " /::\\~\\:\\  \\  "
               "/:/\\:\\ \\:\\__\\ "
               "\\:\\~\\:\\ \\/__/ "
               " \\:\\ \\:\\__\\   "
               "  \\:\\ \\/__/   "
               "   \\:\\__\\     "
               "    \\/__/     "))

    (letter-m .
              ("     ___      "
               "    /\\  \\     "
               "   /||\\  \\    "
               "  /|/\\|\\  \\   "
               " /|/ /||\\  \\  "
               "/|/ /|/\\|\\__\\ "
               "\\/_/|/ /|/  / "
               "  /|/ /|/  /  "
               "  \\/_/|/  /   "
               "    /|/  /    "
               "    \\/__/     "))

    (letter-a .
              ("     ___      "
               "    /\\  \\     "
               "   /::\\  \\    "
               "  /:/\\:\\  \\   "
               " /::\\~\\:\\  \\  "
               "/:/\\:\\ \\:\\__\\ "
               "\\/__\\:\\/:/  / "
               "     \\::/  /  "
               "     /:/  /   "
               "    /:/  /    "
               "    \\/__/     "))

    (letter-c .
              ("     ___      "
               "    /\\  \\     "
               "   /::\\  \\    "
               "  /:/\\:\\  \\   "
               " /:/  \\:\\  \\  "
               "/:/__/ \\:\\__\\ "
               "\\:\\  \\  \\/__/ "
               " \\:\\  \\       "
               "  \\:\\  \\      "
               "   \\:\\__\\     "
               "    \\/__/     "))

    (letter-s .
              ("     ___      "
               "    /\\  \\     "
               "   /||\\  \\    "
               "  /|/\\|\\  \\   "
               " _\\|\\~\\|\\  \\  "
               "/\\ \\|\\ \\|\\__\\ "
               "\\|\\ \\|\\ \\/__/ "
               " \\|\\ \\|\\__\\   "
               "  \\|\\/|/  /   "
               "   \\||/  /    "
               "    \\/__/     "))
    ) "Banner.")

(defun get-letter-color (letter)
  "Return the color associated with the given LETTER."
  (cond ((eq letter 'letter-e) '(:foreground "#b16286"))
        ((eq letter 'letter-m) '(:foreground "#8f3f71"))
        ((eq letter 'letter-a) '(:foreground "#b16286"))
        ((eq letter 'letter-c) '(:foreground "#b16286"))
        ((eq letter 'letter-s) '(:foreground "#b16286"))
        (t '(:foreground "white"))))

(defun make-banner ()
  "Concatenate the letters with specific colors."
  (let* ((letters '(letter-e letter-m letter-a letter-c letter-s))
         (banner-lines (mapcar (lambda (line-index)
                                 (string-join
                                  (mapcar (lambda (letter)
                                            ;; Get the letter string
                                            (let ((letter-string (nth line-index (cdr (assoc letter +separate-banner)))))
                                              (propertize letter-string 'face (get-letter-color letter))))
                                          letters)
                                  ""))
                               (number-sequence 0 10))))
    banner-lines))

(defun draw-ascii-banner-fn ()
  (let* ((banner (make-banner))
         (longest-line (apply #'max (mapcar #'length banner)))
         (current-width (window-width))
         (banner-height (length banner))

         ;; Adjust padding to be more gentle
         (padding-top (max 0 (floor (/ (- (window-height) banner-height) +banner--top-pos))))
         (padding-string (make-string longest-line ?\s)))
    (let (
          (inhibit-read-only t)
          (loaded-packages (get-loaded-packages-count))
          (not-installed (get-not-installed-packages-count)))
      (erase-buffer)
      ;; Add top padding
      (dotimes (_ padding-top)
        (insert padding-string "\n"))
      (dolist (line banner)
        (insert (+banner--center current-width
                                 (concat line (make-string (max 0 (- longest-line (length line))) 32))) "\n"))

      ;; Reduced number of newlines between sections
      (insert padding-string "\n")
      (insert (+banner--center current-width
                               (propertize (format "Loaded Packages: %d" loaded-packages)
                                           'face `(:foreground "#689d6a" :height 0.8))) "\n")
      (insert padding-string "\n")
      (insert (+banner--center current-width
                               (propertize (format "Not installed Packages: %d" not-installed)
                                           'face `(:foreground "#fe8019" :height 0.8))) "\n"))
    (read-only-mode 1)))

(defun setup-ascii-banner ()
  "Setup the ASCII banner and hook it to window resizing."
  (lambda ()
    (let ((buffer (get-buffer-create "*Letters*")))
      (with-current-buffer buffer
        (draw-ascii-banner-fn))
      buffer)
    (add-hook 'window-size-change-functions #'+banner--resize-handler)
    (switch-to-buffer "*Letters*")))


(defun +banner--resize-handler (_)
  "Redraw the ASCII banner when the window is resized."
  (let ((buffer (get-buffer "*Letters*")))
    (when (and buffer (window-live-p (get-buffer-window buffer)))
      (with-current-buffer buffer
        (draw-ascii-banner-fn)))))

(setq initial-buffer-choice (setup-ascii-banner))

;;; Letters.el ends here
