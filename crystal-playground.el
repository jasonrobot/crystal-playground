;;; crystal-playground.el --- Local crystal playground for short code snippets.

;; Copyright (C) 2018  Jason Howell (jasonrobot)

;; Author: 
;; URL: 
;; Version: 0.1
;; Keywords: tools, crystal
;; Package-Requires: ((emacs "24.3") (crystal-mode "0.1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; It is port of github.com/grafov/rust-playground for Crystal language.

;;; Code:

(require 'compile)
(require 'time-stamp)

(defgroup crystal-playground nil
  "Options for crystal-playground."
  :group 'crystal-mode)

(defcustom crystal-playground-basedir
  (locate-user-emacs-file "crystal-playground")
  "Base directory for the crystal playground."
  :type 'file
  :group 'crystal-playground)

(defcustom crystal-playground-run-command
  "crystal run playground.cr"
  "Command used to run the playground."
  :type 'string
  :group 'crystal-playground)

(defcustom crystal-playground-main-template
  "require \"./playground/*\"

module Playground
  def self.main
    # TODO: Put your code here
    
  end
end

puts \"Result: %s\" % Playground.main"
  "When creating a new playground, this will be used as the playground.cr file"
  :type 'string
  :group 'crystal-playground)

(defcustom crystal-playground-source-header-comment
  ""
  "Header comment that goes in the source file with instructions."
  :type 'string
  :group 'crystal-playground)

;; TODO keymap
(define-minor-mode crystal-playground-mode
  "A place to play with crystal!"
  :init-value nil
  :lighter "Play(Crystal)")

(defun crystal-playground-get-current-basedir (&optional path)
  "Get the path of the dir containing this playground.

Start from PATH or the path of the current buffer's file.  Returns 
the path to the basedir or NIL if this is not a snippet."
  (unless path
    (setq path (buffer-file-name)))
  ;; need this check incase buffer isn't visiting a file
  (if (not path)
      nil
    ;; descend recursively
    (if (not (string= path "/"))
        (let ((base (expand-file-name crystal-playground-basedir))
              (path-parent (file-name-directory (directory-file-name path))))
          (if (string= (file-name-as-directory base)
                       (file-name-as-directory path-parent))
              path
            (crystal-playground-get-current-basedir path-parent)))
      nil)))

(defmacro in-crystal-playground (&rest forms)
  "Execute FORMS if current buffer is part of a playground.
Otherwise message the user that they aren't in one."
  `(if (not (crystal-playground-get-current-basedir))
       (message "You aren't in a Crystal playground.")
     ,@forms))

;;TODO make basedir optional, default to a defcustom
(defun crystal-playground-make-new (basedir)
  "Create a new crystal project in BASEDIR using the 'crystal' command."
  ;; use `crystal` command to fill out the snippet
  ;; (shell-command "crystal init app playground"))
  ;;make a name from a datestamp
  (let ((playground-dir (expand-file-name
                         (file-name-as-directory
                          (concat
                           (file-name-as-directory basedir)
                           (time-stamp-string "at-%:y-%02m-%02d-%02H%02M%02S"))))))
    ;; this makes its target dir automatically.
    ;; in fact it wont work if the dir exists
    (call-process "crystal" nil nil nil "init" "app" "playground" playground-dir)
    ;;return the dir
    playground-dir))

(defun crystal-playground-get-main-cr (basedir)
  "Get the path to the main .cr file in the playground at BASEDIR."
  (concat basedir (file-name-as-directory "src") "playground.cr"))

(defun crystal-playground-get-shard-yml (basedir)
  "Get the path to the main .cr file in the playground at BASEDIR."
  (concat basedir "shard.yml"))

;;;###autoload
(defun crystal-playground ()
  "Start a new crystal playground."
  (interactive)
  (let* ((current-playground-dir
          (crystal-playground-make-new crystal-playground-basedir))
         (main-cr (crystal-playground-get-main-cr current-playground-dir))
         (shard-yml (crystal-playground-get-shard-yml current-playground-dir)))
    ;; open the main file
    (find-file main-cr)
    ;; set all the modes we need
    (crystal-playground-mode)
    ;; just making my own rather than modifying what exists
    (erase-buffer)
    ;; insert the template header
    (insert crystal-playground-source-header-comment)
    (insert crystal-playground-main-template)
    ;; put the point in a nice place
    (forward-line -4)
    (end-of-line)))

(defun crystal-playground-exec ()
  "Run the code of the playground."
  (interactive)
  (in-crystal-playground
   (save-buffer t)
   (compile crystal-playground-run-command)))

;;;###autoload
(defun crystal-playground-rm ()
  "Remove files of the current snippet together with directory of this snippet."
  (interactive)
  (in-crystal-playground
   (let ((current-basedir (crystal-playground-get-current-basedir)))
     (if current-basedir
         (when (or (not crystal-playground-confirm-deletion)
                   (y-or-n-p (format "Do you want delete whole dir %s? "
                                     current-basedir)))
           ;; (dolist (buf (crystal-playground-get-all-buffers))
           ;;   (kill-buffer buf))
           (delete-directory current-basedir t t))
       (message "Won't delete this! Because %s is not under the path %s. Remove the snippet manually!"
                (buffer-file-name)
                crystal-playground-basedir)))))

(provide 'crystal-playground)
;;; crystal-playground.el ends here
