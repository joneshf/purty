;;; purty.el --- Minor-mode to format PureScript code with purty on save

;; Version: 0.1.0
;; Keywords: purescript format purty
;; Package-Requires: ((reformatter "0.4"))
;; URL: https://gitlab.com/joneshf/purty
;; Author: Gabe Johnson

;; To use this package with `purescript-mode', simply load the file and add a
;; directory local variable in a `.dir-locals.el' file at the root of your
;; project:
;;
;;   ((purescript-mode (mode . purty-on-save)))

;; If you would prefer to install `purty' locally using `npm' or `yarn', set the
;; `purty-use-npm-bin' to true.

(require 'reformatter)

(defgroup purty nil
  "Minor mode for formatting PureScript buffers with purty."
  :group 'languages
  :link '(url-link "https://gitlab.com/joneshf/purty"))

(defcustom purty-command "purty"
  "The name of the `purty` command."
  :group 'purty
  :type 'string)

(reformatter-define purty
  :program (purty-executable-path)
  :args '("-")
  :group 'purty
  :lighter " PurtyFmt")

;; The following is adapted from https://github.com/purescript-emacs/psc-ide-emacs/blob/a10cc85565f330ee277698b27f3f715fef2e1ce2/psc-ide.el

(defcustom purty-use-npm-bin nil
  "Whether to use `npm bin` to determine the location of purty"
  :group 'purty
  :type 'boolean)

(defun purty-executable-path ()
  "Return the full path to the purty executable."
  (let* ((npm-bin-path (if purty-use-npm-bin
                           (purty-npm-bin-executable purty-command)
                         nil)))
    (or npm-bin-path purty-command)))

(defun purty-npm-bin-executable (cmd)
  "Find purty binary CMD of current project by invoking `npm bin`."
  (let* ((npm-bin (s-trim-right (shell-command-to-string "npm bin")))
         (formatter (expand-file-name cmd npm-bin)))
    (if (and formatter (file-executable-p formatter)) formatter nil)))

(provide 'purty)
;;; purty.el ends here
