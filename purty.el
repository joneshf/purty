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
  :program purty-command
  :args '("-")
  :group 'purty
  :lighter " PurtyFmt")

(provide 'purty)
;;; purty.el ends here
