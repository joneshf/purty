;;; purty-format.el --- Minor-mode to format PureScript code with purty on save

;; Version: 0.1.0
;; Keywords: purescript format purty
;; Package-Requires: ((reformatter "0.4"))
;; URL: https://gitlab.com/joneshf/purty/purty-format.el
;; Author: Gabe Johnson

;; To use this package with `purescript-mode', simply load the file and add a
;; directory local variable in a `.dir-locals.el' file at the root of your
;; project:
;;
;;   ((purescript-mode (mode . purty-format-on-save)))

(require 'reformatter)

(defgroup purty-format nil
  "Minor mode for formatting PureScript buffers with purty."
  :group 'languages
  :link '(url-link "https://gitlab.com/joneshf/purty/purty-format.el"))

(defcustom purty-format-command "purty"
  "The name of the `purty-format` command."
  :group 'purty-format
  :type 'string)

(reformatter-define purty-format
  :program purty-format-command
  :args '("-")
  :group 'purty-format
  :lighter " PurtyFmt")

(provide 'purty-format)
;;; purty-format.el ends here
