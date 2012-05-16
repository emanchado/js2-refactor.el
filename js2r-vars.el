(require 'mark-multiple)

;; Add to jslint globals annotation

(defun current-line ()
  (buffer-substring (save-excursion (beginning-of-line) (point))
                    (save-excursion (end-of-line) (point))))

(require 'thingatpt)

(defun js2r-add-to-globals-annotation ()
  (interactive)
  (let ((var (word-at-point)))
    (save-excursion
      (beginning-of-buffer)
      (when (not (string-match "^/\\* global " (current-line)))
        (newline)
        (previous-line)
        (insert "/* global */")
        (newline)
        (previous-line))
      (while (not (string-match "*/" (current-line)))
        (next-line))
      (end-of-line)
      (delete-char -2)
      (unless (looking-back "global ")
        (while (looking-back " ")
          (delete-char -1))
        (insert ", "))
      (insert (concat var " */")))))

;; Rename variable

(defun js2r--name-node-at-point ()
  (let ((current-node (js3-node-at-point)))
    (unless (js3-name-node-p current-node)
      (setq current-node (js3-node-at-point (- (point) 1))))
    (if (not (and current-node (js3-name-node-p current-node)))
        (error "Point is not on an identifier.")
      current-node)))

(defun js2r--local-name-node-p (node)
  (and (js3-name-node-p node)
       (not (save-excursion ; not key in object literal { key: value }
              (goto-char (+ (js3-node-abs-pos node) (js3-node-len node)))
              (looking-at "[\n\t ]*:")))
       (not (save-excursion ; not property lookup on object
              (goto-char (js3-node-abs-pos node))
              (looking-back "\\.[\n\t ]*")))))

(defun js2-rename-var ()
  "Renames the variable on point and all occurrences in its lexical scope."
  (interactive)
  (let ((current-node (js2r--name-node-at-point)))
    (unless (js2r--local-name-node-p current-node)
      (error "Point is not on a local identifier"))
    (let* ((name (js3-name-node-name current-node))
           (scope (js3-node-get-enclosing-scope current-node))
           (scope (js3-get-defining-scope scope name))
           (current-start (js3-node-abs-pos current-node))
           (current-end (+ current-start (js3-node-len current-node))))
      (push-mark current-end)
      (goto-char current-start)
      (activate-mark)
      (mm/create-master current-start current-end)
      (js3-with-unmodifying-text-property-changes
        (js3-visit-ast
         scope
         (lambda (node end-p)
           (when (and (not end-p)
                      (not (eq node current-node))
                      (js2r--local-name-node-p node)
                      (string= name (js3-name-node-name node)))
             (let* ((start (js3-node-abs-pos node))
                    (end (+ start (js3-node-len node))))
               (mm/add-mirror start end)))
           t))))))

;; Extract variable

(defun js2r--start-of-parent-stmt ()
  (js3-node-abs-pos (js3-node-parent-stmt (js3-node-at-point))))

(defun js2r--object-literal-key-behind (pos)
  (save-excursion
    (goto-char pos)
    (when (looking-back "\\sw: ?")
      (backward-char 2)
      (js3-name-node-name (js2r--name-node-at-point)))))

(defun js2-extract-variable (start end)
  (interactive "r")
  (unless (use-region-p)
    (error "Mark the expression you want to extract first."))
  (let ((deactivate-mark nil)
        (expression (buffer-substring start end))
        (varpos (make-marker))
        (name (or (js2r--object-literal-key-behind start) "name"))
        beg)
    (delete-region start end)
    (set-marker varpos (point))
    (insert name)
    (goto-char (js2r--start-of-parent-stmt))
    (insert "var ")
    (setq beg (point))
    (insert name)
    (insert (concat " = " expression ";\n"))
    (when (string-match-p "^function " expression)
      (insert "\n"))
    (goto-char varpos)
    (indent-region beg (point))
    (push-mark (+ (length name) varpos) t t)
    (mm/create-master varpos (+ (length name) varpos))
    (mm/add-mirror beg (+ (length name) beg))))

;; todo: mark-multiple should switch to multiple-cursors after first change
;;       also: always delete everything, not rely on region to do that.

(provide 'js2r-vars)
