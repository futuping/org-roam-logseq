;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  BACK UP YOUR LOGSEQ DIR BEFORE RUNNING THIS!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (C) Aug 4 2022, William R. Burdick Jr.
;;
;; LICENSE
;; This code is dual-licensed with MIT and GPL licenses.
;; Take your pick and abide by whichever license appeals to you.
;;
;; logseq compatibility
;; put ids and titles at the tops of non-journal files
;; change fuzzy links from [[PAGE]] to [[id:2324234234][PAGE]]
;; also change file links to id links, provided that the links
;; expand to file names that have ids in the roam database.
;;
;; NOTE: this currently only converts fuzzy links.
;; If you have the setting :org-mode/insert-file-link? true in your Logseq config,
;; it won't convert the resulting links.
;;

;; (require 'f)

;; Your logseq directory should be inside your org-roam directory,
;; put the directory you use here
;; (defvar bill/logseq-folder (f-expand (f-join org-roam-directory "zettel")))
(defvar bill/logseq-folder (f-expand org-roam-directory))

;; You probably don't need to change these values
(defvar bill/logseq-pages (f-expand (f-join bill/logseq-folder "pages")))
(defvar bill/logseq-journals (f-expand (f-join bill/logseq-folder "journals")))
;;(defvar bill/rich-text-types [bold italic subscript link strike-through superscript underline inline-src-block footnote-reference inline-babel-call entity])
(defvar bill/rich-text-types '(bold italic subscript link strike-through superscript underline inline-src-block))
;; ignore files matching bill/logseq-exclude-pattern
;; (defvar bill/logseq-exclude-pattern (string-join (list "^" bill/logseq-folder "/logseq/.*$")))
(defvar bill/logseq-exclude-pattern "^$")

(defun bill/textify (headline)
  (save-excursion
    (apply 'concat (flatten-list
                    (bill/textify-all (org-element-property :title headline))))))

(defun bill/textify-all (nodes) (mapcar 'bill/subtextify nodes))

(defun bill/with-length (str) (cons (length str) str))

(defun bill/subtextify (node)
  (cond ((not node) "")
        ((stringp node) (substring-no-properties node))
        ((member (org-element-type node) bill/rich-text-types)
         (list (bill/textify-all (cddr node))
               (if (> (org-element-property :post-blank node))
                   (make-string (org-element-property :post-blank node) ?\s)
                 "")))
        (t "")))

(defun bill/logseq-journal-p (file) (string-match-p (concat "^" bill/logseq-journals) file))

(defun bill/replace-dot-with-tilde (input-str)
  "逐一对字符串中的字符进行判断，如果句号处于数字之间，则将句号替换为~号。"
  (let ((output-str "")
        (len (length input-str))
        (i 0))
    (while (< i len)
      (let ((current-char (aref input-str i))
            (next-char (if (< (1+ i) len) (aref input-str (1+ i)) nil))
            (after-next-char (if (< (+ i 2) len) (aref input-str (+ i 2)) nil)))
        (if (and (>= current-char ?0) (<= current-char ?9)
                 (and next-char (char-equal next-char ?.)
                      (and after-next-char (>= after-next-char ?0) (<= after-next-char ?9))))
            (progn
              (setq output-str (concat output-str (char-to-string current-char) "~"))
              (setq i (1+ i))) ; Skip the next character (the dot)
          (setq output-str (concat output-str (char-to-string current-char))))
        (setq i (1+ i))))
    output-str))

(defun bill/replace-tilde-with-dot (input-str)
  "逐一对字符串中的字符进行判断，如果~号处于数字之间，则将句号替换为句号。"
  (let ((output-str "")
        (len (length input-str))
        (i 0))
    (while (< i len)
      (let ((current-char (aref input-str i))
            (next-char (if (< (1+ i) len) (aref input-str (1+ i)) nil))
            (after-next-char (if (< (+ i 2) len) (aref input-str (+ i 2)) nil)))
        (if (and (>= current-char ?0) (<= current-char ?9)
                 (and next-char (char-equal next-char ?~)
                      (and after-next-char (>= after-next-char ?0) (<= after-next-char ?9))))
            (progn
              (setq output-str (concat output-str (char-to-string current-char) "."))
              (setq i (1+ i))) ; Skip the next character (the tidle)
          (setq output-str (concat output-str (char-to-string current-char))))
        (setq i (1+ i))))
    output-str))

(defun bill/capitalize-title-words (title)
  "Capitalize the first word of the TITLE and words after '?', ':', ';', '!'."
  (let* ((words (split-string title " "))
         (result '())
         (capitalize-next t))
    (dolist (word words)
      (when (not (string-empty-p word))
        (push (if capitalize-next
                  (progn
                    (setq capitalize-next nil)
                    (capitalize word))
                word)
              result)
        (when (string-match-p "[?:;!]" (substring word -1))
          (setq capitalize-next t))))
    (setq result (nreverse result))
    (string-join result " ")))

(setq org-roam-dailies-capture-templates
      '(("j" "default" entry
         "* %?"
         :if-new (file+head "journal.daily.%<%Y%m%d.%A>.org"
                            "#+title: %<%Y-%m-%d %A>\n"))))

(defun bill/url-encode-special-chars (string)
  "Encode special characters in STRING using URL encoding."
  (replace-regexp-in-string
   "[?:;! ]"
   (lambda (char)
     (format "%%%02x" (string-to-char char)))
   string))

(defun bill/url-encode-special-chars-logseq (string)
  "Encode special characters in STRING using URL encoding."
  (replace-regexp-in-string
   "[?:]"
   (lambda (char)
     (format "%%%02x" (string-to-char char)))
   string))

(defun bill/ensure-file-id (file)
  "Visit an existing file, ensure it has an id, return whether a new buffer was created"
  (setq file (f-expand file))
  (let* ((buf (get-file-buffer file))
         (was-modified (buffer-modified-p buf))
         (new-buf nil)
         has-data
         org
         changed
         sec-end)
    (when (not buf)
      (setq buf (find-file-noselect file))
      (setq new-buf t))
    (set-buffer buf)
    (setq org (org-element-parse-buffer))
    (setq has-data (cddr org))
    (goto-char 1)
    (when (not (and (eq 'section (org-element-type (nth 2 org))) (org-roam-id-at-point)))
      ;; this file has no file id
      ;; (setq changed t)
      (when (eq 'headline (org-element-type (nth 2 org)))
        ;; if there's no section before the first headline, add one
        (insert "\n")
        (goto-char 1))
      (org-id-get-create)
      (setq org (org-element-parse-buffer))
      (save-buffer))
    (when (nth 3 org)
      (when (not (org-collect-keywords ["title"]))
        ;; no title -- ensure there's a blank line at the section end
        ;; (setq changed t)
        (setq sec-end (org-element-property :end (nth 2 org)))
        (goto-char (1- sec-end))
        (when (and (not (equal "\n\n" (buffer-substring-no-properties (- sec-end 2) sec-end))))
          (insert "\n")
          (goto-char (1- (point)))
          (setq org (org-element-parse-buffer)))
        ;; set the title to the file name
        (let ((regex "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\(Monday\\|Tuesday\\|Wednesday\\|Thursday\\|Friday\\|Saturday\\|Sunday\\)\\|^[0-9]\\{4\\}_[0-9]\\{2\\}_[0-9]\\{2\\}$"))
          (if (string-match-p regex (f-base file))
              (progn
                (insert (format "#+title: %s" (f-base file)))
                (save-buffer)
                (setq templates-str (prin1-to-string org-roam-dailies-capture-templates))
                ;; (when (string-match "journal.*\\.org" templates-str)
                (when (string-match "file\\+head \"\\(.*\\.org\\)\"" templates-str)
                  (setq found-file-path (match-string 1 templates-str))
                  (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \\([A-Za-z]+\\)" (f-base file))
                    (setq date (match-string 1 (f-base file)))
                    (setq replaced-string (replace-regexp-in-string "%<.*?>" (format-time-string "%Y%m%d.%A" (date-to-time date)) found-file-path)))
                  (when (string-match "\\([0-9]\\{4\\}_[0-9]\\{2\\}_[0-9]\\{2\\}\\)" (f-base file))
                    (setq formatted-date (replace-regexp-in-string "_" "-" (f-base file)))
                    (setq replaced-string (replace-regexp-in-string "%<.*?>" (format-time-string "%Y%m%d.%A" (date-to-time formatted-date)) found-file-path)))
                  ;; change file name like org-roam
                  (let* ((initial-file-directory (file-name-directory (buffer-file-name)))
                         (replaced-string (bill/url-encode-special-chars replaced-string))
                         (new-file (concat (expand-file-name replaced-string initial-file-directory))))
                    (rename-file file new-file)
                    (setq buf (find-file-noselect new-file)))))
            (progn
              (let ((file-name (bill/replace-dot-with-tilde (url-unhex-string (f-base file)))))
                (let ((hierarchy-title (bill/capitalize-title-words (car (last (split-string file-name "\\."))))))
                  (setq hierarchy-title (bill/replace-tilde-with-dot hierarchy-title))
                  (insert (format "#+title: %s" hierarchy-title))
                  (save-buffer))))))))
    ;; ensure org-roam knows about the new id and/or title
    (when changed (save-buffer))
    (cons new-buf buf)))

(defun bill/convert-logseq-file (buf)
  "convert fuzzy, file:../pages and block((uuid)) logseq links in the file to id links"
  (save-excursion
    (let* (changed
           link)
      (set-buffer buf)
      (goto-char 1)
      (while (search-forward "[[" nil t)
        (setq link (org-element-context))
        (setq newlink (bill/reformat-link link))
        (when newlink
          (setq changed t)
          (goto-char (org-element-property :begin link))
          (delete-region (org-element-property :begin link) (org-element-property :end link))
          ;; note, this format string is reall =[[%s][%s]]= but =%= is a markup char so one's hidden
          (insert newlink)))
      (goto-char (point-min))
      (while (search-forward "((" nil t)
        (let ((start-pos (point)))
          (when (search-forward "))" nil t)
            (let ((end-pos (point)))
              (let ((uuid (buffer-substring-no-properties start-pos (- end-pos 2))))
                (when (string-match-p "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'" uuid)
                  (let ((db-result (org-roam-db-query [:select [id title] :from nodes :where (= id $s1)] uuid)))
                    (when db-result
                      (setq changed t)
                      (setq uuid-id (car (car db-result)))
                      (setq title-id (cadr (car db-result)))
                      (delete-region (- start-pos 2) end-pos)
                      (insert (format "[[id:%s][%s]]" uuid-id title-id))))))))))
      ;; ensure org-roam knows about the changed links
      (when changed (save-buffer)))))

;; find the type of link in org file：
;; (require 'org-element)
;; (setq org-tree (org-element-parse-buffer))
;; (org-element-map org-tree 'link
;;   (lambda (link)
;;     (message "链接类型：%s" (org-element-property :type link))))

(defun bill/reformat-link (link)
  (let (filename
        id
        linktext
        newlink)
    (when (eq 'link (org-element-type link))
      (when (equal "fuzzy" (org-element-property :type link))
        (setq filename (f-expand (f-join bill/logseq-pages
                                         (concat (org-element-property :path link) ".org"))))
        (setq linktext (org-element-property :raw-link link)))
      (when (equal "file" (org-element-property :type link))
        (setq filename (f-expand (org-element-property :path link)))
        (if (org-element-property :contents-begin link)
            (setq linktext (buffer-substring-no-properties
                            (org-element-property :contents-begin link)
                            (org-element-property :contents-end link)))
          (setq linktext (buffer-substring-no-properties
                          (+ (org-element-property :begin link) 2)
                          (- (org-element-property :end link) 2)))))

      (if (equal "id" (org-element-property :type link))
          (progn
            (setq linkuuid (org-element-property :path link))
            ;; (setq id (caar (org-roam-db-query [:select id :from nodes :where (= id $s1)] linkuuid)))
            (setq title-linkuuid (caar (org-roam-db-query [:select title :from nodes :where (= id $s1)] linkuuid)))
            (setq new-newlink (format "[[id:%s][%s]]" linkuuid title-linkuuid))
            (setq newlink new-newlink)
            newlink)
        (progn
          (setq logseq-filename-base (bill/url-encode-special-chars-logseq (f-base filename)))
          (setq logseq-filename (concat (file-name-directory filename) logseq-filename-base ".org"))
          (when (and logseq-filename (f-exists-p logseq-filename))
            (setq id-filename (caar (org-roam-db-query [:select id :from nodes :where (like file $s1)] logseq-filename)))
            (setq title-filename (caar (org-roam-db-query [:select title :from nodes :where (like file $s1)] logseq-filename)))
            (setq new-newlink (format "[[id:%s][%s]]" id-filename title-filename))
            ;; change file name like org-roam
            (let* ((directory-path (file-name-directory logseq-filename))
                   (replace-string (bill/url-encode-special-chars (f-base logseq-filename)))
                   (new-filename (concat (expand-file-name replace-string directory-path) ".org")))
              (rename-file logseq-filename new-filename))
            (when id-filename
              (setq newlink (format "[[id:%s][%s]]%s"
                                    id-filename
                                    linktext
                                    (if (> (org-element-property :post-blank link))
                                        (make-string (org-element-property :post-blank link) ?\s)
                                      "")))
              (when (or (not (equal newlink
                                    (buffer-substring-no-properties
                                     (org-element-property :begin link)
                                     (org-element-property :end link))))
                        (not (equal newlink new-newlink)))
                (progn
                  (setq newlink new-newlink)
                  newlink)))))))))

(defun bill/roam-file-modified-p (file-path)
  (and (not (string-match-p bill/logseq-exclude-pattern file-path))
       (let ((content-hash (org-roam-db--file-hash file-path))
             (db-hash (caar (org-roam-db-query [:select hash :from files
                                                        :where (= file $s1)] file-path))))
         (not (string= content-hash db-hash)))))

(defun bill/modified-logseq-files ()
  (emacsql-with-transaction (org-roam-db)
    (seq-filter 'bill/roam-file-modified-p
                (org-roam--list-files bill/logseq-folder))))

(defun bill/check-logseq ()
  (interactive)
  (let (created
        files
        bufs
        unmodified
        cur
        bad
        buf)
    (setq files (org-roam--list-files bill/logseq-folder))
    ;; make sure all the files have file ids
    (dolist (file-path files)
      (setq file-path (f-expand file-path))
      (setq cur (bill/ensure-file-id file-path))
      (setq buf (cdr cur))
      (push buf bufs)
      (when (not buf)
        (push file-path bad))
      (when (not (buffer-modified-p buf))
        (push buf unmodified))
      (when (car cur)
        (push buf created)))
    ;; patch fuzzy links
    (mapc 'bill/convert-logseq-file (seq-filter 'identity bufs))
    (dolist (buf unmodified)
      (when (buffer-modified-p buf)
        (save-buffer unmodified)))
    (mapc 'kill-buffer created)
    (when bad
      (message "Bad items: %s" bad))
    nil))


(provide 'org-roam-logseq)
;;; org-roam-logseq.el ends here
