;;; company-forge.el --- Company backend for mentions and topics from forge -*- lexical-binding: t -*-

;; Copyright (C) 2025 Przemyslaw Kryger

;; Author: Przemyslaw Kryger <pkryger@gmail.com>
;; Keywords: convenience completion company forge
;; Homepage: https://github.com/pkryger/company-forge.el
;; Package-Requires: ((emacs "29.1") (company "1.0.0") (forge "0.5.0"))
;; Version: 0.0.0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;;            ____________________________________________________
;;
;;               COMPANY-FORGE.EL - COMPANY BACKEND FOR MENTIONS
;;                            AND TOPICS FROM FORGE
;;            ____________________________________________________
;;
;;
;; The `company-forge' is a [company-mode] completion backend for [forge].  It
;; uses current `forge' repository data to offer completions for mentions (`@'
;; references to users and teams) and topics (`#' (or `!' for GitLab)
;; references to issues, discussions, and pull requests).
;;
;;
;; [company-mode] <https://github.com/company-mode/company-mode>
;;
;; [forge] <https://github.com/magit/forge>
;;
;;
;; Features
;; ========
;;
;; - Offer completion after entering `@' and `#' (or `!' for GitLab).
;; - Support for users, teams, issues, discussions, and pull requests.
;; - Suppoet for different matching types (see `company-forge-match-type').
;; - Display [octicons] for candidates (see `company-forge-icons-mode').
;; - Display issues, discussions, and pull-request text as a documentation
;;   with `quickhelp-string' and `doc-buffer' `company' commands.
;; - Provide `company-forge-completion-at-point-function' compatible with
;;   built in command `completion-at-point'.
;;
;;
;; [octicons] <https://github.com/primer/octicons>
;;
;;
;; Installation
;; ============
;;
;; Installing from MELPA
;; ~~~~~~~~~~~~~~~~~~~~~
;;
;; The easiest way to install and keep `company-forge' up-to-date is using
;; Emacs' built-in package manager.  `company-forge' is available in the MELPA
;; repository.  Refer to <https://melpa.org/#/getting-started> for how to
;; install a package from MELPA.
;;
;; Please see [Configuration] section for example configuration.
;;
;; You can use any of the package managers that supports installation from
;; MELPA.  It can be one of (but not limited to): one of the built-in
;; `package', `use-package', or any other package manger that handles
;; autoloads generation, for example (in alphabetical order) [Borg], [Elpaca],
;; [Quelpa], or [straight.el].
;;
;;
;; [Configuration] See section Configuration
;;
;; [Borg] <https://github.com/emacscollective/borg>
;;
;; [Elpaca] <https://github.com/progfolio/elpaca>
;;
;; [Quelpa] <https://github.com/quelpa/quelpa>
;;
;; [straight.el] <https://github.com/radian-software/straight.el>
;;
;;
;; Installing from GitHub
;; ~~~~~~~~~~~~~~~~~~~~~~
;;
;; The preferred method is to use built-in `use-package'.  Add the following
;; to your Emacs configuration file (usually `~/.emacs' or
;; `~/.emacs.d/init.el'):
;;
;; (use-package company-forge
;;   :vc (:url "https://github.com/pkryger/company-forge.el.git"
;;        :rev :newest)))
;;
;; Please refer to [Configuration] section for example configuration.
;;
;;
;; [Configuration] See section Configuration
;;
;;
;; Configuration
;; =============
;;
;; This section assumes you have `company-forge''s autoloads set up at Emacs
;; startup.  If you have installed `company-forge' using built-in `package' or
;; `use-package' then you should be all set.
;;
;; (use-package company-forge
;;   :config
;;   (company-forge-icons-mode) ;; Display icons
;;   (advice-add #'forge--pull ;; Reset cache after forge pull
;;               :filter-args #'company-forge-reset-cache-after-pull)
;;   (add-to-list 'company-backends 'company-forge))
;;
;; As an alternative to using `company-forge' backend (which is a recommended
;; way) a function `company-forge-completion-function' can be used.  This
;; function has been designed to be used in `completion-at-point-functions'
;; and can be added there with, for example:
;;
;; (add-hook 'completion-at-point-functions
;;             #'company-forge-completion-at-point-function)
;;
;; The user option `company-forge-capf-doc-buffer-function' controls how the
;; documentation buffer (and `company-posframe' quickdoc) is displayed in such
;; a case.
;;
;;
;; Customization
;; =============
;;
;; Below is a list of configuration options and optional functions with their
;; synopses that can be used to customize `company-forge' behavior.  Please
;; refer to documentation of respective symbol for more details.
;;
;; - user option: `company-forge-match-type': define how to match candidates
;; - user option: `company-forge-predicate': a buffer predicate to control if
;;   the backend is enabled
;; - user option: `company-forge-use-cache': control whether cache is used for
;;   candidates retrieval
;; - user option: `company-forge-capf-doc-buffer-function': a function used do
;;   show documentation when `company' backend `company-capf' is used
;; - minor mode: `comany-forge-icons-mode': control whether to display icons
;;   for candidates
;; - function: `company-forge-reset-cache-after-pull': designed as a
;;   `:filter-args' advice for `forge--pull'
;;
;;
;; Contributing
;; ============
;;
;; Contributions are welcome! Feel free to submit issues and pull requests on
;; the [GitHub repository].
;;
;;
;; [GitHub repository] <https://github.com/pkryger/company-forge.el>
;;
;; Testing
;; ~~~~~~~
;;
;; When creating a pull request make sure all tests in
;; <file:test/company-forge.t.el> are passing.  When adding a new
;; functionality, please strive to add tests for it as well.
;;
;; To run tests:
;; - open the <file:test/company-forge.t.el>
;; - type `M-x eval-buffer <RET>'
;; - type `M-x ert <RET> t <RET>'
;;
;;
;; Documentation autoring
;; ~~~~~~~~~~~~~~~~~~~~~~
;;
;; This package uses the following packages to help with documentation
;; authoring:
;;
;; - [org-commentary.el] (which is different from the one available on MELPA!)
;;   to generate and validate commentary section in `difftastic.el'.  Please
;;   see the package documentation for usage instructions.
;;
;; - [org-make-toc] to generate and validate table of contents in the
;;   `README.org' file.
;;
;; Appropriate functions from both of these packages are added to
;; `after-save-hook' and `before-save-hook' respectively, when packages are
;; available in user's Emacs (see `dir-locals.el').
;;
;;
;; [org-commentary.el] <https://github.com/pkryger/org-commentary.el>
;;
;; [org-make-toc] <https://github.com/alphapapa/org-make-toc>

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'eieio)
(require 'forge)
(require 'rx)

(defgroup company-forge nil
  "Company backend for assignees, and topics from forge."
  :link '(emacs-commentary-link "company-forge")
  :group 'tools
  :group 'conveniance
  :group 'matching)

(defcustom company-forge-match-type 'infix
  "How to perform a match.
The value can be one of prefix, infix, or anywhere.
Alternatively, it can be a cons cell in a form of (TOPIC
. ASSIGNEE), where TOPIC defines how to perform match for
topics (# references to issues and pull requests) and ASSIGNEE
defines how to perform a match for assignees (@ mentions of users
and teams).

The value of prefix means that only beginning of candidates is
matched, for example typing \"@b\" will match \"bar\" and
\"baz\", but typing \"@a\" won't match them.  The value anywhere
means that anywhere in the string is matched, for example typing
\"@b\" will match like in prefix case, yet typing \"@a\" will
yield the same matches.  The value infix has the same meaning as
prefix for topic, but for assignees it enables mathing prefixes
of teams, for example typing \"@f\" will yield \"foo\" and
\"org/foo\"."
  :type '(choice (radio (const prefix)
                        (const infix)
                        (const anywhere))
                 (cons (radio :tag "topic"
                              (const prefix)
                              (const anywhere))
                       (radio :tag "assignee"
                              (const prefix)
                              (const infix)
                              (const anywhere))))
  :group 'company-forge)

(defcustom company-forge-predicate '(or (derived-mode . forge-post-mode)
                                        (derived-mode . git-commit-ts-mode)
                                        (derived-mode . git-commit-elisp-text-mode)
                                        (lambda (buffer _prefix)
                                          (with-current-buffer buffer
                                            (bound-and-true-p git-commit-mode))))
  "Whether to use `company-forge' in a buffer.
The default is to enable the mode in all buffers that are either
in `git-commit-mode', `git-commit-ts-mode',
`git-commit-elisp-text-mode', or `forge-post-mode', that is in
buffers that are usually contain `forge' assignees and topics.

The predicate and current prefix are passed as
arguments CONDITION and ARG respectively to `buffer-match-p',
which see."
  :type 'buffer-predicate
  :group 'company-forge
  :safe #'booleanp)

(defcustom company-forge-use-cache t
  "Whether to use cache for candidates retrieval.
While using cache for candidate retrieval may improve experience
while typing (avoiding an SQL query on each keypress) it may turn
out to be cumbersome to reset cache after a forge repository has
been updated (for example after a `forge-pull').  This may become
especially visible in a long living buffers.  The recommendation
is to use function `company-forge-reset-cache-after-pull' (which
see) for automated cache cleanup and command
`company-forge-reset-cache' for manual cache cleanup.  If the
above methods prove to be insufficient the caching mechanism may
be turned off by setting this variable to nil."
  :type 'boolean
  :group 'company-forge
  :safe #'booleanp)

(defcustom company-forge-capf-doc-buffer-function
  (if (bound-and-true-p company-posframe-mode)
      #'company-forge--capf-quickhelp-buffer
    #'company-forge--doc-buffer)
  "Function that is used to provide a documentation when called via capf.
This function is used as when `company-show-doc-buffer' is called or
when `company-posframe' automatically displays a quickhelp after a
timeout or as a response to `company-posframe-quickhelp-toggle'.  It
comes to play when `company-forge-completion-at-point-function' is in
`completion-at-point-functions' and completion is performed with
`company' backend `company-capf'.  The function
`company-forge--doc-buffer' displays a detailed documention, including
all topic headers, similarly to what `forge' would do to display the
topic.  However, such a detailed documentation, may be less desireable
when documentation is presented in a quickhelp popup of
`company-posframe'.  It is recommended to use
`company-forge--capf-quickhelp-buffer' in such a case."
  :type  '(radio
           (const :tag "Quickhelp buffer (recommended for `company-posframe')"
                  company-forge--capf-quickhelp-buffer)
           (const :tag "Doc buffer"
                  company-forge--doc-buffer))
  :group 'company-forge)

(defconst company-forge-icons-directory
  (when-let* ((directory (file-name-directory
                          (or load-file-name
                              (bound-and-true-p byte-compile-current-file)
                              (buffer-file-name)))))
    (expand-file-name "icons"
                      directory)))

(defvar company-forge-icons-mapping
  '((discussion . "comment-discussion-16.svg")
    (discussion-closed . "discussion-closed-16.svg")
    (discussion-duplicate . "discussion-duplicate-16.svg")
    (discussion-outdated . "discussion-outdated-16.svg")
    (issue . "issue-opened-16.svg")
    (issue-closed . "issue-closed-16.svg")
    (issue-draft . "issue-draft-16.svg")
    (pullreq . "git-pull-request-16.svg")
    (pullreq-merged . "git-merge-16.svg")
    (pullreq-rejected . "git-pull-request-closed-16.svg")
    (pullreq-draft . "git-pull-request-draft-16.svg")
    (user . "person-16.svg")
    (team . "people-16.svg")))

(defvar company-forge-text-icons-mapping
  '((discussion "d" forge-discussion-open)
    (discussion-closed "c" forge-discussion-completed)
    (discussion-duplicate "c" forge-discussion-expunged)
    (discussion-outdated "o" forge-discussion-expunged)
    (issue "i" forge-issue-open)
    (issue-closed "c" forge-issue-completed)
    (issue-draft "d" forge-topic-pending)
    (pullreq "p" forge-pullreq-open)
    (pullreq-merged "m" forge-pullreq-merged)
    (pullreq-rejected "r" forge-pullreq-rejected)
    (pullreq-draft  "d" forge-pullreq-draft)
    (user "u")
    (team "t")))

(defvar company-forge--cache (make-hash-table :test #'equal :size 10))

(defvar-local company-forge--repo nil)
(defvar-local company-forge--type nil)

(defun company-forge--topic-type-p (type)
  "Return non nil when TYPE is a topic (# or !)."
  (memq type '(?# ?!)))

(defun company-forge--completion-suffix (prefix)
  "Get completion suffix for the PREFIX starting from the current point."
  (when-let*
      ((type (aref prefix 0))
       (regexp (if (company-forge--topic-type-p type)
                   (when-let* ((length (- 10 (length prefix)))
                               ((<= 0 length)))
                     (rx-to-string `(seq
                                     (group (repeat 0 ,length digit))
                                     (or "," whitespace line-end string-end))))
                 (cond
                  ((string= "@" prefix)
                   (if (looking-at (rx (or "," whitespace line-end string-end)))
                       (rx (group ""))
                     (rx-let ((identifier
                               (seq alphanumeric
                                    (repeat 0 38 (or alphanumeric "-")))))
                       (rx (group
                            (or identifier
                                (seq identifier "/"
                                     (zero-or-one identifier))))
                           (or "," whitespace line-end string-end)))))
                  ((string-match (
                                  rx "/" alphanumeric (group (zero-or-more any)))
                                 prefix)
                   (when-let* ((length (- 37
                                          (length (match-string-no-properties 1))))
                               ((<= 0 length)))
                     (rx-to-string
                      `(seq (group
                             (repeat 0 ,length (or alphanumeric "-")))
                            (or "," whitespace line-end string-end)))))
                  ((string-match (rx "/" string-end) prefix)
                   (rx (seq (group
                             alphanumeric
                             (repeat 0 38
                                     (or alphanumeric "-")))
                            (or "," whitespace line-end string-end))))
                  ((string-match
                    (rx "@" alphanumeric (group (zero-or-more any)))
                    prefix)
                   (when-let* ((length (- 37
                                          (length (match-string-no-properties 1))))
                               ((<= 0 length)))
                     (rx-let-eval '((identifier (length)
                                                (repeat 0 length (or alphanumeric "-"))))
                       (rx-to-string
                        `(seq (group (zero-or-one
                                      (or (identifier ,length)
                                          (seq (identifier ,length) "/"
                                               (zero-or-one (seq alphanumeric
                                                                 (identifier 38)))))))
                              (or "," whitespace line-end string-end))))))))))
    (when (looking-at regexp)
      (match-string-no-properties 1))))

(defun company-forge--completion-prefix ()
  "Get the completion prefix at the current point."
  (company-grab-line
   (rx-let-eval `((identifier
                   (seq alphanumeric
                        (repeat 0 38 (or alphanumeric "-"))))
                  (topic-prefix ,(if (forge--childp company-forge--repo
                                                    'forge-gitlab-repository)
                                     '(or "#" "!")
                                   "#")))
     (rx-to-string
      `(seq
        (or string-start line-start whitespace)
        (group
         (or
          (seq topic-prefix (repeat 0 10 digit))
          (seq "@" (zero-or-one
                    (or identifier
                        (seq identifier "/"
                             (zero-or-one identifier))))))))))
   1))

(defun company-forge--grab-symbol-parts ()
  "Grab symbol parts.
Return a list compatible with a company backend command prefix."
  (if-let* ((prefix (company-forge--completion-prefix))
            (suffix (company-forge--completion-suffix prefix)))
      (progn
        (setq company-forge--type (aref prefix 0))
        (list (substring prefix 1) suffix t))
  (setq company-forge--type nil)))

(defun company-forge--prefix ()
  "Return prefix when buffer matches `company-forge-predicate'.
The value returned is compatible with company backend command
prefix."
  (when-let* ((prefix (company-forge--grab-symbol-parts))
              ((buffer-match-p company-forge-predicate
                               (current-buffer)
                               prefix)))
    prefix))

(defun company-forge--match-type ()
  "Get the match type for the current completion."
  (if (consp company-forge-match-type)
      (if (company-forge--topic-type-p company-forge--type)
          (car company-forge-match-type)
        (cdr company-forge-match-type))
    company-forge-match-type))

(defun company-forge--string-match (pat candidate &optional pos)
  "Match the PAT in CANDIDATE starting from POS in the latter.
Match is performed according to match type of the current
completion."
  (string-match
   (rx-to-string
    (pcase (company-forge--match-type)
      ('prefix `(seq string-start (group ,pat)))
      ('infix `(seq (or string-start "/") (group ,pat)))
      (_ `(group ,pat))))
   candidate pos))

(defun company-forge--match (candidate)
  "Return all matches of `company-prefix' in CANDIDATE.
Match is performed according to match type of the current
completion.  The value returned is compatible with company
backend command match."
  (when (and company-prefix
             (< 0 (length company-prefix)))
    (let ((pos 0)
          matches)
      (while (company-forge--string-match company-prefix candidate pos)
        (push (cons (match-beginning 1)
                    (setq pos (match-end 1)))
              matches))
      (nreverse matches))))

(defun company-forge--topics (prefix)
  "Return topics matching PREFIX.
Match is performed according to match type of the current
completion."
  (mapcar
   (lambda (topic)
     (propertize
      (number-to-string (oref topic number))
      'company-forge-id (oref topic id)
      'company-forge-annotation (oref topic title)
      'company-forge-kind (pcase (list
                                  (eieio-object-class topic)
                                  (oref topic state)
                                  (when (slot-exists-p topic 'draft-p)
                                    (ignore-error unbound-slot
                                      (oref topic draft-p))))
                            (`(forge-discussion open ,_) 'discussion)
                            (`(forge-discussion duplicate ,_ 'discussion-duplicate))
                            (`(forge-discussion outdated ,_ 'disscussion-outdated))
                            (`(forge-discussion ,_ ,_) 'discussion-closed)
                            ('(forge-issue open nil) 'issue)
                            ('(forge-issue open t) 'issue-draft)
                            (`(forge-issue ,_ ,_) 'issue-closed)
                            ('(forge-pullreq open nil) 'pullreq)
                            ('(forge-pullreq open t) 'pullreq-draft)
                            (`(forge-pullreq merged ,_) 'pullreq-merged)
                            (`(forge-pullreq ,_ ,_) 'pullreq-rejected))))
   (cl-remove-if-not
    (lambda (topic)
      (company-forge--string-match
       prefix
       (number-to-string (oref topic number))))
    (forge--list-topics
     (forge--topics-spec :type 'topic
                         :active nil
                         :state nil
                         :order 'recently-updated)
     company-forge--repo))))

(defun company-forge--assignees (prefix)
  "Return assignees matching PREFIX.
Match is performed according to match type of the current
completion."
  (append
   (cl-remove-if-not
    (lambda (assignee)
      (company-forge--string-match prefix assignee))
    (mapcar (lambda (assignee)
              (propertize (cadr assignee)
                          'company-forge-annotation (caddr assignee)
                          'company-forge-kind 'user))
            (ignore-errors (oref company-forge--repo assignees))))
   (cl-remove-if-not
    (lambda (team)
      (company-forge--string-match prefix team))
    (mapcar (lambda (team)
              (propertize team
                          'company-forge-kind 'team))
            (ignore-errors (oref company-forge--repo teams))))))

(defun company-forge--candidates (prefix)
  "Return all candidates mathing PREFIX.
Match is performed according to match type of the current
completion.  The value returned is compatible with company
backend command candidates."
  (if company-forge-use-cache
      (let ((cache (or (gethash (oref company-forge--repo id)
                                company-forge--cache)
                       (company-forge-reset-cache company-forge--repo)))
            (key (format "%c%s" company-forge--type prefix)))
        (or (gethash key cache)
            (puthash key
                     (if (company-forge--topic-type-p company-forge--type)
                         (company-forge--topics prefix)
                       (company-forge--assignees prefix))
                     cache)))
    (if (company-forge--topic-type-p company-forge--type)
        (company-forge--topics prefix)
      (company-forge--assignees prefix))))

(defun company-forge-reset-cache (&optional repo)
  "Clear and return `company-forge' cache hash table for forge repository REPO.
REPO can be a `forge-repository' object.  REPO can also be nil,
meaning to reset cache for current repository.  In the latter
case when no repository is found the return value is nil.  REPO
can be also \\='all, meaning to reset cache for all repositories.

When called interactively, clear repository for current forge
repository or, when called with prefix argument REPO, clear cache
for all repositories.

When clearing cache for all repositories returned value should be
ignored."
  (interactive "p")
  (if (or (eql 4 repo) (eq 'all repo))
      (clrhash company-forge--cache)
    (when-let* ((repo (or (when (cl-typep repo 'forge-repository) repo)
                          company-forge--repo
                          (forge-get-repository :tracked?))))
      (puthash (oref repo id)
               (make-hash-table :test #'equal :size 10)
               company-forge--cache))))

;;;###autoload
(defun company-forge-reset-cache-after-pull (args)
  "Reset cache when calling callback from ARGS (second element).
This function is designed to be used as a `:filter-args'
advice to `forge--pull'.  However, it will not work for selective
repositories (for example when pulling individual topics).  Use
 `company-forge-reset-cache' in such a case."
  (let ((repo (car args))
        (callback (cadr args)))
      (append
       (list repo
             (lambda ()
               (prog1
                   (when callback (funcall callback))
                 (company-forge-reset-cache repo))))
       (cddr args))))

(defun company-forge--init ()
  "Initialize `company-forge' backend for the current buffer."
  (if-let* ((repo (forge-get-repository :tracked?)))
      (setq company-forge--repo repo)
    (error "No tracked forge repository")))

(defun company-forge--add-text-icons-mapping (icons-mapping)
  "Add mappings from `company-forge-text-icons-mapping' to ICONS-MAPPING."
  (dolist (mapping company-forge-text-icons-mapping)
    (setf (alist-get (car mapping) icons-mapping)
          (cdr mapping)))
  icons-mapping)

(defun company-forge--remove-text-icons-mapping (icons-mapping)
  "Remove `company-forge-text-icons-mapping' mappings from ICONS-MAPPING."
  (let ((types (mapcar #'car company-forge-text-icons-mapping)))
    (cl-remove-if (lambda (elt)
                    (memq (car elt) types))
                icons-mapping)))

(defun company-forge-icons-margin (orig-fun &rest args)
  "Display `company-forge' icons for candidate (car ARGS).
If icon cannot be displayed call ORIG-FUN."
  (if-let* ((candidate (car args))
            ((get-text-property 0 'company-forge-kind candidate))
            (company-forge-icons-directory)
            ((display-graphic-p))
            ((image-type-available-p 'svg))
            ;; Octicons delivered with `company-forge' are slightly larger
            ;; than icons delivered with `company'. Make them appear a bit
            ;; smaller
            (icon (let ((company-icon-size
                         (pcase company-icon-size
                           ((and (pred numberp) value)
                            (truncate (fround (* .9 value))))
                           (`(auto-scale . ,value)
                            (cons 'auto-scale
                                  (truncate (fround (* .9 value))))))))
                    (company--render-icons-margin
                     company-forge-icons-mapping
                     company-forge-icons-directory
                     candidate
                     (cadr args)))))
      icon
    (apply orig-fun args)))

;;;###autoload
(define-minor-mode company-forge-icons-mode
  "A minor mode to display margin icons for `company-forge' backend candidates.
Note that graphical icons will be displayed only when
`company-format-margin-function' is set to
`company-detect-icons-margin'.  when a different function is
used, then it may need to be adviced with
`company-forge-icons-margin'.  Text icons will be displayed in
the above case (when Emacs is not capable of rendering icons) as
well as when `company-format-margin-function' is set to
`company-text-icons-margin'."
  :global t
  :group 'company-forge
  (if company-forge-icons-mode
      (progn
        (setq company-text-icons-mapping (company-forge--add-text-icons-mapping
                                          company-text-icons-mapping))
        (advice-add #'company-detect-icons-margin
                    :around #'company-forge-icons-margin))
    (advice-remove #'company-detect-icons-margin #'company-forge-icons-margin)
    (setq company-text-icons-mapping (company-forge--remove-text-icons-mapping
                                      company-text-icons-mapping))))

(defun company-forge--kind (candidate)
  "Return kind of CANDIDATE.
The value is returned only when `company-forge-icons-mode' is
non-nil to avoid rendering \"symbol-misc\" icons."
  (when company-forge-icons-mode
    (get-text-property 0 'company-forge-kind candidate)))

(defun company-forge--annotation (candidate string &optional face)
  "Return annotation for CANDIDATE formatted with STRING.
Additionally add text property `face' with value of FACE when FACE is non nil."
  (when-let* ((annotation (get-text-property
                           0 'company-forge-annotation candidate))
              (annotation (format string annotation)))
    (if face
        (propertize annotation 'face face)
      annotation)))

(defun company-forge--text-icon-margin (candidate string face)
  "Return text icon for CANDIDATE formatted with STRING.
Additionally add text property `face' with value of FACE."
  (when-let* ((kind (cadr ;; TODO: extract to separate function
                     (assq
                      (get-text-property 0 'company-forge-kind candidate)
                      company-forge-text-icons-mapping))))
    (propertize (format string kind)
                'face face)))

(defun company-forge--quickhelp-string (candidate)
  "Return a quickhelp-string for CANDIDATE.
The CANDIDATE needs to have `company-forge-id' text property set."
  (when-let* ((id (get-text-property 0 'company-forge-id candidate))
              (topic (forge-get-topic id)))
    (concat
     (company-forge-icons-margin
      (lambda (&rest _)
        (or (company-forge--text-icon-margin candidate "[%s] " 'italic)
            ""))
      candidate)
     (propertize (oref topic title)
                 'face 'bold)
     "\n\n"
     (oref topic body))))

(defun company-forge--doc-buffer (candidate)
  "Setup `company-doc-buffer' for CANDIDATE.
The CANDIDATE needs to have `company-forge-id' text property set."
  (when-let* ((id (get-text-property 0 'company-forge-id candidate))
              (topic (forge-get-topic id))
              (buffer (company-doc-buffer))
              (magit-display-buffer-function #'ignore)
              (magit-display-buffer-noselect t))
    ;; Do like `forge-topic-setup-buffer' does, except:
    ;; - ensure the buffer is not selected with
    ;; - `magit-display-buffer-noselect',
    ;; - don't display the buffer by suppressing
    ;;   `magit-display-buffer-function'; this is to allow `company-posframe'
    ;;   use this function as a part of when called via `company-capf',
    ;; - use `company-doc-buffer' as the buffer,
    ;; - don't mark topic as read,
    ;; - ensure `buffer-read-only' is nil.
    (unwind-protect
        (magit-setup-buffer-internal
         (pcase-exhaustive (eieio-object-class topic)
           ('forge-discussion #'forge-discussion-mode)
           ('forge-issue      #'forge-issue-mode)
           ('forge-pullreq    #'forge-pullreq-mode))
         t
         `((forge-buffer-topic ,topic))
         buffer
         (or (forge-get-worktree company-forge--repo) "/"))
      (with-current-buffer buffer
        (setq buffer-read-only nil)))))

;;;###autoload
(defun company-forge (command &optional arg &rest _)
  "The `company-forge' backend entry point.
See the documentation of `company-backends' for COMMAND and ARG."
  (interactive (list 'interactive))
  (pcase command
    ('match (company-forge--match arg))
    ('kind (company-forge--kind arg))
    ('annotation (company-forge--annotation arg " [%s]"))
    ('prefix (company-forge--prefix))
    ('candidates (company-forge--candidates arg))
    ('quickhelp-string (company-forge--quickhelp-string arg))
    ('doc-buffer (company-forge--doc-buffer arg))
    ('sorted (company-forge--topic-type-p company-forge--type))
    ('no-cache t)
    ('init (company-forge--init))
    ('interactive (company-begin-backend 'company-forge))))

(defun company-forge--capf-affixation (candidates)
  "Add affixation to each candidate in CANDIDATES."
  (let ((company-backend 'company-forge)
        (orig-background (face-attribute 'company-tooltip :background)))
    (unwind-protect
        (progn
          (set-face-attribute 'company-tooltip nil :background 'unspecified)
          (mapcar (lambda (candidate)
                    (list
                     candidate
                     (company-forge-icons-margin
                      (lambda (&rest _)
                        (company-forge--text-icon-margin
                         candidate "%s " 'completions-annotations))
                      candidate)
                     (company-forge--annotation
                      candidate " %s" 'completions-annotations)))
                  candidates))
      (set-face-attribute 'company-tooltip nil :background orig-background))))

(defun company-forge--capf-quickhelp-buffer (candidate)
  "Insert quickhelp for CANDIDATE into a `company-doc-buffer'."
  (let ((company-backend 'company-forge)
         (quickhelp (company-forge--quickhelp-string candidate))
         (buffer (company-doc-buffer)))
    (with-current-buffer buffer
      (insert quickhelp))
    buffer))

(defun company-forge--capf-completion-table (type)
  "Return a completion table with candidates of TYPE."
  (completion-table-dynamic
   (lambda (_)
     (when-let* (((setq company-forge--repo
                        (or company-forge--repo
                            (forge-get-repository :tracked?))))
                 (company-forge--type type))
       (company-forge--candidates "")))
   t))

;;;###autoload
(defun company-forge-completion-at-point-function ()
  "Function used for `completion-at-point-functions'."
  (when-let* ((prefix (company-forge--completion-prefix))
              (end (match-end 1))
              (beg (min (1+ (match-beginning 1))
                        end))
              ((buffer-match-p company-forge-predicate
                               (current-buffer)
                               prefix))
              (type (aref prefix 0)))
    (list beg end
          (company-forge--capf-completion-table type)
          :category (if (company-forge--topic-type-p type)
                        'company-forge-topics
                      'company-forge-assignees)
          :affixation-function #'company-forge--capf-affixation
          :exclusive 'no
          ;; `comapny-capf' interface
          :company-kind #'company-forge--kind
          :company-match #'company-forge--match
          :company-doc-buffer company-forge-capf-doc-buffer-function
          ;; `:annotation-function' is actually used by `company-capf', which
          ;; is nice as it can differ from what `:affixation-function' does for
          ;; regular `complete-at-point'
          :annotation-function #'company-forge--annotation)))

(provide 'company-forge)

;;; company-forge.el ends here
