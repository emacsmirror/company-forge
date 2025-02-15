;;; company-forge.el --- Company backend for assignees, and topics from forge -*- lexical-binding: t -*-

;; Copyright (C) 2025 Przemyslaw Kryger

;; Author: Przemyslaw Kryger <pkryger@gmail.com>
;; Keywords: tools completion company forge
;; Homepage: https://github.com/pkryger/company-forge.el
;; Package-Requires: ((emacs "29.1") (company "1.0.0") (forge "0.4.7"))
;; Version: 0.0.0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; TODO:

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
arguments, CONDITION and ARG respectively to `buffer-match-p',
which see."
  :type 'buffer-predicate
  :safe #'booleanp)

(defvar-local company-forge--repo nil)
(defvar-local company-forge--cache nil)
(defvar-local company-forge--type nil)

(defun company-forge--completion-suffix (prefix)
  "Get completion suffix for the PREFIX starting from the current point."
  (when-let*
      ((type (aref prefix 0))
       (regexp (if (eq type ?#)
                   (when-let* ((length (- 10 (length prefix)))
                               ((<= 0 length)))
                     (rx-to-string `(seq
                                     (group (repeat 0 ,length digit))
                                     (or whitespace line-end string-end))))
                 (cond
                  ((string= "@" prefix)
                   (if (looking-at (rx (or whitespace line-end string-end)))
                       (rx (group ""))
                     (rx-let ((identifier
                               (seq alphanumeric
                                    (repeat 0 38 (or alphanumeric "-")))))
                       (rx (group
                            (or identifier
                                (seq identifier "/"
                                     (zero-or-one identifier))))
                           (or whitespace line-end string-end)))))
                  ((string-match (
                                  rx "/" alphanumeric (group (zero-or-more any)))
                                 prefix)
                   (when-let* ((length (- 37 (length (match-string-no-properties 1))))
                               ((<= 0 length)))
                     (rx-to-string
                      `(seq (group
                             (repeat 0 ,length (or alphanumeric "-")))
                            (or whitespace line-end string-end)))))
                  ((string-match (rx "/" string-end) prefix)
                   (rx (seq (group
                             alphanumeric
                             (repeat 0 38
                                     (or alphanumeric "-")))
                            (or whitespace line-end string-end))))
                  ((string-match
                    (rx "@" alphanumeric (group (zero-or-more any)))
                    prefix)
                   (when-let* ((length (- 37 (length (match-string-no-properties 1))))
                               ((<= 0 length)))
                     (rx-let-eval '((identifier (length)
                                                    (repeat 0 length (or alphanumeric "-"))))
                       (rx-to-string
                        `(seq (group (zero-or-one
                                      (or (identifier ,length)
                                          (seq (identifier ,length) "/"
                                               (zero-or-one (seq alphanumeric
                                                                 (identifier 38)))))))
                              (or whitespace line-end string-end))))))))))
    (when (looking-at regexp)
      (match-string-no-properties 1))))

(defun company-forge--completion-prefix ()
  "Get the completion prefix at the current point."
  (company-grab-line
   (rx-let ((identifier
             (seq alphanumeric
                  (repeat 0 38 (or alphanumeric "-")))))
     (rx (or
          (seq "#" (repeat 0 10 digit))
          (seq "@" (zero-or-one
                    (or identifier
                        (seq identifier "/"
                             (zero-or-one identifier))))))))))

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
      (if (eq company-forge--type ?#)
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
  (unless company-forge--cache
    (company-forge-reset-cache))
  (let ((key (format "%c%s" company-forge--type prefix)))
    (if-let* ((value (gethash key company-forge--cache)))
        value
      (puthash
       key
       (if (eq company-forge--type ?#)
           (company-forge--topics prefix)
         (company-forge--assignees prefix))
       company-forge--cache))))

(defun company-forge--init ()
  "Initialize `company-forge' backend for the current buffer."
  (if-let* ((repo (forge-get-repository :tracked?)))
      (setq company-forge--repo repo)
    (error "No tracked forge repository")))

(defun company-forge-reset-cache ()
  "Reset `company-forge' cache for the current buffer."
  (interactive)
  (setq company-forge--cache (make-hash-table :test #'equal :size 10)))

;;;###autoload
(defun company-forge (command &optional arg &rest _)
  "The `company-forge' backend entry point.
See the documentation of `company-backends' for COMMAND and ARG."
  (interactive (list 'interactive))
  (pcase command
    ('match (company-forge--match arg))
    ('candidates (company-forge--candidates arg))
    ('prefix (company-forge--prefix))
    ('sorted (eq company-forge--type ?#))
    ('no-cache t)
    ('init (company-forge--init))
    ('interactive (company-begin-backend 'company-forge))))

(provide 'company-forge)

;;; company-forge.el ends here
