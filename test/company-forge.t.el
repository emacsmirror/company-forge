;;; company-forge.t.el --- Tests for company-forge   -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:
(when-let* ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file
                                          buffer-file-name))))
  (load-file (file-name-concat dir "undercover-init.el")))

(require 'company-forge)
(require 'ert)


(ert-deftest company-forge-t--completion-suffix ()
  (with-temp-buffer
    (insert "@")
    (should (equal ""
                   (company-forge--completion-suffix "@"))))
  (with-temp-buffer
    (insert "@ ")
    (goto-char 2)
    (should (equal ""
                   (company-forge--completion-suffix "@"))))
  (with-temp-buffer
    (insert "@\n")
    (goto-char 2)
    (should (equal ""
                   (company-forge--completion-suffix "@"))))
  (with-temp-buffer
    (insert "@a-user-1")
    (should (equal ""
                   (company-forge--completion-suffix "@a-user-1"))))
  (with-temp-buffer
    (insert "@a-user-1 ")
    (should (equal ""
                   (company-forge--completion-suffix "@a-user-1"))))
  (with-temp-buffer
    (insert "@a-user-1\n")
    (should (equal ""
                   (company-forge--completion-suffix "@a-user-1"))))
  (with-temp-buffer
    (insert "@a-user-1")
    (goto-char 2)
    (should (equal "a-user-1"
                   (company-forge--completion-suffix "@"))))
  (with-temp-buffer
    (insert "@a-user-1 ")
    (goto-char 2)
    (should (equal "a-user-1"
                   (company-forge--completion-suffix "@"))))
  (with-temp-buffer
    (insert "@a-user-1\n")
    (goto-char 2)
    (should (equal "a-user-1"
                   (company-forge--completion-suffix "@"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1")
    (should (equal ""
                   (company-forge--completion-suffix "@an-org-1/a-team-1"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1 ")
    (should (equal ""
                   (company-forge--completion-suffix "@an-org-1/a-team-1"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1\n")
    (should (equal ""
                   (company-forge--completion-suffix "@an-org-1/a-team-1"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1")
    (goto-char 2)
    (should (equal "an-org-1/a-team-1"
                   (company-forge--completion-suffix "@"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1 ")
    (goto-char 2)
    (should (equal "an-org-1/a-team-1"
                   (company-forge--completion-suffix "@"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1\n")
    (goto-char 2)
    (should (equal "an-org-1/a-team-1"
                   (company-forge--completion-suffix "@"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1")
    (goto-char 10)
    (should (equal "/a-team-1"
                   (company-forge--completion-suffix "@an-org-1"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1")
    (goto-char 11)
    (should (equal "a-team-1"
                   (company-forge--completion-suffix "@an-org-1/"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1")
    (goto-char 12)
    (should (equal "-team-1"
                   (company-forge--completion-suffix "@an-org-1/a"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1 ")
    (goto-char 10)
    (should (equal "/a-team-1"
                   (company-forge--completion-suffix "@an-org-1"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1 ")
    (goto-char 11)
    (should (equal "a-team-1"
                   (company-forge--completion-suffix "@an-org-1/"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1 ")
    (goto-char 12)
    (should (equal "-team-1"
                   (company-forge--completion-suffix "@an-org-1/a"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1\n")
    (goto-char 10)
    (should (equal "/a-team-1"
                   (company-forge--completion-suffix "@an-org-1"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1\n")
    (goto-char 11)
    (should (equal "a-team-1"
                   (company-forge--completion-suffix "@an-org-1/"))))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1\n")
    (goto-char 12)
    (should (equal "-team-1"
                   (company-forge--completion-suffix "@an-org-1/a"))))
  (with-temp-buffer
    (insert "@-")
    (goto-char 2)
    (should-not (company-forge--completion-suffix "@")))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1/bad")
    (goto-char 2)
    (should-not (company-forge--completion-suffix "@")))
  (with-temp-buffer
    (insert "@an-org-1/-team-1")
    (goto-char 2)
    (should-not (company-forge--completion-suffix "@")))
  (with-temp-buffer
    (insert "#")
    (should (equal ""
                  (company-forge--completion-suffix "#"))))
  (with-temp-buffer
    (insert "# ")
    (goto-char 2)
    (should (equal ""
                  (company-forge--completion-suffix "#"))))
  (with-temp-buffer
    (insert "#\n")
    (goto-char 2)
    (should (equal ""
                  (company-forge--completion-suffix "#"))))
  (with-temp-buffer
    (insert "#1")
    (goto-char 2)
    (should (equal "1"
                   (company-forge--completion-suffix "#"))))
  (with-temp-buffer
    (insert "#1\n")
    (goto-char 2)
    (should (equal "1"
                   (company-forge--completion-suffix "#"))))
  (with-temp-buffer
    (insert "#123456789")
    (goto-char 2)
    (should (equal "123456789"
                   (company-forge--completion-suffix "#"))))
  (with-temp-buffer
    (insert "#1234567890")
    (should-not (company-forge--completion-suffix "#1234567890")))
  (with-temp-buffer
    (insert "#1234567890")
    (goto-char 2)
    (should-not (company-forge--completion-suffix "#")))
  (with-temp-buffer
    (insert "#1234567890")
    (forward-char -1)
    (should-not (company-forge--completion-suffix "#123456789")))
  (with-temp-buffer
    (insert "#a")
    (goto-char 1)
    (should-not (company-forge--completion-suffix "#"))))

(ert-deftest company-forge-t--completion-prefix ()
  (with-temp-buffer
    (should-not (company-forge--completion-prefix)))
  (with-temp-buffer
    (insert "foo")
    (should-not (company-forge--completion-prefix)))
  (with-temp-buffer
    (insert "foo")
    (forward-char -1)
    (should-not (company-forge--completion-prefix)))
  (with-temp-buffer
    (insert "@")
    (should (equal "@" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@f")
    (should (equal "@f" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@foo")
    (should (equal "@foo" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@foo")
    (forward-char -1)
    (should (equal "@fo" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@foo/")
    (should (equal "@foo/" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@foo/b")
    (should (equal "@foo/b" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@foo/bar")
    (should (equal "@foo/bar" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@foo/bar-baz-1")
    (should (equal "@foo/bar-baz-1" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@foo-")
    (should (equal "@foo-" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@foo-bar-1")
    (should (equal "@foo-bar-1" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "@-")
    (should-not (company-forge--completion-prefix)))
  (with-temp-buffer
    (insert "@/")
    (should-not (company-forge--completion-prefix)))
  (with-temp-buffer
    (insert "@foo/-")
    (should-not (company-forge--completion-prefix)))
  (with-temp-buffer
    (insert "@foo/bar/baz")
    (should-not (company-forge--completion-prefix)))

  (with-temp-buffer
    (insert "#")
    (should (equal "#" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "#1")
    (should (equal "#1" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "#1234567890")
    (should (equal "#1234567890" (company-forge--completion-prefix))))
  (with-temp-buffer
    (insert "#12345678901")
    (should-not (company-forge--completion-prefix)))
  (with-temp-buffer
    (insert "#a")
    (should-not (company-forge--completion-prefix)))
  (with-temp-buffer
    (insert "#0a")
    (should-not (company-forge--completion-prefix))))


(ert-deftest company-forge-t-grab-symbol-parts ()
  (with-temp-buffer
    (setq company-forge--type 'test)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type))
  (with-temp-buffer
    (insert "@")
    (should (equal '("" "" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?@)))
  (with-temp-buffer
    (insert "@a-user-1")
    (goto-char 4)
    (should (equal '("a-" "user-1" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?@)))
  (with-temp-buffer
    (insert "@an-org-1/a-team-1")
    (goto-char 11)
    (should (equal '("an-org-1/" "a-team-1" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?@)))
  (with-temp-buffer
    (setq company-forge--type 'test)
    (insert "@an-org-1/a-team-1/bad")
    (goto-char 11)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type))
  (with-temp-buffer
    (setq company-prefix 'test)
    (insert "@-org-1/a-team-1")
    (goto-char 11)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type))

  (with-temp-buffer
    (insert "#")
    (should (equal '("" "" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?#)))
  (with-temp-buffer
    (insert "#12")
    (goto-char 3)
    (should (equal '("1" "2" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?#)))
  (with-temp-buffer
    (setq company-forge--type 'test)
    (insert "#1a")
    (goto-char 3)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type))
  (with-temp-buffer
    (setq company-forge--type 'test)
    (insert "#a1")
    (goto-char 3)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type)))

(ert-deftest company-forge-t-match-type ()
  (let ((company-forge-match-type 'prefix))
    (let ((company-forge--type ?@))
      (should (eq (company-forge--match-type) 'prefix)))
    (let ((company-forge--type ?#))
      (should (eq (company-forge--match-type) 'prefix))))
  (let ((company-forge-match-type '(anywhere . infix)))
    (let ((company-forge--type ?@))
      (should (eq (company-forge--match-type) 'infix)))
    (let ((company-forge--type ?#))
      (should (eq (company-forge--match-type) 'anywhere)))))

(ert-deftest company-forge-t-string-match ()
  (let ((company-forge-match-type 'prefix)
        (company-forge--type ?#))
    (should (company-forge--string-match "" "123"))
    (should (company-forge--string-match "1" "123"))
    (should-not (company-forge--string-match "2" "123")))
  (let ((company-forge-match-type 'infix)
        (company-forge--type ?#))
    (should (company-forge--string-match "" "123"))
    (should (company-forge--string-match "1" "123"))
    (should-not (company-forge--string-match "2" "123")))
  (let ((company-forge-match-type 'anywhere)
        (company-forge--type ?#))
    (should (company-forge--string-match "" "123"))
    (should (company-forge--string-match "1" "123"))
    (should (company-forge--string-match "2" "123")))
  (let ((company-forge-match-type 'prefix)
        (company-forge--type ?@))
    (should (company-forge--string-match "" "foo"))
    (should (company-forge--string-match "" "foo/bar"))
    (should (company-forge--string-match "f" "foo"))
    (should (company-forge--string-match "f" "foo/bar"))
    (should-not (company-forge--string-match "o" "foo"))
    (should-not (company-forge--string-match "o" "foo/bar"))
    (should-not (company-forge--string-match "b" "foo"))
    (should-not (company-forge--string-match "b" "foo/bar"))
    (should-not (company-forge--string-match "a" "foo"))
    (should-not (company-forge--string-match "a" "foo/bar")))
  (let ((company-forge-match-type 'infix)
        (company-forge--type ?@))
    (should (company-forge--string-match "" "foo"))
    (should (company-forge--string-match "" "foo/bar"))
    (should (company-forge--string-match "f" "foo"))
    (should (company-forge--string-match "f" "foo/bar"))
    (should-not (company-forge--string-match "o" "foo"))
    (should-not (company-forge--string-match "o" "foo/bar"))
    (should-not (company-forge--string-match "b" "foo"))
    (should (company-forge--string-match "b" "foo/bar"))
    (should-not (company-forge--string-match "a" "foo"))
    (should-not (company-forge--string-match "a" "foo/bar")))
  (let ((company-forge-match-type 'anywhere)
        (company-forge--type ?@))
    (should (company-forge--string-match "" "foo"))
    (should (company-forge--string-match "" "foo/bar"))
    (should (company-forge--string-match "f" "foo"))
    (should (company-forge--string-match "f" "foo/bar"))
    (should (company-forge--string-match "o" "foo"))
    (should (company-forge--string-match "o" "foo/bar"))
    (should-not (company-forge--string-match "b" "foo"))
    (should (company-forge--string-match "b" "foo/bar"))
    (should-not (company-forge--string-match "a" "foo"))
    (should (company-forge--string-match "a" "foo/bar"))))

(ert-deftest company-forge-t-match ()
  (let ((company-forge-match-type 'anywhere))
    (let (company-prefix)
      (should-not (company-forge--match "foo-bar/foo-foo-baz")))
    (let ((company-prefix ""))
      (should-not (company-forge--match "foo-bar/foo-foo-baz")))
    (let ((company-prefix "qux"))
      (should-not  (company-forge--match "foo-bar/foo-foo-baz")))
    (let ((company-prefix "foo"))
      (should (equal '((0 . 3) (8 . 11) (12 . 15))
                     (company-forge--match "foo-bar/foo-foo-baz"))))
    (let ((company-prefix "bar"))
      (should (equal '((4 . 7))
                     (company-forge--match "foo-bar/foo-foo-baz")))))
  (let ((company-forge-match-type 'infix))
    (let (company-prefix)
      (should-not (company-forge--match "foo-bar/foo-foo-baz")))
    (let ((company-prefix ""))
      (should-not (company-forge--match "foo-bar/foo-foo-baz")))
    (let ((company-prefix "qux"))
      (should-not  (company-forge--match "foo-bar/foo-foo-baz")))
    (let ((company-prefix "foo"))
      (should (equal '((0 . 3) (8 . 11))
                     (company-forge--match "foo-bar/foo-foo-baz"))))
    (let ((company-prefix "bar"))
      (should-not (company-forge--match "foo-bar/foo-foo-baz"))))
  (let ((company-forge-match-type 'prefix))
    (let (company-prefix)
      (should-not (company-forge--match "foo-bar/foo-foo-baz")))
    (let ((company-prefix ""))
      (should-not (company-forge--match "foo-bar/foo-foo-baz")))
    (let ((company-prefix "qux"))
      (should-not  (company-forge--match "foo-bar/foo-foo-baz")))
    (let ((company-prefix "foo"))
      (should (equal '((0 . 3))
                     (company-forge--match "foo-bar/foo-foo-baz"))))
    (let ((company-prefix "bar"))
      (should-not (company-forge--match "foo-bar/foo-foo-baz")))))

(provide 'company-forge.t)

;;; company-forge.t.el ends here
