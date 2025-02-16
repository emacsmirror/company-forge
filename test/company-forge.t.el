;;; company-forge.t.el --- Tests for company-forge   -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:
(when-let* ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file
                                          buffer-file-name))))
  (load-file (file-name-concat dir "undercover-init.el")))

(require 'company-forge)
(require 'company)
(require 'ert)
(require 'el-mock)

(defun company-forge-t-match-explainer (candidate)
                                        ; checkdoc-params: (candidate)
  "Explainer function for `company-forge--match'."
  (if-let* ((match (company-forge--match candidate)))
      `(company-prefix ,company-prefix
                       found-in-candidate ,candidate
                       company-forge-match-type ',company-forge-match-type
                       first-math-at ,(caar match))
    `(company-prefix ,company-prefix
                     not-found-in-candidate ,candidate
                     company-forge-match-type ',company-forge-match-type)))
(put 'company-forge--match 'ert-explainer 'company-forge-t-match-explainer)

(defclass company-forge-t-repository (forge-repository) nil)

(ert-deftest company-forge-t--completion-suffix-@-1 ()
  (with-temp-buffer
    (insert "@")
    (should (equal ""
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-2 ()
  (with-temp-buffer
    (insert "@ ")
    (goto-char 2)
    (should (equal ""
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-3 ()
  (with-temp-buffer
    (insert "@\n")
    (goto-char 2)
    (should (equal ""
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-4 ()
  (with-temp-buffer
    (insert "@,")
    (goto-char 2)
    (should (equal ""
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-user-1 ()
  (with-temp-buffer
    (insert "@user-1")
    (should (equal ""
                   (company-forge--completion-suffix "@user-1")))))

(ert-deftest company-forge-t--completion-suffix-@-user-2 ()
  (with-temp-buffer
    (insert "@user-2 ")
    (should (equal ""
                   (company-forge--completion-suffix "@user-2")))))

(ert-deftest company-forge-t--completion-suffix-@-user-3 ()
  (with-temp-buffer
    (insert "@user-3\n")
    (should (equal ""
                   (company-forge--completion-suffix "@user-3")))))

(ert-deftest company-forge-t--completion-suffix-@-user-4 ()
  (with-temp-buffer
    (insert "@user-4,")
    (should (equal ""
                   (company-forge--completion-suffix "@user-4")))))

(ert-deftest company-forge-t--completion-suffix-@-user-5 ()
  (with-temp-buffer
    (insert "@user-5")
    (goto-char 2)
    (should (equal "user-5"
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-user-6 ()
  (with-temp-buffer
    (insert "@user-6 ")
    (goto-char 2)
    (should (equal "user-6"
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-user-7 ()
  (with-temp-buffer
    (insert "@user-7\n")
    (goto-char 2)
    (should (equal "user-7"
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-user-8 ()
  (with-temp-buffer
    (insert "@user-8,")
    (goto-char 2)
    (should (equal "user-8"
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-team-1 ()
  (with-temp-buffer
    (insert "@org-1/team-1")
    (should (equal ""
                   (company-forge--completion-suffix "@org-1/team-1")))))

(ert-deftest company-forge-t--completion-suffix-@-team-2 ()
  (with-temp-buffer
    (insert "@org-1/team-2 ")
    (should (equal ""
                   (company-forge--completion-suffix "@org-1/team-2")))))

(ert-deftest company-forge-t--completion-suffix-@-team-3 ()
  (with-temp-buffer
    (insert "@org-1/team-3\n")
    (should (equal ""
                   (company-forge--completion-suffix "@org-1/team-3")))))

(ert-deftest company-forge-t--completion-suffix-@-team-4 ()
  (with-temp-buffer
    (insert "@org-1/team-4,")
    (should (equal ""
                   (company-forge--completion-suffix "@org-1/team-4")))))

(ert-deftest company-forge-t--completion-suffix-@-team-5 ()
  (with-temp-buffer
    (insert "@org-1/team-5")
    (goto-char 2)
    (should (equal "org-1/team-5"
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-team-6 ()
  (with-temp-buffer
    (insert "@org-1/team-6 ")
    (goto-char 2)
    (should (equal "org-1/team-6"
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-team-7 ()
  (with-temp-buffer
    (insert "@org-1/team-7\n")
    (goto-char 2)
    (should (equal "org-1/team-7"
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-team-8 ()
  (with-temp-buffer
    (insert "@org-1/team-8,")
    (goto-char 2)
    (should (equal "org-1/team-8"
                   (company-forge--completion-suffix "@")))))

(ert-deftest company-forge-t--completion-suffix-@-team-9 ()
  (with-temp-buffer
    (insert "@org-1/team-9")
    (goto-char 7)
    (should (equal "/team-9"
                   (company-forge--completion-suffix "@org-1")))))

(ert-deftest company-forge-t--completion-suffix-@-team-10 ()
  (with-temp-buffer
    (insert "@org-1/team-10")
    (goto-char 8)
    (should (equal "team-10"
                   (company-forge--completion-suffix "@org-1/")))))

(ert-deftest company-forge-t--completion-suffix-@-team-11 ()
  (with-temp-buffer
    (insert "@org-1/team-11")
    (goto-char 9)
    (should (equal "eam-11"
                   (company-forge--completion-suffix "@org-1/t")))))

(ert-deftest company-forge-t--completion-suffix-@-team-12 ()
  (with-temp-buffer
    (insert "@org-1/team-12 ")
    (goto-char 7)
    (should (equal "/team-12"
                   (company-forge--completion-suffix "@org-1")))))

(ert-deftest company-forge-t--completion-suffix-@-team-13 ()
  (with-temp-buffer
    (insert "@org-1/team-13 ")
    (goto-char 8)
    (should (equal "team-13"
                   (company-forge--completion-suffix "@org-1/")))))

(ert-deftest company-forge-t--completion-suffix-@-team-14 ()
  (with-temp-buffer
    (insert "@org-1/team-14 ")
    (goto-char 9)
    (should (equal "eam-14"
                   (company-forge--completion-suffix "@org-1/t")))))

(ert-deftest company-forge-t--completion-suffix-@-team-15 ()
  (with-temp-buffer
    (insert "@org-1/team-15\n")
    (goto-char 7)
    (should (equal "/team-15"
                   (company-forge--completion-suffix "@org-1")))))

(ert-deftest company-forge-t--completion-suffix-@-team-16 ()
  (with-temp-buffer
    (insert "@org-1/team-16\n")
    (goto-char 8)
    (should (equal "team-16"
                   (company-forge--completion-suffix "@org-1/")))))

(ert-deftest company-forge-t--completion-suffix-@-team-17 ()
  (with-temp-buffer
    (insert "@org-1/team-17\n")
    (goto-char 9)
    (should (equal "eam-17"
                   (company-forge--completion-suffix "@org-1/t")))))

(ert-deftest company-forge-t--completion-suffix-@-team-18 ()
  (with-temp-buffer
    (insert "@org-1/team-18,")
    (goto-char 7)
    (should (equal "/team-18"
                   (company-forge--completion-suffix "@org-1")))))

(ert-deftest company-forge-t--completion-suffix-@-team-19 ()
  (with-temp-buffer
    (insert "@org-1/team-19,")
    (goto-char 8)
    (should (equal "team-19"
                   (company-forge--completion-suffix "@org-1/")))))

(ert-deftest company-forge-t--completion-suffix-@-team-20 ()
  (with-temp-buffer
    (insert "@org-1/team-20,")
    (goto-char 9)
    (should (equal "eam-20"
                   (company-forge--completion-suffix "@org-1/t")))))

(ert-deftest company-forge-t--completion-suffix-@-error-1 ()
  (with-temp-buffer
    (insert "@-")
    (goto-char 2)
    (should-not (company-forge--completion-suffix "@"))))

(ert-deftest company-forge-t--completion-suffix-@-error-2 ()
  (with-temp-buffer
    (insert "@org-1/team-1/bad")
    (goto-char 2)
    (should-not (company-forge--completion-suffix "@"))))

(ert-deftest company-forge-t--completion-suffix-@-error-3 ()
  (with-temp-buffer
    (insert "@org-1/-team-1")
    (goto-char 2)
    (should-not (company-forge--completion-suffix "@"))))

(ert-deftest company-forge-t--completion-suffix-hash-1 ()
  (with-temp-buffer
    (insert "#")
    (should (equal ""
                  (company-forge--completion-suffix "#")))))

(ert-deftest company-forge-t--completion-suffix-hash-2 ()
  (with-temp-buffer
    (insert "# ")
    (goto-char 2)
    (should (equal ""
                  (company-forge--completion-suffix "#")))))

(ert-deftest company-forge-t--completion-suffix-hash-3 ()
  (with-temp-buffer
    (insert "#\n")
    (goto-char 2)
    (should (equal ""
                  (company-forge--completion-suffix "#")))))

(ert-deftest company-forge-t--completion-suffix-hash-4 ()
  (with-temp-buffer
    (insert "#,")
    (goto-char 2)
    (should (equal ""
                  (company-forge--completion-suffix "#")))))

(ert-deftest company-forge-t--completion-suffix-hash-topic-1 ()
  (with-temp-buffer
    (insert "#1")
    (goto-char 2)
    (should (equal "1"
                   (company-forge--completion-suffix "#")))))

(ert-deftest company-forge-t--completion-suffix-hash-topic-2 ()
  (with-temp-buffer
    (insert "#2\n")
    (goto-char 2)
    (should (equal "2"
                   (company-forge--completion-suffix "#")))))

(ert-deftest company-forge-t--completion-suffix-hash-topic-3 ()
  (with-temp-buffer
    (insert "#3,")
    (goto-char 2)
    (should (equal "3"
                   (company-forge--completion-suffix "#")))))

(ert-deftest company-forge-t--completion-suffix-hash-topic-123456789 ()
  (with-temp-buffer
    (insert "#123456789")
    (goto-char 2)
    (should (equal "123456789"
                   (company-forge--completion-suffix "#")))))

(ert-deftest company-forge-t--completion-suffix-hash-topic-1234567890 ()
  (with-temp-buffer
    (insert "#123456790,")
    (goto-char 2)
    (should (equal "123456790"
                   (company-forge--completion-suffix "#")))))

(ert-deftest company-forge-t--completion-suffix-hash-topic-1234567890-1 ()
  (with-temp-buffer
    (insert "#1234567890")
    (should-not (company-forge--completion-suffix "#1234567890"))))

(ert-deftest company-forge-t--completion-suffix-hash-topic-1234567890-2 ()
  (with-temp-buffer
    (insert "#1234567890")
    (goto-char 2)
    (should-not (company-forge--completion-suffix "#"))))

(ert-deftest company-forge-t--completion-suffix-hash-topic-1234567890-3 ()
  (with-temp-buffer
    (insert "#1234567890")
    (forward-char -1)
    (should-not (company-forge--completion-suffix "#123456789"))))

(ert-deftest company-forge-t--completion-suffix-hash-topic-a ()
  (with-temp-buffer
    (insert "#a")
    (goto-char 1)
    (should-not (company-forge--completion-suffix "#"))))

(ert-deftest company-forge-t--completion-prefix-empty ()
  (with-temp-buffer
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-no-@-no-hash-1 ()
  (with-temp-buffer
    (insert "foo")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-no-@-no-hash-2 ()
  (with-temp-buffer
    (insert "foo")
    (forward-char -1)
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-@ ()
  (with-temp-buffer
    (insert "@")
    (should (equal "@" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-u ()
  (with-temp-buffer
    (insert "@u")
    (should (equal "@u" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-user-1 ()
  (with-temp-buffer
    (insert "@user-1")
    (should (equal "@user-1" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-user-2 ()
  (with-temp-buffer
    (insert "@user-2")
    (forward-char -1)
    (should (equal "@user-" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-user-3 ()
  (with-temp-buffer
    (insert " @user-1")
    (should (equal "@user-1" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-org-1 ()
  (with-temp-buffer
    (insert "@org-1/")
    (should (equal "@org-1/" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-org-2 ()
  (with-temp-buffer
    (insert "@org-2/")
    (forward-char -1)
    (should (equal "@org-2" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-org-3-u ()
  (with-temp-buffer
    (insert "@org-3/u")
    (should (equal "@org-3/u" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-org-4-u ()
  (with-temp-buffer
    (insert "@org-4/u")
    (forward-char -1)
    (should (equal "@org-4/" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-org-5-user-1 ()
  (with-temp-buffer
    (insert "@org-5/user-1")
    (should (equal "@org-5/user-1" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-org-5-user-2 ()
  (with-temp-buffer
    (insert "@org-5/user-2")
    (forward-char -1)
    (should (equal "@org-5/user-" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-org-6-user-3 ()
  (with-temp-buffer
    (insert " @org-6/user-3")
    (should (equal "@org-6/user-3" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-@-error-1 ()
  (with-temp-buffer
    (insert "@-")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-@-error-2 ()
  (with-temp-buffer
    (insert "@/")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-@-error-3 ()
  (with-temp-buffer
    (insert "@foo/-")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-@-error-4 ()
  (with-temp-buffer
    (insert "@foo/bar/baz")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-@-middle ()
  (with-temp-buffer
    (insert "foo@bar")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-hash-1 ()
  (with-temp-buffer
    (insert "#")
    (should (equal "#" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-hash-2 ()
  (with-temp-buffer
    (insert "#2")
    (should (equal "#2" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-hash-3 ()
  (with-temp-buffer
    (insert " #2")
    (should (equal "#2" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-hash-1234567890 ()
  (with-temp-buffer
    (insert "#1234567890")
    (should (equal "#1234567890" (company-forge--completion-prefix)))))

(ert-deftest company-forge-t--completion-prefix-hash-error-12345678901 ()
  (with-temp-buffer
    (insert "#12345678901")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-hash-error-a ()
  (with-temp-buffer
    (insert "#a")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-hash-error-0a ()
  (with-temp-buffer
    (insert "#0a")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--completion-prefix-hash-middle ()
  (with-temp-buffer
    (insert "1#2")
    (should-not (company-forge--completion-prefix))))

(ert-deftest company-forge-t--grab-symbol-parts-empty ()
  (with-temp-buffer
    (setq company-forge--type 'test)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type)))

(ert-deftest company-forge-t--grab-symbol-parts-@ ()
  (with-temp-buffer
    (insert "@")
    (should (equal '("" "" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?@))))

(ert-deftest company-forge-t--grab-symbol-parts-@-user-1 ()
  (with-temp-buffer
    (insert "@user-1")
    (goto-char 4)
    (should (equal '("us" "er-1" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?@))))

(ert-deftest company-forge-t--grab-symbol-parts-@-org-1-user-1 ()
  (with-temp-buffer
    (insert "@org-1/team-1")
    (goto-char 8)
    (should (equal '("org-1/" "team-1" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?@))))

(ert-deftest company-forge-t--grab-symbol-parts-@-error-/ ()
  (with-temp-buffer
    (setq company-forge--type 'test)
    (insert "@org-1/team-1/bad")
    (goto-char 8)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type)))

(ert-deftest company-forge-t--grab-symbol-parts-@-error-org ()
  (with-temp-buffer
    (setq company-prefix 'test)
    (insert "@-org-1/team-1")
    (goto-char 9)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type)))

(ert-deftest company-forge-t--grab-symbol-parts-@-error-team ()
  (with-temp-buffer
    (setq company-prefix 'test)
    (insert "@org-1/-team-1")
    (goto-char 8)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type)))

(ert-deftest company-forge-t--grab-symbol-parts-hash ()
  (with-temp-buffer
    (insert "#")
    (should (equal '("" "" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?#))))

(ert-deftest company-forge-t--grab-symbol-parts-hash-12 ()
  (with-temp-buffer
    (insert "#12")
    (goto-char 3)
    (should (equal '("1" "2" t)
                   (company-forge--grab-symbol-parts)))
    (should (eq company-forge--type ?#))))

(ert-deftest company-forge-t--grab-symbol-parts-hash-error-1a ()
  (with-temp-buffer
    (setq company-forge--type 'test)
    (insert "#1a")
    (goto-char 3)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type)))

(ert-deftest company-forge-t--grab-symbol-parts-hash-error-a1 ()
  (with-temp-buffer
    (setq company-forge--type 'test)
    (insert "#a1")
    (goto-char 3)
    (should-not (company-forge--grab-symbol-parts))
    (should-not company-forge--type)))

(ert-deftest company-forge-t--prefix-basic ()
  (let ((company-forge-predicate '(derived-mode . fundamental-mode)))
    (with-temp-buffer
      (insert "@")
      (should (equal (company-forge--prefix)
                     '("" "" t))))))

(ert-deftest company-forge-t--prefix-wrong-mode ()
  (let ((company-forge-predicate '(not (derived-mode . fundamental-mode))))
    (with-temp-buffer
      (insert "@")
      (should-not (company-forge--prefix)))))

(ert-deftest company-forge-t--prefix-no-prefix ()
  (let ((company-forge-predicate '(derived-mode . fundamental-mode)))
    (with-temp-buffer
      (should-not (company-forge--prefix)))))

(ert-deftest company-forge-t--match-type-atom ()
  (let ((company-forge-match-type 'prefix))
    (let ((company-forge--type ?@))
      (should (eq (company-forge--match-type) 'prefix)))
    (let ((company-forge--type ?#))
      (should (eq (company-forge--match-type) 'prefix)))))

(ert-deftest company-forge-t--match-type-cons ()
  (let ((company-forge-match-type '(anywhere . infix)))
    (let ((company-forge--type ?@))
      (should (eq (company-forge--match-type) 'infix)))
    (let ((company-forge--type ?#))
      (should (eq (company-forge--match-type) 'anywhere)))))

(ert-deftest company-forge-t--string-match-prefix-hash ()
  (let ((company-forge-match-type 'prefix)
        (company-forge--type ?#))
    (should (company-forge--string-match "" "123"))
    (should (company-forge--string-match "1" "123"))
    (should-not (company-forge--string-match "2" "123"))))

(ert-deftest company-forge-t--string-match-infix-hash ()
  (let ((company-forge-match-type 'infix)
        (company-forge--type ?#))
    (should (company-forge--string-match "" "123"))
    (should (company-forge--string-match "1" "123"))
    (should-not (company-forge--string-match "2" "123"))))

(ert-deftest company-forge-t--string-match-anywhere-hash ()
  (let ((company-forge-match-type 'anywhere)
        (company-forge--type ?#))
    (should (company-forge--string-match "" "123"))
    (should (company-forge--string-match "1" "123"))
    (should (company-forge--string-match "2" "123"))))

(ert-deftest company-forge-t--string-match-prefix-@ ()
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
    (should-not (company-forge--string-match "a" "foo/bar"))))

(ert-deftest company-forge-t--string-match-infix-@ ()
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
    (should-not (company-forge--string-match "a" "foo/bar"))))

(ert-deftest company-forge-t--string-match-anywhere-@ ()
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

(ert-deftest company-forge-t--match-anywhere ()
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
                     (company-forge--match "foo-bar/foo-foo-baz"))))))

(ert-deftest company-forge-t--match-infix ()
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
      (should-not (company-forge--match "foo-bar/foo-foo-baz")))))

(ert-deftest company-forge-t--match-prefix ()
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

(ert-deftest company-forge-t--assignees ()
  (let ((company-forge-match-type 'anywhere)
        (company-forge--repo (company-forge-t-repository)))
    (oset company-forge--repo
          teams '("org-1/team-1"
                  "org-2/team-2"))
    (oset company-forge--repo
          assignees '((1 "user-1" "Full Name 1")
                      (2 "user-2" "Full Name 2")))
    (should-not (cl-set-exclusive-or
                 (company-forge--assignees "-2")
                 (list (propertize "org-2/team-2"
                                   'company-forge-kind 'team)
                       (propertize "user-2"
                                   'company-forge-annotation "Full Name 2"
                                   'company-forge-kind 'user))
                 :test #'equal))))

(ert-deftest company-forge-t--topics ()
  (let ((company-forge-match-type 'prefix)
        (company-forge--repo 'repo))
    (mocklet (((forge--list-topics * 'repo) =>
               (list (forge-issue :id "id-12"
                                  :state 'closed
                                  :number 12
                                  :title "Issue 12")
                     (forge-pullreq :id "id-13"
                                    :state 'open
                                    :draft-p t
                                    :number 13
                                    :title "Pull Request 13")
                     (forge-issue :id "id-20"
                                  :state 'open
                                  :number 20
                                  :title "Issue 20")
                     (forge-pullreq :id "id-14"
                                    :state 'open
                                    :draft-p nil
                                    :number 14
                                    :title "Pull Request 14")
                     (forge-issue :id "id-11"
                                  :state 'open
                                  :number 11
                                  :title "Issue 11")
                     (forge-pullreq :id "id-15"
                                    :state 'merged
                                    :number 15
                                    :title "Pull Request 15")
                     (forge-pullreq :id "id-16"
                                    :state 'rejected
                                    :number 16
                                    :title "Pull Request 16")
                     (forge-pullreq :id "id-30"
                                    :state 'open
                                    :number 30
                                    :title "Pull Request 30"))))
      (should (equal
               (company-forge--topics "1")
               (list (propertize "12"
                                 'company-forge-id "id-12"
                                 'company-forge-annotation "Issue 12"
                                 'company-forge-kind 'issue-closed)
                     (propertize "13"
                                 'company-forge-id "id-13"
                                 'company-forge-annotation "Pull Request 13"
                                 'company-forge-kind 'pullreq)
                     (propertize "14"
                                 'company-forge-id "id-14"
                                 'company-forge-annotation "Pull Request 14"
                                 'company-forge-kind 'pullreq-draft)
                     (propertize "11"
                                 'company-forge-id "id-11"
                                 'comaany-forge-annotation "Issue 11"
                                 'company-forge-kind 'issue)
                     (propertize "15"
                                 'company-forge-id "id-15"
                                 'company-forge-annotation "Pull Request 15"
                                 'company-forge-kind 'pullreq-merged)
                     (propertize "16"
                                 'company-forge-id "id-16"
                                 'company-forge-annotation "Pull Request 16"
                                 'company-forge-kind 'pullreq-rejected)))))))

(provide 'company-forge.t)

;;; company-forge.t.el ends here
