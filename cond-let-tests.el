;;; cond-let-tests.el --- Tests for Cond-Let  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Jonas Bernoulli

;; Authors: Jonas Bernoulli <emacs.cond-let@jonas.bernoulli.dev>
;; Homepage: https://github.com/tarsius/cond-let

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'pp)

(require 'cond-let)

;;; Utilities

(defmacro cond-let-test--with-deterministic-gensym (&rest body)
  (declare (indent 0))
  `(let ((prefixes '("anon" ":while-let*"
                     ":while-let" ":if-let*"
                     ":cond-let*" ":cond-let"))
         (counter 0))
     (cl-letf
         (((symbol-function #'gensym)
           (lambda (&optional prefix)
             (if (member prefix prefixes)
                 (intern (format "%s%d" prefix (cl-incf counter)))
               (let ((num (prog1 gensym-counter (cl-incf gensym-counter))))
                 (make-symbol (format "%s%d" (or prefix "g") num)))))))
       ,@body)))

(defmacro cond-let-test--macroexpansion (all &optional value form expansion) ;TEMP optional
  (declare (indent 2))
  `(cond-let-test--with-deterministic-gensym
     (should (equal (,(if all #'macroexpand-all #'macroexpand) ',form)
                    ',expansion))
     (let ((standard-output #'ignore))
       (should (equal ,form ,value)))))

(defun cond-let-test--pp-macroexpand-last-sexp (all)
  (interactive "P")
  (cond-let-test--with-deterministic-gensym
    (insert "\n\n")
    (let ((beg (point)))
      (insert (pp-to-string (funcall (if all #'macroexpand-all #'macroexpand)
                                     (pp-last-sexp))))
      (indent-region beg (point)))))

;;; Cond

(ert-deftest cond-let-test--101-expand--cond-let* ()
  (cond-let-test--macroexpansion nil '(3 4)
    (cond-let*
      ((< 1 0)
       1)
      ([b 2]
       [_ (> b 2)]
       b)
      [[c 3]
       [d (+ c 1)]]
      ([e 5]
       [_ (> e (+ c d))]
       (print e)
       e)
      (t
       (list c d)))

    (catch ':cond-let*1
      (when (< 1 0)
        (throw ':cond-let*1 1))
      (cond-let--when-let*
          ((b 2)
           (_ (> b 2)))
        (throw ':cond-let*1 b))
      (let* ((c 3)
             (d (+ c 1)))
        (cond-let--when-let*
            ((e 5)
             (_ (> e (+ c d))))
          (throw ':cond-let*1 (progn (print e) e)))
        (when t
          (throw ':cond-let*1 (list c d))))))

  (cond-let-test--macroexpansion nil '(2 3)
    (cond-let*
      ([a nil]
       (- a))
      [[b 2]]
      ([c 3]
       [_ (> 3 b)]
       (list b c)))

    (catch ':cond-let*1
      (cond-let--when-let ((a nil))
        (throw ':cond-let*1 (- a)))
      (let ((b 2))
        (cond-let--when-let*
            ((c 3)
             (_ (> 3 b)))
          (throw ':cond-let*1 (list b c))))))

  (cond-let-test--macroexpansion nil '(1 2 3)
    (cond-let*
      [[a 1]
       [b 2]]
      ((> 3 (+ a b)) 'c)
      [[d 3]]
      ((= (+ a b) d)
       (list a b d)))

    (catch ':cond-let*1
      (let* ((a 1)
             (b 2))
        (when (> 3 (+ a b))
          (throw ':cond-let*1 'c))
        (let ((d 3))
          (when (= (+ a b) d)
            (throw ':cond-let*1 (list a b d))))))))

(ert-deftest cond-let-test--102-expand--cond-let ()
  (cond-let-test--macroexpansion nil '(nil clause nil)
    (cond-let
      [[a nil]
       [b 'shared]
       [c nil]]
      ([_(eq a c)]
       [b 'clause]
       (list a b c))
      (b))

    (catch ':cond-let1
      (let ((a nil)
            (b 'shared)
            (c nil))
        (cond-let--when-let
            ((_ (eq a c))
             (b 'clause))
          (throw ':cond-let1 (list a b c)))
        (let ((anon2 b))
          (when anon2
            (throw ':cond-let1 anon2))))))

  (cond-let-test--macroexpansion nil 'shared
    (cond-let
      [[a 'shared]]
      ([a 'clause]
       [b nil]
       (list a b))
      ((eq a 'shared)
       a))

    (catch ':cond-let1
      (let ((a 'shared))
        (cond-let--when-let
            ((a 'clause)
             (b nil))
          (throw ':cond-let1 (list a b)))
        (when (eq a 'shared)
          (throw ':cond-let1 a))))))

;;; And

(ert-deftest cond-let-test--103-expand--and-let* ()
  (cond-let-test--macroexpansion nil 3

    (and-let* ((_ t)
               (b 1)
               (c (1+ b))
               (_ (> c b))
               (_ (= c 2)))
      (+ b c))

    (let* ((anon1 t)
           (b     (and anon1 1))
           (c     (and b     (1+ b)))
           (anon2 (and c     (> c b)))
           (anon3 (and anon2 (= c 2))))
      (and anon3
           (+ b c))))

  (cond-let-test--macroexpansion nil 2

    (and-let* ((a 1))
      (1+ a))

    (let* ((a 1))
      (and a
           (1+ a))))

  (cond-let-test--macroexpansion nil 2

    (and-let* ((_ 1))
      2)

    (let* ((anon1 1))
      (and anon1 2))))

(ert-deftest cond-let-test--104-expand--and-let ()
  (cond-let-test--macroexpansion nil 3

    (and-let ((a 1)
              (b 2))
      (+ a b))

    (let (anon1 anon2)
      (and (setq anon1 1)
           (setq anon2 2)
           (let ((a anon1)
                 (b anon2))
             (+ a b)))))

  (cond-let-test--macroexpansion nil 6

    (and-let ((_ 1)
              (a 2)
              (_ 3)
              (b 4)
              (_ 5))
      (+ a b))

    (let (anon1 anon2)
      (and 1
           (setq anon1 2)
           3
           (setq anon2 4)
           5
           (let ((a anon1)
                 (b anon2))
             (+ a b)))))

  (cond-let-test--macroexpansion nil 2

    (and-let ((a 1))
      (1+ a))

    (let ((a 1))
      (and a
           (1+ a))))

  (cond-let-test--macroexpansion nil 2

    (and-let ((_ 1))
      2)

    (let ((anon1 1))
      (and anon1 2))))

(ert-deftest cond-let-test--105-expand--and$ ()
  (cond-let-test--macroexpansion nil 3

    (and$ (+ 0 1)
          (+ $ 2))

    (let (($ (+ 0 1)))
      (and $
           (+ $ 2)))))

(ert-deftest cond-let-test--106-expand--and> ()
  (cond-let-test--macroexpansion nil 6

    (and> (+ 0 1)
          (+ $ 2)
          (+ $ 3))

    (let* (($ (+ 0 1))
           ($ (and $ (+ $ 2))))
      (and $
           (+ $ 3))))

  (cond-let-test--macroexpansion nil 4

    (and> (+ 1 1)
          (+ $ 2))

    (let (($ (+ 1 1)))
      (and $
           (+ $ 2)))))

;;; If

(ert-deftest cond-let-test--107-expand--if-let* ()
  (cond-let-test--macroexpansion nil '(1 2)

    (if-let* ((a 1)
              (b 2))
        (list a b)
      (print 3)
      4)

    (catch ':if-let*1
      (let* ((a 1)
             (b (and a 2)))
        (when b
          (throw ':if-let*1 (list a b))))
      (print 3)
      4)))

(ert-deftest cond-let-test--108-expand--if-let ()
  (cond-let-test--macroexpansion nil '(1 2)

    (if-let ((a 1)
             (b 2))
        (list a b)
      3)

    (let (anon1 anon2)
      (if (and (setq anon1 1)
               (setq anon2 2))
          (let ((a anon1)
                (b anon2))
            (list a b))
        3)))

  (cond-let-test--macroexpansion nil '(1)

    (if-let ((a 1))
        (list a)
      2)

    (let (anon1)
      (if (setq anon1 1)
          (let ((a anon1))
            (list a))
        2))))

;;; When

(ert-deftest cond-let-test--109-expand--when-let* ()
  (cond-let-test--macroexpansion nil 2

    (when-let* ((_ t)
                (b 1)
                (c (1+ b))
                (_ (> c b))
                (_ (= c 2)))
      (print b)
      (print c))

    (let* ((anon1 t)
           (b     (and anon1 1))
           (c     (and b (1+ b)))
           (anon2 (and c (> c b)))
           (anon3 (and anon2 (= c 2))))
      (when anon3
        (print b)
        (print c))))

  (cond-let-test--macroexpansion nil 1

    (when-let* ((a 1))
      (print a))

    (let* ((a 1))
      (when a
        (print a))))

  (cond-let-test--macroexpansion nil 2

    (when-let* ((_ 1))
      2)

    (let* ((anon1 1))
      (when anon1
        2))))

(ert-deftest cond-let-test--110-expand--when-let ()
  (cond-let-test--macroexpansion nil 2

    (when-let ((a 1)
               (b 2))
      (print a)
      (print b))

    (let (anon1 anon2)
      (when (and (setq anon1 1)
                 (setq anon2 2))
        (let ((a anon1)
              (b anon2))
          (print a)
          (print b)))))

  (cond-let-test--macroexpansion nil 4

    (when-let ((_ 1)
               (b 2)
               (_ 3)
               (d 4)
               (_ 5))
      (print b)
      (print d))

    (let (anon1 anon2)
      (when (and 1
                 (setq anon1 2)
                 3
                 (setq anon2 4)
                 5)
        (let ((b anon1)
              (d anon2))
          (print b)
          (print d)))))

  (cond-let-test--macroexpansion nil 1

    (when-let ((a 1))
      (print a))

    (let ((a 1))
      (when a
        (print a))))

  (cond-let-test--macroexpansion nil 2

    (when-let ((_ 1))
      2)

    (let ((anon1 1))
      (when anon1 2))))

;;; While

(ert-deftest cond-let-test--111-expand--while-let* ()
  (let ((n 5))
    (cond-let-test--macroexpansion nil nil
      (while-let* ((a (setq n (1- n)))
                   (_ (> a 0)))
        (print a))

      (catch ':while-let*2
        (while t
          (let* ((a     (setq n (1- n)))
                 (anon1 (and a (> a 0))))
            (if anon1
                (print a)
              (throw ':while-let*2 nil))))))))

(ert-deftest cond-let-test--112-expand--while-let ()
  (let ((n 5))
    (cond-let-test--macroexpansion nil nil
      (while-let ((a (setq n (1- n)))
                  (_ (> n 0)))
        (print a))

      (catch ':while-let2
        (while t
          (let (anon1)
            (if (and (setq anon1 (setq n (1- n)))
                     (> n 0))
                (let ((a anon1))
                  (print a))
              (throw ':while-let2 nil))))))))


;;; Shorthands

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"      . "cond-let--and$")
;;   ("and>"      . "cond-let--and>")
;;   ("and-let"   . "cond-let--and-let")
;;   ("if-let"    . "cond-let--if-let")
;;   ("when-let"  . "cond-let--when-let")
;;   ("while-let" . "cond-let--while-let"))
;; End:

;;; cond-let-tests.el ends here
