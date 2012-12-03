;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :it.bese.fiveam)

;;;; * Test Suites

;;;; Test suites allow us to collect multiple tests into a single
;;;; object and run them all using asingle name. Test suites do not
;;;; affect the way test are run nor the way the results are handled,
;;;; they are simply a test organizing group.

;;;; Test suites can contain both tests and other test suites. Running
;;;; a test suite causes all of its tests and test suites to be
;;;; run. Suites do not affect test dependencies, running a test suite
;;;; can cause tests which are not in the suite to be run.

;;;; ** Creating Suits

(defvar *suites* (make-hash-table :test 'eql))

(defmacro def-suite (name &key description (in nil in-p) (fixture nil fixture-p))
  "Define a new test-suite named NAME.

NAME::
  The symbol naming the test.

DESCRIPTION::
  A string describing the contents/purpose of this suite.

IN (a symbol), if provided, causes this suite te be nested in the
suite named by `IN`. If `IN` is `NIL`, as opposed to not being passed
at all, the new suite will not be a part of any existing suite.

\[NOTE]
This macro is built on top of `make-suite` as such it, like `make-suite`,
will overrwrite any existing suite named `NAME`.

DESCRIPTION is just a string.

FIXTURE is the fixture argument (exactly like the `:fixture` argument to
def-test) to pass to tests in this suite."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (make-suite ',name
                 ,@(when description `(:description ,description))
                 ,@(when in-p      `(:in ',in))
                 ,@(when fixture-p `(:fixture ',fixture)))
     ',name))

(defmacro def-suite* (name &rest def-suite-args)
  `(progn
     (def-suite ,name ,@def-suite-args)
     (in-suite ,name)))

(declaim (special *suite*))

(defun make-suite (name &key description ((:in parent-suite) *suite*) fixture)
  "Create a new test suite object.

Overrides any existing suite named NAME."
  (let ((suite (make-instance 'test-suite :name name)))
    (%update-suite name suite description parent-suite fixture)
    (setf (gethash name *suites*) suite)
    (setf (get-test name) suite)
    suite))

(defun update-suite (name suite
                     &key description ((:in parent-suite) *suite*) fixture)
  (%update-suite name suite description parent-suite fixture))

(defun list-all-suites ()
  "Returns an unordered LIST of all suites."
  (hash-table-values *suites*))

(defun remove-from-suites (name &optional parents)
  (when (get-test name)
    ;; if this suite already exists, and its :IN some other suite, remove it.
    (dolist (s (list-all-suites))
      (let ((tests (tests s)))
        (when (and (not (member (name s) parents :test #'eq))
                   (gethash name tests))
          (remhash name tests))))))

(defun add-to-suites (name suite parents)
  (dolist (i parents)
    (let ((in-suite (get-test i)))
      (when (null in-suite)
        (cerror "Create a new suite named ~A." "Unknown suite ~A." i)
        (setf in-suite (make-suite i)
              (get-test in-suite) in-suite))
      (setf (gethash name (tests in-suite)) suite))))

(defun remove-from-add-to-suites (test-name suite parent-suite)
  ;; prevent cycles
  (unless (eq suite parent-suite)
    (let ((parents (ensure-list parent-suite)))
      (remove-from-suites test-name parents)
      (add-to-suites test-name suite parents))))

(defun %update-suite (name suite description parent-suite fixture)
  (setf (description suite) description)
  (setf (fixture suite) fixture)
  (remove-from-add-to-suites name suite parent-suite)
  suite)

;;;; ** Managing the Current Suite

(defvar *suite* (setf (get-test 'T) (make-suite 'T :description "Default global suite" :in nil))
  "The current test suite object")

(defmacro in-suite (suite-name)
  "Set the `*suite*` special variable so that all tests defined
after the execution of this form are, unless specified otherwise,
in the test-suite named `SUITE-NAME`.

See also: `DEF-SUITE` and `*SUITE*`. "
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%in-suite ,suite-name)))

(defmacro in-suite* (suite-name &rest def-suite-args)
  "Same effect as `IN-SUITE`, but if `SUITE-NAME` does not exist it
will be created (as per DEF-SUITE), or updated with the new arguments."
  `(%in-suite ,suite-name
              :fail-on-error nil
              ,@def-suite-args))

(defmacro %in-suite (suite-name
                     &key description (in nil in-p) (fixture nil fixture-p)
                          (fail-on-error t))
  (let ((def-suite-args
          `(,@(when description `(:description ,description))
            ,@(when in-p `(:in ',in))
            ,@(when fixture-p `(:fixture ',fixture)))))
    (with-gensyms (suite)
      `(progn
         (if-let (,suite (get-test ',suite-name))
           (setf *suite* (update-suite ',suite-name ,suite ,@def-suite-args))
           (progn
             ,@(when fail-on-error
                 `((cerror "Create a new suite named ~A."
                           "Unknown suite ~A." ',suite-name)))
             (setf *suite* (make-suite ',suite-name ,@def-suite-args))))
         ',suite-name))))

;; Copyright (c) 2002-2003, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
