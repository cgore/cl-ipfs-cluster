(defpackage ipfs-cluster/system
  (:use :common-lisp
        :asdf)
  (:export :author
           :copyright
           :version-string
           :version-list
           :version-major
           :version-minor
           :version-revision))
(in-package :ipfs-cluster/system)

(defparameter author "Christopher Mark Gore <cgore@cgore.com>")
(defparameter copyright "Copyright © 2021 Christopher Mark Gore, all rights reserved.")
(defparameter version-major    0)
(defparameter version-minor    0)
(defparameter version-revision 1)

(defun version-list ()
  (list version-major version-minor version-revision))

(defun version-string ()
  (format nil "~{~A.~A.~A~}" (version-list)))

(defsystem "ipfs-cluster"
  :description "Library for interfacing with IPFS Clusters. "
  :version #.(version-string)
  :author author
  :license "BSD 3-Clause"
  :depends-on ("alexandria" "dexador" "yason")
  :components ((:module "source" :components ((:file "id")
                                              (:file "rest-api" :depends-on ("id"))
                                              (:file "main"     :depends-on ("rest-api"))))))
