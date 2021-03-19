(defpackage #:ipfs-cluster
  (:use #:common-lisp)
  (:export "cluster-id"))
(in-package #:ipfs-cluster)

(defun cluster-id ()
  "Cluster ID"
  (gethash "id" (ipfs-cluster/rest-api:get-cluster-id)))

(defun cluster-version ()
  "Cluster version"
  (gethash "version" (ipfs-cluster/rest-api:get-cluster-version)))
