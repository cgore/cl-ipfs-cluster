(defpackage #:ipfs-cluster
  (:use #:cl)
  (:export "cluster-id"))
(in-package #:ipfs-cluster)

(defvar *cluster*
  "http://127.0.0.1:9094"
  "Base URL for the IPFS Cluster")

(defun cluster-id ()
  "Cluster peer information"
  (dex:get (concatenate 'string *cluster* "/id")))
