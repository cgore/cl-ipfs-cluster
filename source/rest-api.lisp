(defpackage #:ipfs-cluster/rest-api
  (:use #:common-lisp)
  (:export #:get-cluster-id
           #:get-cluster-version))
(in-package #:ipfs-cluster/rest-api)

(defvar *cluster*
  "http://127.0.0.1:9094"
  "Base URL for the IPFS Cluster")

(defun http-get-json (&rest url-components)
  "Make an HTTP GET call to the IPFS Cluster, expecting JSON back, and parse."
  (let ((yason:*parse-json-arrays-as-vectors*   t)
        (yason:*parse-json-booleans-as-symbols* t))
    (yason:parse (dex:get (apply #'concatenate 'string *cluster* url-components)))))

(defun get-cluster-id ()
  "Cluster peer information"
  (http-get-json "/id"))

(defun get-cluster-version ()
  "Cluster version"
  (http-get-json "/version"))
