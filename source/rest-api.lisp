(defpackage #:ipfs-cluster/rest-api
  (:use :common-lisp)
  (:export :get-cluster-id
           :get-cluster-version
           :get-cluster-peers
           :delete-peer
           ;; TODO #:post-add
           :get-allocations
           :get-pins
           :post-pins-sync
           :post-pins
           :delete-pins
           :post-cid-sync
           :post-cid-recover
           :post-recover
           :get-monitor-metrics
           :get-monitor-metrics-freespace
           :get-monitor-metrics-ping
           :get-health-alerts
           :get-health-graph
           :post-ipfs-gc))
(in-package #:ipfs-cluster/rest-api)

(defvar *cluster*
  "http://127.0.0.1:9094"
  "Base URL for the IPFS Cluster")

(defmacro with-cluster (cluster &body body)
  "Simple wrapper macro to operate on a specified cluster."
  `(let ((*cluster* ,cluster))
     ,@body))

(defgeneric url-component (thing))

(defmethod url-component ((string string))
  string)

(defmethod url-component ((cid ipfs-cluster/id:cid))
  (ipfs-cluster/id:cid cid))

(defmethod url-component ((ipns ipfs-cluster/id:ipns))
  (concatenate 'string "ipns/" (ipfs-cluster/id:key ipns)))

(defmethod url-component ((ipld ipfs-cluster/id:ipld))
  (concatenate 'string "ipld/" (ipfs-cluster/id:cid ipld)))

(defun build-url (&rest url-components)
  (apply #'concatenate 'string *cluster* url-components))

(defun http-get-json (&rest url-components)
  "Make an HTTP GET call to the IPFS Cluster, expecting JSON back, and parse."
  (let ((yason:*parse-json-arrays-as-vectors*   t)
        (yason:*parse-json-booleans-as-symbols* t))
    (yason:parse (dex:get (apply #'build-url url-components)))))

(defun http-post (&rest url-components)
  "Make an HTTP POST call to the IPFS Cluster."
  (dex:post (apply #'build-url url-components)))

(defun http-delete (&rest url-components)
  "Make an HTTP DELETE call to the IPFS Cluster."
  (dex:delete (apply #'build-url url-components)))

(defun get-cluster-id ()
  "Cluster peer information"
  (http-get-json "/id"))

(defun get-cluster-version ()
  "Cluster version"
  (http-get-json "/version"))

(defun get-cluster-peers ()
  "Cluster peers"
  (http-get-json "/peers"))

(defun delete-peer (peer-id)
  "Remove a peer"
  (http-delete "/peers/" peer-id))

;; TODO post-add

(defun get-allocations (&optional cid)
  "List of pins and their allocations (pinset).
If a CID is specified, show a single pin and its allocations (from the pinset)."
  (if cid
      (http-get-json "/allocations/" cid)
      (http-get-json "/allocations")))

(defun get-pins (&optional cid)
  "Local status of all tracked CIDs
If a CID is specified, local status of single CIDLocal status of single CID."
  (if cid
      (http-get-json "/pins/" cid)
      (http-get-json "/pins")))

(defun post-pins-sync ()
  "Sync local status from IPFS"
  (http-post "/pins/sync"))

(defun post-cid-sync (cid)
  "Sunc a CID"
  (http-post "/" cid "/sync"))

(defun post-pins (id)
  "Pin using an IPFS/IPNS/IPLD path"
  (http-post "/pins/" (url-component id)))

(defun delete-pins (id)
  "Unpin a CID"
  (http-delete "/pins/" (url-component id)))

(defun post-cid-recover (cid)
  "Recover a CID"
  (http-post "/" cid "/recover"))

(defun post-recover ()
  "Recover all pins in the receiving Cluster peer"
  (http-post "/recover"))

(defun get-monitor-metrics (&optional metric)
  "Get a list of current metrics seen by this peer.
If called with no arguments, get a list of metric types known to the peer."
  (if metric
      (http-get-json "/monitor/metrics/" metric)
      (http-get-json "/monitor/metrics")))

(defun get-monitor-metrics-freespace ()
  (get-monitor-metrics "freespace"))

(defun get-monitor-metrics-ping ()
  (get-monitor-metrics "ping"))

(defun get-health-alerts ()
  "Display a list of alerts (metric expiration events)"
  (http-get-json "/health/alerts"))

(defun get-health-graph ()
  "Get connection graph"
  (http-get-json "/health/graph"))

(defun post-ipfs-gc ()
  "Perform GC in the IPFS nodes"
  (http-post "/ipfs/gc"))
