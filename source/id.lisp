(defpackage #:ipfs-cluster/id
  (:use :common-lisp)
  (:export :id
           :cid
           :make-cid
           :ipns
           :key
           :make-ipns
           :ipld
           :make-ipld))
(in-package :ipfs-cluster/id)

(defclass id ()
  ()
  (:documentation "Root class for all IPFS identifier types."))

(defclass cid (id)
  ((cid :accessor cid :initarg :cid :initform nil
        :documentation "The string form of the actual CID multihash."))
  (:documentation "An IPFS Content Identifier (CID) is an idempotent address for
a block of data in IPFS that is derived from its content.

Cf. <https://docs.ipfs.io/concepts/content-addressing/#identifier-formats>"))

(defun make-cid (cid)
  "Constructor for CIDs"
  (make-instance 'cid :cid cid))

(defclass ipns (id)
  ((key :accessor key :initarg :key :initform nil
        :documentation "The string form of the actual key."))
  (:documentation "IPNS = InterPlanetary Name System.

Cf. <https://docs.ipfs.io/concepts/ipns/>"))

(defun make-ipns (key)
  "Constructor for IPNSs"
  (make-instance 'ipns :key key))

(defclass ipld (id)
  ((cid :accessor cid :initarg :cid :initform nil
        :documentation "The string form of the actual CID."))
  (:documentation "IPLD = InterPlanetary Linked Data.

Cf. <https://docs.ipfs.io/concepts/ipld/>
Cf. <https://medium.com/towardsblockchain/understanding-ipfs-in-depth-2-6-what-is-interplanetary-linked-data-ipld-c8c01551517b>
Cf. <https://explore.ipld.io/#/explore/zdpuAujL3noEMamveLPQWJPY6CYZHhHoskYQaZBvRbAfVwR8S>"))

(defun make-ipld (cid)
  "Constructor for IPLDs"
  (make-instance 'ipld :cid cid))
