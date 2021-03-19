(defpackage #:ipfs-cluster/id
  (:use :common-lisp)
  (:export :ipfs-id
           :ipfs-cid
           :ipns-name
           :ipld-node-cid))
(in-package :ipfs-cluster/id)

(defclass ipfs-id ()
  ()
  (:documentation "Root class for all IPFS identifier types."))

(defclass ipfs-cid (ipfs-id)
  ((cid :accessor cid :initarg :cid :initform nil
        :documentation "The string form of the actual CID multihash."))
  (:documentation "An IPFS Content Identifier (CID) is an idempotent address for
a block of data in IPFS that is derived from its content.

Cf. <https://docs.ipfs.io/concepts/content-addressing/#identifier-formats>"))

(defclass ipns-name (ipfs-id)
  ((key :accessor key :initarg :key :initform nil
        :documentation "The string form of the actual key."))
  (:documentation "IPNS = InterPlanetary Name System.

Cf. <https://docs.ipfs.io/concepts/ipns/>"))

(defclass ipld-node-cid (ipfs-id)
  ((cid :accessor cid :initarg :cid :initform nil
        :documentation "The string form of the actual CID."))
  (:documentation "IPLD = InterPlanetary Linked Data.

Cf. <https://docs.ipfs.io/concepts/ipld/>
Cf. <https://medium.com/towardsblockchain/understanding-ipfs-in-depth-2-6-what-is-interplanetary-linked-data-ipld-c8c01551517b>
Cf. <https://explore.ipld.io/#/explore/zdpuAujL3noEMamveLPQWJPY6CYZHhHoskYQaZBvRbAfVwR8S>"))
