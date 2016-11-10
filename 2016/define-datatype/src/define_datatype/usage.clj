;;; Usage sample from outside the namespace.
;;;
;;; Eli Bendersky [http://eli.thegreenplace.net]
;;; This code is in the public domain.
(ns define-datatype.usage
  (:require [define-datatype.define-datatype :as dd]))

(dd/define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(defn node-count
  [bt]
  (dd/cases bintree bt
            (leaf-node (num) 1)
            (interior-node (key left right)
                           (+ (node-count left) (node-count right)))))

(let [bt1 (interior-node 'k
                         (interior-node 'p (leaf-node 20) (leaf-node 30))
                         (leaf-node 40))]
  (node-count bt1))
