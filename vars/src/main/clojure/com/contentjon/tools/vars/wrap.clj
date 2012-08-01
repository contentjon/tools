(ns com.contentjon.tools.vars.wrap)

(defmacro wrap-one [external-sym]
  (let [ns  (-> external-sym resolve meta :ns .getName symbol)
        sym (-> external-sym name symbol)
        ext-sym (symbol (str ns "/" sym))]
    `(do
       (require '~ns)
       (let [v# (var ~ext-sym)
             m# (meta v#)
             mac?# (:macro m#)
             meta# (select-keys m# [:arglists :doc])]
         (if mac?#
           (defmacro ~sym [& args#]
             (list* '~ext-sym args#))
           (def ~sym
             @v#))
         (.alterMeta (var ~sym) merge (list meta#))))))

(defmacro wrap [& external-syms]
  `(do ~@(for [sym external-syms]
           `(wrap-one ~sym))))