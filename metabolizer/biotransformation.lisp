
(setq *substrate-atom-bond-graph* (adj-list 'etoh))
;((O (2 1) (3 1)) 
; (H (1 1)) 
; (C (1 1) (4 1)) 
; (C (3 1)))
(setq *substrate-pattern* (adj-list '|Alcohols|))
;((O (2 1)) 
; (R (1 1)))
(setq *product-template* (adj-list '|an aldehyde|))
;((H (4 1)) 
; (O (4 2)) 
; (R (4 1)) 
; (C (1 1) (2 2) (3 1)))
(setq *product-atom-bond-graph* (adj-list 'acetald))
;((O (3 2)) 
; (H (3 1)) 
; (C (2 1) (1 2) (4 1)) 
; (C (3 1)))

(setq *pattern-substrate-bindings* (match-substrate-pattern *substrate-atom-bond-graph* *substrate-pattern*))
; (#(HEAD 1 3 2 C H H H H H) 
;        (O R H))

(cpd-adj-list-structure-equal? (*product-atom-bond-graph* (adj-list 'acetald))
; 
;; For efficiency and error avoidance, match-kb without match-h should followed with match-single with match-h on query results.

(defun match-substrate-pattern (substrate pattern &key (substrate-format :adj-list) (pattern-format :adj-list))
  (multiple-value-bind (pattern-atom-bond-graph
			substrate-atom-bond-graph
			substrate-pattern-bindings
			pattern-substrate-bindings
			pattern-atom-list) 
      (match-single (hydrogenate pattern) substrate 
				     :query-format pattern-format
				     :ref-format substrate-format 
				     :match-h? t
				     :verbose? t
				     :match-charge? nil)
    (if pattern-substrate-bindings
	(list pattern-substrate-bindings pattern-atom-list))))

(defun make-biotransformation-rule (rule-trigger rule-template)
  ;; To effectively substitute the rule pattern with the template, it is only necessary to keep track of the 
  ;; R groups,  and rest can be recomputed.
  (list rule-trigger rule-template))




  
  