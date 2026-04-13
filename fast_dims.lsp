;; FASTDIMS
;; Places one overall horizontal dimension and one overall vertical dimension
;; for each separate cluster of selected layer-0 linework.

(vl-load-com)

;; If two object bounding boxes are within this distance, they count as one piece.
(setq *fd-group-tolerance* 1.0)

(defun fd-make-dim-layer ( / )
  ;; Created dimensions go on layer DIM. If it already exists, this simply sets it current.
  (command "_.-LAYER" "_Make" "DIM" "")
)

(defun fd-supported-object-p (ename / data typ lay)
  ;; Only measure real geometry on layer 0. This keeps old dimensions, text, leaders, etc. out of the math.
  (setq data (entget ename))
  (setq typ (cdr (assoc 0 data)))
  (setq lay (cdr (assoc 8 data)))
  (and (= (strcase lay) "0")
       (member typ '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "CIRCLE")))
)

(defun fd-variant-point->list (varpt / safearr)
  (if (= (type varpt) 'VARIANT)
    (setq safearr (vlax-variant-value varpt))
    (setq safearr varpt)
  )
  (vlax-safearray->list safearr)
)

(defun fd-get-bbox (ename / obj minpt maxpt minlst maxlst)
  ;; AutoCAD's bounding box handles arcs and bulged polylines for us.
  (setq obj (vlax-ename->vla-object ename))
  (if (not (vl-catch-all-error-p
             (vl-catch-all-apply
               'vla-getboundingbox
               (list obj 'minpt 'maxpt))))
    (progn
      (setq minlst (fd-variant-point->list minpt))
      (setq maxlst (fd-variant-point->list maxpt))
      (list (car minlst) (cadr minlst) (car maxlst) (cadr maxlst))
    )
  )
)

(defun fd-bbox-union (a b)
  (list (min (nth 0 a) (nth 0 b))
        (min (nth 1 a) (nth 1 b))
        (max (nth 2 a) (nth 2 b))
        (max (nth 3 a) (nth 3 b)))
)

(defun fd-bbox-close-p (a b tol / xgap ygap)
  ;; Two boxes are close if the empty gap between them is no more than the grouping tolerance.
  (setq xgap (max 0.0 (- (max (nth 0 a) (nth 0 b)) (min (nth 2 a) (nth 2 b)))))
  (setq ygap (max 0.0 (- (max (nth 1 a) (nth 1 b)) (min (nth 3 a) (nth 3 b)))))
  (and (<= xgap tol) (<= ygap tol))
)

(defun fd-add-to-clusters (item clusters tol / bbox cluster hit rebuilt)
  ;; Each cluster is stored as:
  ;;   (combined-bounding-box list-of-entity-names)
  ;;
  ;; When a new object is close to an existing cluster, we merge it into that cluster.
  ;; If it is not close to any cluster, it starts a new separate piece.
  (setq bbox (cadr item))
  (setq hit nil)
  (setq rebuilt nil)
  (foreach cluster clusters
    (if (and (not hit) (fd-bbox-close-p bbox (car cluster) tol))
      (progn
        (setq hit T)
        (setq rebuilt
               (cons
                 (list (fd-bbox-union bbox (car cluster))
                       (cons (car item) (cadr cluster)))
                 rebuilt))
      )
      (setq rebuilt (cons cluster rebuilt))
    )
  )
  (if hit
    (reverse rebuilt)
    (cons (list bbox (list (car item))) clusters))
)

(defun fd-merge-close-clusters (clusters tol / changed result cluster other merged)
  ;; After objects are first added, two clusters may become close because a cluster's
  ;; bounding box grew. This repeats merging until no nearby clusters remain separate.
  (setq changed T)
  (while changed
    (setq changed nil)
    (setq result nil)
    (while clusters
      (setq cluster (car clusters))
      (setq clusters (cdr clusters))
      (setq merged nil)
      (foreach other clusters
        (if (and (not merged) (fd-bbox-close-p (car cluster) (car other) tol))
          (progn
            (setq cluster
                   (list (fd-bbox-union (car cluster) (car other))
                         (append (cadr cluster) (cadr other))))
            (setq clusters (vl-remove other clusters))
            (setq changed T)
            (setq merged T))
        )
      )
      (setq result (cons cluster result))
    )
    (setq clusters result)
  )
  clusters
)

(defun fd-build-clusters (items tol / clusters)
  (setq clusters nil)
  (foreach item items
    (setq clusters (fd-add-to-clusters item clusters tol))
  )
  (fd-merge-close-clusters clusters tol)
)

(defun fd-highlight-list (enames / ss)
  (setq ss (ssadd))
  (foreach ename enames
    (if (and ename (entget ename))
      (ssadd ename ss)
    )
  )
  (sssetfirst nil ss)
)

(defun fd-create-cluster-dims (bbox offset / xmin ymin xmax ymax hp1 hp2 hloc vp1 vp2 vloc ok)
  (setq xmin (nth 0 bbox))
  (setq ymin (nth 1 bbox))
  (setq xmax (nth 2 bbox))
  (setq ymax (nth 3 bbox))
  (setq ok (and (> (- xmax xmin) 0.0001) (> (- ymax ymin) 0.0001)))
  (if ok
    (progn
      ;; Horizontal dimension: measures the overall width and sits above the bounding box.
      (setq hp1 (list xmin ymax 0.0))
      (setq hp2 (list xmax ymax 0.0))
      (setq hloc (list (/ (+ xmin xmax) 2.0) (+ ymax offset) 0.0))
      (vl-cmdf "_.DIMLINEAR" "_non" hp1 "_non" hp2 "_non" hloc)

      ;; Vertical dimension: measures the overall height and sits left of the bounding box.
      (setq vp1 (list xmin ymin 0.0))
      (setq vp2 (list xmin ymax 0.0))
      (setq vloc (list (- xmin offset) (/ (+ ymin ymax) 2.0) 0.0))
      (vl-cmdf "_.DIMLINEAR" "_non" vp1 "_non" vp2 "_non" vloc)
      T
    )
    nil
  )
)

(defun c:FASTDIMS ( / oldlayer sel idx ename bbox items clusters offset failed made)
  (setq oldlayer (getvar "CLAYER"))
  (setq failed nil)
  (setq made 0)
  (prompt "\nSelect layer-0 logo/EZ geometry to dimension:")
  (setq sel (ssget))
  (if sel
    (progn
      (setq offset (getreal "\nEnter dimension offset <10>: "))
      (if (not offset) (setq offset 10.0))
      (if (> offset 0.0)
        (progn
          (setq idx 0)
          (setq items nil)
          (while (< idx (sslength sel))
            (setq ename (ssname sel idx))
            (if (fd-supported-object-p ename)
              (progn
                (setq bbox (fd-get-bbox ename))
                (if bbox
                  (setq items (cons (list ename bbox) items))
                  (setq failed (cons ename failed))
                )
              )
            )
            (setq idx (1+ idx))
          )

          (if items
            (progn
              (fd-make-dim-layer)
              (setq clusters (fd-build-clusters items *fd-group-tolerance*))
              (foreach cluster clusters
                (if (fd-create-cluster-dims (car cluster) offset)
                  (setq made (1+ made))
                  (setq failed (append (cadr cluster) failed))
                )
              )
              (setvar "CLAYER" oldlayer)
              (if failed
                (progn
                  (fd-highlight-list failed)
                  (prompt
                    (strcat
                      "\nFASTDIMS finished with failed pieces highlighted. Dimensioned "
                      (itoa made)
                      " piece(s)."))
                )
                (prompt
                  (strcat
                    "\nFASTDIMS complete. Dimensioned "
                    (itoa made)
                    " piece(s)."))
              )
            )
            (progn
              (setvar "CLAYER" oldlayer)
              (prompt "\nNo supported layer-0 linework was selected.")
            )
          )
        )
        (prompt "\nOffset must be greater than 0. FASTDIMS cancelled.")
      )
    )
    (prompt "\nNothing selected. FASTDIMS cancelled.")
  )
  (princ)
)
