;; GRABLETTER
;; 1. Identify separate letter groups.
;; 2. In each letter, find a close outer border pair (within 0.5).
;;    Select the smaller polyline from that pair.
;; 3. Then look inside that selected polyline. If there is another close
;;    border pair inside it (within 0.5), select the larger polyline
;;    from that inner pair.

(vl-load-com)

(setq *gl-cluster-tol* 0.01)
(setq *gl-pair-gap* 0.5)

(defun _gl-object-area (ename / obj result)
  (setq obj (vlax-ename->vla-object ename))
  (setq result (vl-catch-all-apply 'vla-get-Area (list obj)))
  (if (vl-catch-all-error-p result) nil result)
)

(defun _gl-object-bbox (ename / obj minpt maxpt pt1 pt2)
  (setq obj (vlax-ename->vla-object ename))
  (vla-getBoundingBox obj 'minpt 'maxpt)
  (setq pt1 (vlax-safearray->list minpt))
  (setq pt2 (vlax-safearray->list maxpt))
  (list (car pt1) (cadr pt1) (car pt2) (cadr pt2))
)

(defun _gl-bboxes-touch-p (a b /)
  (and
    (<= (nth 0 a) (+ (nth 2 b) *gl-cluster-tol*))
    (<= (nth 0 b) (+ (nth 2 a) *gl-cluster-tol*))
    (<= (nth 1 a) (+ (nth 3 b) *gl-cluster-tol*))
    (<= (nth 1 b) (+ (nth 3 a) *gl-cluster-tol*))
  )
)

(defun _gl-bbox-contains-p (outer inner /)
  (and
    (<= (- (nth 0 outer) *gl-cluster-tol*) (nth 0 inner))
    (<= (- (nth 1 outer) *gl-cluster-tol*) (nth 1 inner))
    (>= (+ (nth 2 outer) *gl-cluster-tol*) (nth 2 inner))
    (>= (+ (nth 3 outer) *gl-cluster-tol*) (nth 3 inner))
  )
)

(defun _gl-items-related-p (item other)
  (or
    (_gl-bboxes-touch-p (caddr item) (caddr other))
    (_gl-bbox-contains-p (caddr item) (caddr other))
    (_gl-bbox-contains-p (caddr other) (caddr item))
  )
)

(defun _gl-item-touches-cluster-p (item cluster / hit other)
  (setq hit nil)
  (foreach other cluster
    (if (_gl-items-related-p item other)
      (setq hit T)
    )
  )
  hit
)

(defun _gl-cluster-items (items / clusters seed cluster changed pending item)
  (setq clusters '())
  (while items
    (setq seed (car items))
    (setq items (cdr items))
    (setq cluster (list seed))
    (setq changed T)
    (while changed
      (setq changed nil)
      (setq pending '())
      (foreach item items
        (if (_gl-item-touches-cluster-p item cluster)
          (progn
            (setq cluster (cons item cluster))
            (setq changed T)
          )
          (setq pending (cons item pending))
        )
      )
      (setq items pending)
    )
    (setq clusters (cons cluster clusters))
  )
  clusters
)

(defun _gl-sort-by-area-desc (items)
  (vl-sort items '(lambda (a b) (> (car a) (car b))))
)

(defun _gl-poly-gap (outer inner / outerArea innerArea)
  ;; Approximate offset distance from area difference.
  ;; Good enough for finding very-close nested pairs.
  (setq outerArea (car outer)
        innerArea (car inner))
  (if (> outerArea innerArea)
    (/ (- (sqrt outerArea) (sqrt innerArea)) 2.0)
    999999.0
  )
)

(defun _gl-close-pair-p (outer inner)
  (and
    (_gl-bbox-contains-p (caddr outer) (caddr inner))
    (<= (_gl-poly-gap outer inner) *gl-pair-gap*)
  )
)

(defun _gl-find-best-close-pair (items / sorted outer inner bestPair bestGap)
  ;; Find the first close nested pair from largest inward.
  (setq sorted (_gl-sort-by-area-desc items)
        bestPair nil
        bestGap nil)
  (while sorted
    (setq outer (car sorted))
    (foreach inner (cdr sorted)
      (if (_gl-close-pair-p outer inner)
        (if (or (null bestGap) (< (_gl-poly-gap outer inner) bestGap))
          (progn
            (setq bestGap (_gl-poly-gap outer inner))
            (setq bestPair (list outer inner))
          )
        )
      )
    )
    (setq sorted (cdr sorted))
  )
  bestPair
)

(defun _gl-items-inside (container items / out item)
  (setq out '())
  (foreach item items
    (if (_gl-bbox-contains-p (caddr container) (caddr item))
      (setq out (cons item out))
    )
  )
  out
)

(defun _gl-remove-entity (ent items / out item)
  (setq out '())
  (foreach item items
    (if (/= (cadr item) ent)
      (setq out (cons item out))
    )
  )
  out
)

(defun _gl-add-letter-picks (letter out / outerPair outerOuter outerInner insideItems innerPair)
  ;; Step 1: find close outer pair and select the smaller member.
  (setq outerPair (_gl-find-best-close-pair letter))
  (if outerPair
    (progn
      (setq outerOuter (car outerPair)
            outerInner (cadr outerPair))
      (ssadd (cadr outerInner) out)

      ;; Step 2: look inside the selected outer-inner polyline only.
      ;; If another close pair exists there, select the larger member.
      (setq insideItems (_gl-items-inside outerInner letter))
      (setq insideItems (_gl-remove-entity (cadr outerInner) insideItems))
      (if insideItems
        (progn
          (setq innerPair (_gl-find-best-close-pair insideItems))
          (if innerPair
            (ssadd (cadr (car innerPair)) out)
          )
        )
      )
    )
  )
  out
)

(defun c:GRABLETTER ( / ss i ent ed area bbox items letters out count)
  (prompt "\nSelect border polylines: ")
  (setq ss (ssget))

  (if ss
    (progn
      (setq i 0
            items '())
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        (setq area (_gl-object-area ent))
        (if (and area (member (cdr (assoc 0 ed)) '("LWPOLYLINE" "POLYLINE")))
          (setq items (cons (list area ent (_gl-object-bbox ent)) items))
        )
        (setq i (1+ i))
      )

      (setq letters (_gl-cluster-items items))

      (if letters
        (progn
          (setq out (ssadd)
                count 0)
          (foreach letter letters
            (_gl-add-letter-picks letter out)
            (setq count (1+ count))
          )
          (sssetfirst nil out)
          (prompt
            (strcat
              "\nSelected grabletter borders for "
              (itoa count)
              " letter group(s)."
            )
          )
        )
        (prompt "\nNeed at least one polyline in the selection.")
      )
    )
    (prompt "\nNothing selected.")
  )
  (princ)
)

