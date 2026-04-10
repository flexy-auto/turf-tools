;; GRABBORDERS
;; Select border objects for one or more letters, group them into
;; separate letters by geometric separation, and keep the two largest
;; border objects in each letter.

(setq *gb-cluster-tol* 0.01)

(defun _gb-object-area (ename / obj result)
  (setq obj (vlax-ename->vla-object ename))
  (setq result
    (vl-catch-all-apply
      'vla-get-Area
      (list obj)
    )
  )
  (if (vl-catch-all-error-p result)
    nil
    result
  )
)

(defun _gb-object-bbox (ename / obj minpt maxpt pt1 pt2)
  (setq obj (vlax-ename->vla-object ename))
  (vla-getBoundingBox obj 'minpt 'maxpt)
  (setq pt1 (vlax-safearray->list minpt))
  (setq pt2 (vlax-safearray->list maxpt))
  (list (car pt1) (cadr pt1) (car pt2) (cadr pt2))
)

(defun _gb-sort-by-area-desc (items)
  (vl-sort
    items
    (function
      (lambda (a b)
        (> (car a) (car b))
      )
    )
  )
)

(defun _gb-bboxes-touch-p (a b /)
  (and
    (<= (nth 0 a) (+ (nth 2 b) *gb-cluster-tol*))
    (<= (nth 0 b) (+ (nth 2 a) *gb-cluster-tol*))
    (<= (nth 1 a) (+ (nth 3 b) *gb-cluster-tol*))
    (<= (nth 1 b) (+ (nth 3 a) *gb-cluster-tol*))
  )
)

(defun _gb-item-touches-cluster-p (item cluster / hit)
  (setq hit nil)
  (foreach other cluster
    (if (_gb-bboxes-touch-p (caddr item) (caddr other))
      (setq hit T)
    )
  )
  hit
)

(defun _gb-cluster-items (items / clusters seed rest cluster changed pending item)
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
        (if (_gb-item-touches-cluster-p item cluster)
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

(defun _gb-add-top-two-from-cluster (cluster out / sorted)
  (setq sorted (_gb-sort-by-area-desc cluster))
  (if sorted
    (ssadd (cadr (nth 0 sorted)) out)
  )
  (if (> (length sorted) 1)
    (ssadd (cadr (nth 1 sorted)) out)
  )
  out
)

(defun c:GRABBORDERS ( / ss i ent area bbox items clusters out count)
  (vl-load-com)
  (prompt "\nSelect border objects: ")
  (setq ss (ssget))

  (if ss
    (progn
      (setq i 0)
      (setq items '())
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq area (_gb-object-area ent))
        (if area
          (progn
            (setq bbox (_gb-object-bbox ent))
            (setq items (cons (list area ent bbox) items))
          )
        )
        (setq i (+ i 1))
      )

      (setq clusters (_gb-cluster-items items))

      (if clusters
        (progn
          (setq out (ssadd))
          (setq count 0)
          (foreach cluster clusters
            (_gb-add-top-two-from-cluster cluster out)
            (setq count (+ count 1))
          )
          (sssetfirst nil out)
          (prompt
            (strcat
              "\nSelected the two outermost border objects for "
              (itoa count)
              " letter group(s)."
            )
          )
        )
        (prompt "\nNeed at least one area-based object in the selection.")
      )
    )
    (prompt "\nNothing selected.")
  )

  (princ)
)
