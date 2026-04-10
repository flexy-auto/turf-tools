;; AUTOOFFSET
;; Window-select objects, ask for an offset distance, then offset
;; closed polylines and circles outward.

(setq *ao-default-distance* 0.02)
(setq *ao-cluster-tol* 0.01)

(defun _ao-get-distance ( / val)
  (setq val
    (getdist
      (strcat
        "\nEnter offset distance <"
        (rtos *ao-default-distance* 2 2)
        ">: "
      )
    )
  )
  (if val val *ao-default-distance*)
)

(defun _ao-is-closed (ed)
  (= 1 (logand 1 (cdr (assoc 70 ed))))
)

(defun _ao-object-area (ename / obj result)
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

(defun _ao-object-bbox (ename / obj minpt maxpt pt1 pt2)
  (setq obj (vlax-ename->vla-object ename))
  (vla-getBoundingBox obj 'minpt 'maxpt)
  (setq pt1 (vlax-safearray->list minpt))
  (setq pt2 (vlax-safearray->list maxpt))
  (list (car pt1) (cadr pt1) (car pt2) (cadr pt2))
)

(defun _ao-sort-by-area-desc (items)
  (vl-sort
    items
    (function
      (lambda (a b)
        (> (car a) (car b))
      )
    )
  )
)

(defun _ao-bboxes-touch-p (a b /)
  (and
    (<= (nth 0 a) (+ (nth 2 b) *ao-cluster-tol*))
    (<= (nth 0 b) (+ (nth 2 a) *ao-cluster-tol*))
    (<= (nth 1 a) (+ (nth 3 b) *ao-cluster-tol*))
    (<= (nth 1 b) (+ (nth 3 a) *ao-cluster-tol*))
  )
)

(defun _ao-item-touches-cluster-p (item cluster / hit)
  (setq hit nil)
  (foreach other cluster
    (if (_ao-bboxes-touch-p (caddr item) (caddr other))
      (setq hit T)
    )
  )
  hit
)

(defun _ao-cluster-items (items / clusters seed cluster changed pending item)
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
        (if (_ao-item-touches-cluster-p item cluster)
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

(defun _ao-skip-list (clusters / skip sorted)
  (setq skip '())
  (foreach cluster clusters
    (setq sorted (_ao-sort-by-area-desc cluster))
    (if sorted
      (setq skip (cons (cadr (car sorted)) skip))
    )
  )
  skip
)

(defun _ao-member-ename-p (ename items / hit)
  (setq hit nil)
  (foreach item items
    (if (eq ename item)
      (setq hit T)
    )
  )
  hit
)

(defun _ao-outside-point (ename dist / obj minpt maxpt pt1 pt2 x y)
  (setq obj (vlax-ename->vla-object ename))
  (vla-getBoundingBox obj 'minpt 'maxpt)
  (setq pt1 (vlax-safearray->list minpt))
  (setq pt2 (vlax-safearray->list maxpt))
  (setq x (+ (car pt2) (* dist 5.0)))
  (setq y (/ (+ (cadr pt1) (cadr pt2)) 2.0))
  (list x y 0.0)
)

(defun _ao-side-point (ename dist / ed typ ctr rad)
  (setq ed (entget ename))
  (setq typ (cdr (assoc 0 ed)))
  (cond
    ((or (= typ "LWPOLYLINE") (= typ "POLYLINE"))
     (if (_ao-is-closed ed)
       (_ao-outside-point ename dist)
       nil
     )
    )
    ((= typ "CIRCLE")
     (setq ctr (cdr (assoc 10 ed)))
     (setq rad (cdr (assoc 40 ed)))
     (list (+ (car ctr) rad (* dist 5.0)) (cadr ctr) 0.0)
    )
    (T nil)
  )
)

(defun _ao-offset-one (ename dist / side)
  (setq side (_ao-side-point ename dist))
  (if side
    (not
      (vl-catch-all-error-p
        (vl-catch-all-apply
          'vl-cmdf
          (list "_.OFFSET" dist ename side "")
        )
      )
    )
    nil
  )
)

(defun c:AUTOOFFSET ( / ss i ent dist okCount skipCount items area bbox clusters skipList)
  (vl-load-com)
  (prompt "\nSelect objects to offset: ")
  ;; Use AutoCAD's native selection UX (window/crossing/fence/etc).
  (setq ss
    (ssget
      '((0 . "CIRCLE,LWPOLYLINE,POLYLINE"))
    )
  )

  (if ss
    (progn
      (setq dist (_ao-get-distance))
      (setq items '())
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq area (_ao-object-area ent))
        (if area
          (progn
            (setq bbox (_ao-object-bbox ent))
            (setq items (cons (list area ent bbox) items))
          )
        )
        (setq i (+ i 1))
      )
      (setq clusters (_ao-cluster-items items))
      (setq skipList (_ao-skip-list clusters))

      (setq i 0)
      (setq okCount 0)
      (setq skipCount 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (_ao-member-ename-p ent skipList)
          (setq skipCount (+ skipCount 1))
          (if (_ao-offset-one ent dist)
            (setq okCount (+ okCount 1))
            (setq skipCount (+ skipCount 1))
          )
        )
        (setq i (+ i 1))
      )
      (prompt
        (strcat
          "\nOffset "
          (itoa okCount)
          " object(s). Skipped "
          (itoa skipCount)
          "."
        )
      )
    )
    (prompt "\nNothing selected.")
  )

  (princ)
)
