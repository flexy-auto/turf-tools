;; FASTDIMS
;; Group selected objects into clusters by proximity, then place
;; one overall width dimension above each cluster and one overall
;; height dimension to the left of each cluster.

(vl-load-com)

(setq c:DIMCLUSTERS nil)
(setq *fd-cluster-tol* 0.01)
(setq *fd-last-offset* 1.0)

(defun _fd-object-bbox (ename / obj minpt maxpt pt1 pt2)
  (setq obj (vlax-ename->vla-object ename))
  (vla-getBoundingBox obj 'minpt 'maxpt)
  (setq pt1 (vlax-safearray->list minpt))
  (setq pt2 (vlax-safearray->list maxpt))
  (list (car pt1) (cadr pt1) (car pt2) (cadr pt2))
)

(defun _fd-bboxes-touch-p (a b /)
  (and
    (<= (nth 0 a) (+ (nth 2 b) *fd-cluster-tol*))
    (<= (nth 0 b) (+ (nth 2 a) *fd-cluster-tol*))
    (<= (nth 1 a) (+ (nth 3 b) *fd-cluster-tol*))
    (<= (nth 1 b) (+ (nth 3 a) *fd-cluster-tol*))
  )
)

(defun _fd-item-touches-cluster-p (item cluster / hit)
  (setq hit nil)
  (foreach other cluster
    (if (_fd-bboxes-touch-p (cadr item) (cadr other))
      (setq hit T)
    )
  )
  hit
)

(defun _fd-cluster-items (items / clusters seed cluster changed pending item)
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
        (if (_fd-item-touches-cluster-p item cluster)
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

(defun _fd-cluster-bbox (cluster / box item ibox)
  (setq box nil)
  (foreach item cluster
    (setq ibox (cadr item))
    (if box
      (setq box
        (list
          (min (nth 0 box) (nth 0 ibox))
          (min (nth 1 box) (nth 1 ibox))
          (max (nth 2 box) (nth 2 ibox))
          (max (nth 3 box) (nth 3 ibox))
        )
      )
      (setq box ibox)
    )
  )
  box
)

(defun _fd-entity-bbox (ename / obj minpt maxpt pt1 pt2)
  (setq obj (vlax-ename->vla-object ename))
  (vla-getBoundingBox obj 'minpt 'maxpt)
  (setq pt1 (vlax-safearray->list minpt))
  (setq pt2 (vlax-safearray->list maxpt))
  (list (car pt1) (cadr pt1) (car pt2) (cadr pt2))
)

(defun _fd-move-entity (ename dx dy / obj)
  (setq obj (vlax-ename->vla-object ename))
  (vla-Move
    obj
    (vlax-3d-point '(0.0 0.0 0.0))
    (vlax-3d-point (list dx dy 0.0))
  )
)

(defun _fd-adjust-horizontal-dim (ename bbox off / dimbox shift)
  (setq dimbox (_fd-entity-bbox ename))
  (if (< (nth 1 dimbox) (+ (nth 3 bbox) off))
    (progn
      (setq shift (- (+ (nth 3 bbox) off) (nth 1 dimbox)))
      (_fd-move-entity ename 0.0 shift)
    )
  )
)

(defun _fd-adjust-vertical-dim (ename bbox off / dimbox shift)
  (setq dimbox (_fd-entity-bbox ename))
  (if (> (nth 2 dimbox) (- (nth 0 bbox) off))
    (progn
      (setq shift (- (- (nth 0 bbox) off) (nth 2 dimbox)))
      (_fd-move-entity ename shift 0.0)
    )
  )
)

(defun _fd-place-dims (bbox off / x1 y1 x2 y2 topLoc leftLoc before after)
  (setq x1 (nth 0 bbox)
        y1 (nth 1 bbox)
        x2 (nth 2 bbox)
        y2 (nth 3 bbox)
        topLoc (list (/ (+ x1 x2) 2.0) (+ y2 off) 0.0)
        leftLoc (list (- x1 off) (/ (+ y1 y2) 2.0) 0.0))

  (setq before (entlast))
  (vl-cmdf
    "_.DIMROTATED" 0
    (list x1 y2 0.0)
    (list x2 y2 0.0)
    topLoc
  )
  (setq after (entlast))
  (if (and after (/= before after))
    (_fd-adjust-horizontal-dim after bbox off)
  )

  (setq before (entlast))
  (vl-cmdf
    "_.DIMROTATED" 90
    (list x1 y1 0.0)
    (list x1 y2 0.0)
    leftLoc
  )
  (setq after (entlast))
  (if (and after (/= before after))
    (_fd-adjust-vertical-dim after bbox off)
  )
)

(defun c:FASTDIMS ( / ss i ent bbox items clusters count off)
  (prompt "\nSelect objects to dimension by cluster: ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE,CIRCLE,ARC"))))

  (if ss
    (progn
      (initget 6)
      (setq off (getreal (strcat "\nEnter dimension offset distance <" (rtos *fd-last-offset* 2 2) ">: ")))
      (if (null off)
        (setq off *fd-last-offset*)
        (setq *fd-last-offset* off)
      )

      (setq i 0
            items '())
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq bbox (_fd-object-bbox ent))
        (if bbox
          (setq items (cons (list ent bbox) items))
        )
        (setq i (1+ i))
      )

      (setq clusters (_fd-cluster-items items))

      (if clusters
        (progn
          (setq count 0)
          (foreach cluster clusters
            (_fd-place-dims (_fd-cluster-bbox cluster) off)
            (setq count (1+ count))
          )
          (prompt
            (strcat
              "\nPlaced width and height dimensions for "
              (itoa count)
              " cluster(s)."
            )
          )
        )
        (prompt "\nNothing dimensionable found.")
      )
    )
    (prompt "\nNothing selected.")
  )
  (princ)
)
