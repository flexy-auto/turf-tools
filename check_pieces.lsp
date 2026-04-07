;; CHECKPIECES
;; PASS only when:
;; 1. all MTEXT is on layer tags
;; 2. all other selected objects are closed polylines or circles on layer 0
;; 3. all closed polylines are within 179 x 359

(setq *cps-max-width* 179.0)
(setq *cps-max-height* 359.0)
(setq *cps-tol* 0.01)

(setq c:CHECKPIECESIZE nil)

(defun _cps-is-closed (ed)
  (= 1 (logand 1 (cdr (assoc 70 ed))))
)

(defun _cps-piece-dims (ename / obj minpt maxpt pt1 pt2 dx dy)
  (setq obj (vlax-ename->vla-object ename))
  (vla-getBoundingBox obj 'minpt 'maxpt)
  (setq pt1 (vlax-safearray->list minpt))
  (setq pt2 (vlax-safearray->list maxpt))
  (setq dx (abs (- (car pt2) (car pt1))))
  (setq dy (abs (- (cadr pt2) (cadr pt1))))
  (list dx dy)
)

(defun _cps-mtext-ok-p (ed)
  (and
    (= (cdr (assoc 0 ed)) "MTEXT")
    (= (cdr (assoc 8 ed)) "tags")
  )
)

(defun _cps-valid-piece-p (ed)
  (and
    (or
      (= (cdr (assoc 0 ed)) "LWPOLYLINE")
      (= (cdr (assoc 0 ed)) "POLYLINE")
      (= (cdr (assoc 0 ed)) "CIRCLE")
    )
    (= (cdr (assoc 8 ed)) "0")
    (if (or
          (= (cdr (assoc 0 ed)) "LWPOLYLINE")
          (= (cdr (assoc 0 ed)) "POLYLINE")
        )
      (_cps-is-closed ed)
      T
    )
  )
)

(defun c:CHECKPIECES ( / ss i ent ed dims ok foundPiece failMsg failEnt failSet)
  (vl-load-com)
  (prompt "\nSelect objects to validate: ")
  (setq ss (ssget))

  (if ss
    (progn
      (setq i 0)
      (setq ok T)
      (setq foundPiece nil)
      (setq failMsg nil)
      (setq failEnt nil)

      (while (and (< i (sslength ss)) ok)
        (setq ent (ssname ss i))
        (setq ed (entget ent))

        (cond
          ((_cps-mtext-ok-p ed)
           nil
          )

          ((_cps-valid-piece-p ed)
           (setq foundPiece T)
           (if (or
                 (= (cdr (assoc 0 ed)) "LWPOLYLINE")
                 (= (cdr (assoc 0 ed)) "POLYLINE")
               )
             (progn
               (setq dims (_cps-piece-dims ent))
               (if (or
                     (> (car dims) (+ *cps-max-width* *cps-tol*))
                     (> (cadr dims) (+ *cps-max-height* *cps-tol*))
                   )
                 (progn
                   (setq ok nil)
                   (setq failEnt ent)
                   (setq failMsg
                     (strcat
                       "\nFAIL - piece is "
                       (rtos (car dims) 2 2)
                       " wide x "
                       (rtos (cadr dims) 2 2)
                       " high"
                     )
                   )
                 )
               )
             )
           )
          )

          (T
           (setq ok nil)
           (setq failEnt ent)
           (setq failMsg "\nFAIL - found invalid object or wrong layer")
          )
        )

        (setq i (+ i 1))
      )

      (if (not foundPiece)
        (prompt "\nFAIL")
        (if ok
          (prompt "\nPASS")
          (progn
            (setq failSet (ssadd))
            (ssadd failEnt failSet)
            (sssetfirst nil failSet)
            (prompt failMsg)
          )
        )
      )
    )
    (prompt "\nFAIL")
  )

  (princ)
)
