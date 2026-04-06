;; DOUBLELINEDETECTOR
;; Find overlapping straight linework and highlight the parent objects.
;; This command checks LINE, LWPOLYLINE, and POLYLINE objects only.
;; Curves are ignored on purpose.
;; Highlights possible duplicate linework but does not delete anything.

(setq c:CHECKDOUBLES nil)

(setq *dld-tol* 1e-4)
(setq *dld-min-overlap* 1e-4)

(defun _dld-abs (x)
  (if (< x 0.0) (- x) x)
)

(defun _dld-min (a b)
  (if (< a b) a b)
)

(defun _dld-max (a b)
  (if (> a b) a b)
)

(defun _dld-pt2d (pt)
  (list (car pt) (cadr pt))
)

(defun _dld-vsub (a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b)))
)

(defun _dld-dot (a b)
  (+ (* (car a) (car b)) (* (cadr a) (cadr b)))
)

(defun _dld-cross (a b)
  (- (* (car a) (cadr b)) (* (cadr a) (car b)))
)

(defun _dld-len (v)
  (sqrt (_dld-dot v v))
)

(defun _dld-colinear-p (p1 p2 q1 q2 / d lenv)
  (setq d (_dld-vsub p2 p1)
        lenv (_dld-len d))
  (and
    (> lenv *dld-tol*)
    (<= (/ (_dld-abs (_dld-cross d (_dld-vsub q1 p1))) lenv) *dld-tol*)
    (<= (/ (_dld-abs (_dld-cross d (_dld-vsub q2 p1))) lenv) *dld-tol*)
  )
)

(defun _dld-overlap-length (p1 p2 q1 q2 / ax a1 a2 b1 b2 lo hi)
  (if (> (_dld-abs (- (car p2) (car p1))) (_dld-abs (- (cadr p2) (cadr p1))))
    (setq a1 (car p1) a2 (car p2) b1 (car q1) b2 (car q2))
    (setq a1 (cadr p1) a2 (cadr p2) b1 (cadr q1) b2 (cadr q2))
  )
  (setq lo (_dld-max (_dld-min a1 a2) (_dld-min b1 b2))
        hi (_dld-min (_dld-max a1 a2) (_dld-max b1 b2)))
  (- hi lo)
)

(defun _dld-segments-overlap-p (seg1 seg2 / p1 p2 q1 q2)
  (setq p1 (car seg1)
        p2 (cadr seg1)
        q1 (car seg2)
        q2 (cadr seg2))
  (and
    (_dld-colinear-p p1 p2 q1 q2)
    (> (_dld-overlap-length p1 p2 q1 q2) *dld-min-overlap*)
  )
)

(defun _dld-lwpoly-segments (ed / segs first prev vtx bul closed)
  (setq segs '()
        first nil
        prev nil
        closed (= 1 (logand 1 (cdr (assoc 70 ed)))))
  (while ed
    (if (= (caar ed) 10)
      (progn
        (setq vtx (_dld-pt2d (cdar ed))
              bul 0.0)
        (if (and (cdr ed) (= (caadr ed) 42))
          (setq bul (cdadr ed))
        )
        (if prev
          (if (<= (_dld-abs bul) *dld-tol*)
            (setq segs (cons (list prev vtx) segs))
          )
        )
        (if (null first)
          (setq first vtx)
        )
        (setq prev vtx)
      )
    )
    (setq ed (cdr ed))
  )
  (if (and closed prev first)
    (setq segs (cons (list prev first) segs))
  )
  (reverse segs)
)

(defun _dld-poly-segments (ent / nxt ed segs first prev bul closed done vtx)
  (setq nxt (entnext ent)
        segs '()
        first nil
        prev nil
        closed (= 1 (logand 1 (cdr (assoc 70 (entget ent)))))
        done nil)
  (while (and nxt (not done))
    (setq ed (entget nxt))
    (cond
      ((= (cdr (assoc 0 ed)) "VERTEX")
       (setq vtx (_dld-pt2d (cdr (assoc 10 ed)))
             bul (cdr (assoc 42 ed)))
       (if prev
         (if (<= (_dld-abs (if bul bul 0.0)) *dld-tol*)
           (setq segs (cons (list prev vtx) segs))
         )
       )
       (if (null first)
         (setq first vtx)
       )
       (setq prev vtx)
      )
      ((= (cdr (assoc 0 ed)) "SEQEND")
       (setq done T)
      )
    )
    (if (not done)
      (setq nxt (entnext nxt))
    )
  )
  (if (and closed prev first)
    (setq segs (cons (list prev first) segs))
  )
  (reverse segs)
)

(defun _dld-object-segments (ent / ed typ)
  (setq ed (entget ent)
        typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "LINE")
     (list
       (list
         (_dld-pt2d (cdr (assoc 10 ed)))
         (_dld-pt2d (cdr (assoc 11 ed)))
       )
     )
    )
    ((= typ "LWPOLYLINE")
     (_dld-lwpoly-segments ed)
    )
    ((= typ "POLYLINE")
     (_dld-poly-segments ent)
    )
    (T nil)
  )
)

(defun _dld-build-items (ss / i ent segs items)
  (setq i 0
        items '())
  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          segs (_dld-object-segments ent))
    (if segs
      (setq items (cons (cons ent segs) items))
    )
    (setq i (1+ i))
  )
  (reverse items)
)

(defun _dld-any-overlap-p (segs1 segs2 / s1 rest2 s2 found)
  (setq found nil)
  (while (and segs1 (not found))
    (setq s1 (car segs1)
          rest2 segs2)
    (while (and rest2 (not found))
      (setq s2 (car rest2))
      (if (_dld-segments-overlap-p s1 s2)
        (setq found T)
      )
      (setq rest2 (cdr rest2))
    )
    (setq segs1 (cdr segs1))
  )
  found
)

(defun _dld-add-if-missing (ent ss)
  (if (not (ssmemb ent ss))
    (ssadd ent ss)
  )
)

(defun c:DOUBLELINEDETECTOR ( / ss items left right a b out)
  (prompt "\nSelect linework to check for double lines: ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
  (cond
    ((not ss)
     (prompt "\nNothing selected.")
    )
    (T
     (setq items (_dld-build-items ss)
           out (ssadd)
           left items)
     (while left
       (setq a (car left)
             right (cdr left))
       (while right
         (setq b (car right))
         (if (_dld-any-overlap-p (cdr a) (cdr b))
           (progn
             (_dld-add-if-missing (car a) out)
             (_dld-add-if-missing (car b) out)
           )
         )
         (setq right (cdr right))
       )
       (setq left (cdr left))
     )
     (if (> (sslength out) 0)
       (progn
         (sssetfirst nil out)
         (prompt
           (strcat
             "\nFound "
             (itoa (sslength out))
             " object(s) with overlapping straight linework."
            )
         )
       )
       (prompt "\nNo overlapping straight linework found.")
     )
    )
  )
  (princ)
)
