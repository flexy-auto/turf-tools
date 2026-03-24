(setq *tt-label-height* 5.0)
(setq *tt-label-width* 15.2130)
(setq *tt-label-box-height* 8.5)
(setq *tt-label-grid-divisions* 20.0)
(setq *tt-label-clearance* 0.5)

;; Remove old command names from earlier versions after reload.
(setq c:LABELS_BORDER nil)
(setq c:LABEL_BORDERS nil)

(defun _pt-in-verts (verts pt / n i j xi yi xj yj inside)
  (setq n (length verts))
  (setq inside nil)
  (setq i 0)
  (setq j (- n 1))

  (while (< i n)
    (setq xi (car (nth i verts)))
    (setq yi (cadr (nth i verts)))
    (setq xj (car (nth j verts)))
    (setq yj (cadr (nth j verts)))

    (if (and
          (/= (> yi (cadr pt)) (> yj (cadr pt)))
          (< (car pt)
             (+ xi
                (/ (* (- xj xi) (- (cadr pt) yi))
                   (- yj yi)
                )
             )
          )
        )
      (setq inside (not inside))
    )

    (setq j i)
    (setq i (+ i 1))
  )

  inside
)

(defun _pt-in-lwpoly (ename pt / verts)
  (setq verts (_lwpoly-verts ename))
  (_pt-in-verts verts pt)
)

(defun _pt-line-dist (p a b / ax ay bx by px py dx dy proj qx qy)
  (setq ax (car a) ay (cadr a))
  (setq bx (car b) by (cadr b))
  (setq px (car p) py (cadr p))
  (setq dx (- bx ax))
  (setq dy (- by ay))

  (if (and (= dx 0.0) (= dy 0.0))
    (distance p a)
    (progn
      (setq proj (/ (+ (* (- px ax) dx) (* (- py ay) dy))
                    (+ (* dx dx) (* dy dy))))
      (if (< proj 0.0) (setq proj 0.0))
      (if (> proj 1.0) (setq proj 1.0))
      (setq qx (+ ax (* proj dx)))
      (setq qy (+ ay (* proj dy)))
      (distance p (list qx qy 0.0))
    )
  )
)

(defun _label-box-sample-points (p halfW halfH / x y)
  (setq x (car p))
  (setq y (cadr p))
  (list
    (list x y 0.0)
    (list (- x halfW) y 0.0)
    (list (+ x halfW) y 0.0)
    (list x (- y halfH) 0.0)
    (list x (+ y halfH) 0.0)
    (list (- x halfW) (- y halfH) 0.0)
    (list (- x halfW) (+ y halfH) 0.0)
    (list (+ x halfW) (- y halfH) 0.0)
    (list (+ x halfW) (+ y halfH) 0.0)
    (list (- x (* 0.5 halfW)) y 0.0)
    (list (+ x (* 0.5 halfW)) y 0.0)
    (list x (- y (* 0.5 halfH)) 0.0)
    (list x (+ y (* 0.5 halfH)) 0.0)
    (list (- x halfW) (- y (* 0.5 halfH)) 0.0)
    (list (- x halfW) (+ y (* 0.5 halfH)) 0.0)
    (list (+ x halfW) (- y (* 0.5 halfH)) 0.0)
    (list (+ x halfW) (+ y (* 0.5 halfH)) 0.0)
    (list (- x (* 0.5 halfW)) (- y halfH) 0.0)
    (list (+ x (* 0.5 halfW)) (- y halfH) 0.0)
    (list (- x (* 0.5 halfW)) (+ y halfH) 0.0)
    (list (+ x (* 0.5 halfW)) (+ y halfH) 0.0)
  )
)

(defun _get-label-prefix ( / val)
  (setq val (getstring T "\nEnter label prefix <A>: "))
  (if (= val "")
    "A"
    val
  )
)

(defun c:LABELBORDERS ( / ss i ent ed typ items obj area cen regEnt txtnum sorted prefix)

  (vl-load-com)

  ;; make sure tags layer exists
  (if (not (tblsearch "LAYER" "tags"))
    (entmakex
      (list
        '(0 . "LAYER")
        '(100 . "AcDbSymbolTableRecord")
        '(100 . "AcDbLayerTableRecord")
        (cons 2 "tags")
        (cons 62 6)
        (cons 70 0)
      )
    )
  )

  ;; force tags layer to magenta
  (command "_.-LAYER" "_Color" "6" "tags" "")

  (setq prefix (_get-label-prefix))

  (prompt "\nWindow-select closed polylines to label: ")
  (setq ss (ssget))

  (if ss
    (progn
      (setq items '())
      (setq i 0)

      ;; collect closed LWPOLYLINE objects only
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        (setq typ (cdr (assoc 0 ed)))

        (if (and
              (= typ "LWPOLYLINE")
              (= 1 (logand 1 (cdr (assoc 70 ed))))
            )
          (progn
            ;; area for outer/inner sorting if needed
            (setq area (vla-get-Area (vlax-ename->vla-object ent)))

            ;; choose a point that is inside the polyline
            (setq cen (_poly-label-point
                        ent
                        (* 0.5 *tt-label-width*)
                        (* 0.5 *tt-label-box-height*)
                      )
            )

            ;; store: (x y area point ent)
            (setq items
              (cons (list (car cen) (cadr cen) area cen ent) items)
            )
          )
        )

        (setq i (+ i 1))
      )

      (if items
        (progn
          ;; Order by field zones:
          ;; End Zone 1 columns first, then Logo, then End Zone 2 columns.
          (setq sorted (_sort-label-items items))

          ;; place labels
          (setq txtnum 0)
          (foreach item sorted
            (entmakex
              (list
                '(0 . "MTEXT")
                '(100 . "AcDbEntity")
                (cons 8 "tags")
                '(100 . "AcDbMText")
                (cons 10 (nth 3 item))
                (cons 40 *tt-label-height*)
                (cons 41 *tt-label-width*)
                (cons 1 (strcat prefix (itoa txtnum)))
                (cons 71 5)
                (cons 72 5)
              )
            )
            (setq txtnum (+ txtnum 1))
          )
        )
        (prompt "\nNo closed LWPOLYLINE objects found in selection.")
      )
    )
  )

  (princ)
)

(defun _sort-label-items (items / xsorted bounds tagged)
  (setq xsorted (_sort-items-left-right items))
  (setq bounds (_find-band-boundaries xsorted))
  (setq tagged '())

  (foreach item xsorted
    (setq tagged
      (cons
        (list
          (_zone-priority (_item-band item bounds))
          (_item-band item bounds)
          item
        )
        tagged
      )
    )
  )

  (setq tagged
    (vl-sort tagged
      (function
        (lambda (a b)
          (if (/= (car a) (car b))
            (< (car a) (car b))
            (if (/= (cadr a) (cadr b))
              (< (cadr a) (cadr b))
              (if (/= (car (nth 2 a)) (car (nth 2 b)))
                (< (car (nth 2 a)) (car (nth 2 b)))
                (> (cadr (nth 2 a)) (cadr (nth 2 b)))
              )
            )
          )
        )
      )
    )
  )

  (mapcar '(lambda (x) (nth 2 x)) tagged)
)

(defun _sort-items-left-right (items)
  (vl-sort items
    (function
      (lambda (a b)
        (if (/= (car a) (car b))
          (< (car a) (car b))
          (> (cadr a) (cadr b))
        )
      )
    )
  )
)

(defun _find-band-boundaries (items / gaps i leftItem rightItem gap sortedGaps chosen)
  (setq gaps '())
  (setq i 1)
  (while (< i (length items))
    (setq leftItem (nth (- i 1) items))
    (setq rightItem (nth i items))
    (setq gap (- (car rightItem) (car leftItem)))
    (setq gaps
      (cons
        (list
          gap
          (/ (+ (car leftItem) (car rightItem)) 2.0)
        )
        gaps
      )
    )
    (setq i (+ i 1))
  )

  (setq sortedGaps
    (vl-sort gaps
      (function (lambda (a b) (> (car a) (car b))))
    )
  )

  (setq chosen '())
  (setq i 0)
  (while (and (< i 4) (< i (length sortedGaps)))
    (setq chosen (cons (cadr (nth i sortedGaps)) chosen))
    (setq i (+ i 1))
  )

  (vl-sort chosen '<)
)

(defun _item-band (item bounds / x band)
  (setq x (car item))
  (setq band 1)
  (foreach b bounds
    (if (> x b)
      (setq band (+ band 1))
    )
  )
  band
)

(defun _zone-priority (band)
  ;; Overall left-to-right layout:
  ;; 1 = End Zone 2
  ;; 2 = End Zone 1
  ;; 3 = Logo
  ;; 4 = End Zone 2
  ;; 5 = End Zone 1
  (cond
    ((or (= band 2) (= band 5)) 1)
    ((= band 3) 2)
    (T 3)
  )
)

(defun _poly-centroid (ename / obj doc ms copyObj arr regObj cen regName)
  (setq obj (vlax-ename->vla-object ename))
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms  (vla-get-ModelSpace doc))

  ;; copy original polyline so real geometry stays untouched
  (setq copyObj (vla-Copy obj))

  ;; create a region from the copied polyline
  (setq arr (vlax-make-safearray vlax-vbObject '(0 . 0)))
  (vlax-safearray-put-element arr 0 copyObj)

  (setq regObj
    (car
      (vlax-safearray->list
        (vlax-variant-value
          (vla-AddRegion ms arr)
        )
      )
    )
  )

  ;; centroid from temporary region
  (setq cen
    (vlax-safearray->list
      (vlax-variant-value
        (vla-get-Centroid regObj)
      )
    )
  )

  ;; clean up temp objects
  (vla-Delete regObj)
  (vla-Delete copyObj)

  cen
)

(defun _poly-label-point (ename halfW halfH / cen scan best verts bbox target)
  ;; Use the exact centroid when it is valid, otherwise compute a
  ;; guaranteed interior point from a horizontal scanline.
  (setq verts (_lwpoly-verts ename))
  (setq bbox (_poly-bbox verts))
  (setq cen (_poly-centroid ename))
  (setq target (_poly-target-point verts bbox cen halfH))
  (setq best (_poly-best-fit-point verts bbox halfW halfH target))
  (if best
    best
    (if (_pt-in-verts verts cen)
      cen
      (progn
        (setq scan (_poly-scanline-point verts bbox))
        (if scan scan cen)
      )
    )
  )
)

(defun _poly-target-point (verts bbox cen halfH / probe fallback)
  ;; For long strip-like pieces, build an approximate centerline from
  ;; opposite boundary points and target the midpoint of that centerline.
  (setq probe (_poly-strip-target-point verts halfH))
  (if (null probe)
    (setq probe (_poly-axis-target-point verts halfH))
  )
  (if probe
    probe
    (list (/ (+ (nth 0 bbox) (nth 2 bbox)) 2.0) (cadr cen) 0.0)
  )
)

(defun _poly-strip-target-point (verts halfH / perim halfPerim samples step i s
                                       p1 p2 mid mids best requiredWidth)
  (setq perim (_poly-perimeter verts))
  (if (<= perim 0.0)
    nil
    (progn
      (setq halfPerim (/ perim 2.0))
      (setq samples 48)
      (setq step (/ halfPerim (+ samples 0.0)))
      (setq i 0)
      (setq mids '())
      (setq requiredWidth (* 2.0 (+ halfH *tt-label-clearance*)))

      (while (< i samples)
        (setq s (* i step))
        (setq p1 (_point-at-dist-closed verts s))
        (setq p2 (_point-at-dist-closed verts (+ s halfPerim)))
        (setq mid (_midpoint p1 p2))
        ;; Keep a little width data so we can prefer parts of the strip
        ;; that can actually hold the label.
        (if (_pt-in-verts verts mid)
          (setq mids (append mids (list (list mid (distance p1 p2)))))
        )
        (setq i (+ i 1))
      )

      (setq best (_centerline-midpoint mids requiredWidth))
      (if best
        best
        (if mids
          (car (nth (fix (/ (length mids) 2)) mids))
          nil
        )
      )
    )
  )
)

(defun _centerline-midpoint (mids requiredWidth / filtered pathLen halfLen i segLen accum
                                  a b targetDist frac)
  (setq filtered '())
  (foreach item mids
    (if (>= (cadr item) requiredWidth)
      (setq filtered (append filtered (list item)))
    )
  )
  (if (null filtered)
    (setq filtered mids)
  )
  (if (< (length filtered) 2)
    (if filtered (caar filtered) nil)
    (progn
      (setq pathLen 0.0)
      (setq i 1)
      (while (< i (length filtered))
        (setq pathLen
          (+ pathLen
             (distance (car (nth (- i 1) filtered))
                       (car (nth i filtered))
             )
          )
        )
        (setq i (+ i 1))
      )

      (setq halfLen (/ pathLen 2.0))
      (setq accum 0.0)
      (setq i 1)
      (while (< i (length filtered))
        (setq a (car (nth (- i 1) filtered)))
        (setq b (car (nth i filtered)))
        (setq segLen (distance a b))
        (if (>= (+ accum segLen) halfLen)
          (progn
            (setq targetDist (- halfLen accum))
            (setq frac (if (> segLen 0.0) (/ targetDist segLen) 0.0))
            (setq i (length filtered))
            (setq accum
              (list
                (+ (car a) (* frac (- (car b) (car a))))
                (+ (cadr a) (* frac (- (cadr b) (cadr a))))
                0.0
              )
            )
          )
          (setq accum (+ accum segLen))
        )
        (setq i (+ i 1))
      )
      (if (listp accum) accum (car (nth (fix (/ (length filtered) 2)) filtered)))
    )
  )
)

(defun _poly-axis-target-point (verts halfH / axis origin localVerts localBox
                                      mint maxt width steps requiredHeight
                                      midt i axisPos hits pair height bestPt bestScore)
  (setq axis (_poly-major-axis verts))
  (setq origin (nth 0 axis))
  (setq localVerts (_transform-verts verts (nth 0 axis) (nth 1 axis) (nth 2 axis)))
  (setq localBox (_poly-bbox localVerts))
  (setq mint (nth 0 localBox))
  (setq maxt (nth 2 localBox))
  (setq width (- maxt mint))
  (setq steps 24)
  (setq requiredHeight (* 2.0 (+ halfH *tt-label-clearance*)))
  (setq midt (/ (+ mint maxt) 2.0))
  (setq i 0)
  (setq bestPt nil)
  (setq bestScore nil)

  (while (<= i steps)
    ;; Start at the midpoint along the piece and alternate outward.
    (setq axisPos
      (+ midt
         (* (/ width (+ steps 0.0))
            (_center-out-offset i)
         )
      )
    )
    (if (and (>= axisPos mint) (<= axisPos maxt))
      (progn
        (setq hits (_vertical-line-intersections localVerts axisPos))
        (while (and hits (cdr hits))
          (setq pair (list (car hits) (cadr hits)))
          (setq height (- (cadr pair) (car pair)))
          (if (>= height requiredHeight)
            (progn
              (setq bestPt (_local-to-world origin (nth 1 axis) (nth 2 axis)
                                            axisPos (/ (+ (car pair) (cadr pair)) 2.0)))
              (setq i (+ steps 1))
              (setq hits nil)
            )
            (if (or (null bestScore)
                    (> height bestScore)
                    (and (= height bestScore)
                         (< (abs (- axisPos midt))
                            (abs (- (_world-to-local-t bestPt origin (nth 1 axis)) midt))
                         )
                    )
                )
              (progn
                (setq bestScore height)
                (setq bestPt (_local-to-world origin (nth 1 axis) (nth 2 axis)
                                              axisPos (/ (+ (car pair) (cadr pair)) 2.0)))
              )
            )
          )
          (if hits
            (setq hits (cddr hits))
          )
        )
      )
    )
    (setq i (+ i 1))
  )

  bestPt
)

(defun _poly-major-axis (verts / origin sxx syy sxy p rx ry ang ux uy vx vy)
  (setq origin (_avg-point verts))
  (setq sxx 0.0)
  (setq syy 0.0)
  (setq sxy 0.0)
  (foreach p verts
    (setq rx (- (car p) (car origin)))
    (setq ry (- (cadr p) (cadr origin)))
    (setq sxx (+ sxx (* rx rx)))
    (setq syy (+ syy (* ry ry)))
    (setq sxy (+ sxy (* rx ry)))
  )
  (setq ang (* 0.5 (atan (* 2.0 sxy) (- sxx syy))))
  (setq ux (cos ang))
  (setq uy (sin ang))
  (if (< ux 0.0)
    (progn
      (setq ux (- ux))
      (setq uy (- uy))
    )
  )
  (setq vx (- uy))
  (setq vy ux)
  (list origin (list ux uy 0.0) (list vx vy 0.0))
)

(defun _center-out-offset (idx)
  (if (= idx 0)
    0.0
    (if (= 0 (rem idx 2))
      (/ idx -2.0)
      (/ (+ idx 1) 2.0)
    )
  )
)

(defun _transform-verts (verts origin u v / out p relx rely)
  (setq out '())
  (foreach p verts
    (setq relx (- (car p) (car origin)))
    (setq rely (- (cadr p) (cadr origin)))
    (setq out
      (cons
        (list
          (+ (* relx (car u)) (* rely (cadr u)))
          (+ (* relx (car v)) (* rely (cadr v)))
          0.0
        )
        out
      )
    )
  )
  out
)

(defun _local-to-world (origin u v axisPos offset)
  (list
    (+ (car origin) (* axisPos (car u)) (* offset (car v)))
    (+ (cadr origin) (* axisPos (cadr u)) (* offset (cadr v)))
    0.0
  )
)

(defun _world-to-local-t (p origin u / relx rely)
  (setq relx (- (car p) (car origin)))
  (setq rely (- (cadr p) (cadr origin)))
  (+ (* relx (car u)) (* rely (cadr u)))
)

(defun _avg-point (pts / sx sy n p)
  (setq sx 0.0)
  (setq sy 0.0)
  (setq n 0)
  (foreach p pts
    (setq sx (+ sx (car p)))
    (setq sy (+ sy (cadr p)))
    (setq n (+ n 1))
  )
  (list (/ sx n) (/ sy n) 0.0)
)

(defun _midpoint (a b)
  (list
    (/ (+ (car a) (car b)) 2.0)
    (/ (+ (cadr a) (cadr b)) 2.0)
    0.0
  )
)

(defun _poly-perimeter (verts / total i j)
  (setq total 0.0)
  (setq i 0)
  (setq j (- (length verts) 1))
  (while (< i (length verts))
    (setq total (+ total (distance (nth j verts) (nth i verts))))
    (setq j i)
    (setq i (+ i 1))
  )
  total
)

(defun _point-at-dist-closed (verts distAlong / perim d i j a b segLen frac)
  (setq perim (_poly-perimeter verts))
  (if (<= perim 0.0)
    (car verts)
    (progn
      (setq d distAlong)
      (while (>= d perim)
        (setq d (- d perim))
      )
      (while (< d 0.0)
        (setq d (+ d perim))
      )
      (setq i 0)
      (setq j (- (length verts) 1))
      (while (< i (length verts))
        (setq a (nth j verts))
        (setq b (nth i verts))
        (setq segLen (distance a b))
        (if (<= d segLen)
          (progn
            (setq frac (if (> segLen 0.0) (/ d segLen) 0.0))
            (setq i (length verts))
            (setq d
              (list
                (+ (car a) (* frac (- (car b) (car a))))
                (+ (cadr a) (* frac (- (cadr b) (cadr a))))
                0.0
              )
            )
          )
          (setq d (- d segLen))
        )
        (setq j i)
        (setq i (+ i 1))
      )
      (if (listp d) d (car verts))
    )
  )
)

(defun _poly-best-fit-point (verts bbox halfW halfH target / xmin ymin xmax ymax
                                   width height sx sy x y p best bestScore score)
  (setq xmin (nth 0 bbox))
  (setq ymin (nth 1 bbox))
  (setq xmax (nth 2 bbox))
  (setq ymax (nth 3 bbox))
  (setq width (- xmax xmin))
  (setq height (- ymax ymin))
  (setq sx (max (/ width *tt-label-grid-divisions*) 1.0))
  (setq sy (max (/ height *tt-label-grid-divisions*) 1.0))
  (setq x (+ xmin (/ sx 2.0)))
  (setq best nil)
  (setq bestScore nil)

  (while (< x xmax)
    (setq y (+ ymin (/ sy 2.0)))
    (while (< y ymax)
      (setq p (list x y 0.0))
      (if (_pt-in-verts verts p)
        (progn
          (setq score (_poly-box-score verts p halfW halfH target))
          (if (or (null bestScore) (> score bestScore))
            (progn
              (setq bestScore score)
              (setq best p)
            )
          )
        )
      )
      (setq y (+ y sy))
    )
    (setq x (+ x sx))
  )

  best
)

(defun _poly-box-score (verts p halfW halfH target / samples okCount total dist edgeDist clearW clearH dx dy)
  (setq clearW (+ halfW *tt-label-clearance*))
  (setq clearH (+ halfH *tt-label-clearance*))
  (setq samples (_label-box-sample-points p clearW clearH))
  (setq okCount 0)
  (setq total 0)
  (foreach s samples
    (setq total (+ total 1))
    (if (_pt-in-verts verts s)
      (setq okCount (+ okCount 1))
    )
  )
  (setq dx (abs (- (car p) (car target))))
  (setq dy (abs (- (cadr p) (cadr target))))
  (setq dist (distance p target))
  (setq edgeDist (_poly-edge-clearance verts p))
  (if (= okCount total)
    ;; A fully fitting box with clearance always outranks partial fits.
    (+ 1000000.0
       (- 10000.0 (* dx 2500.0) (* dy 250.0) (* dist 50.0))
       (* edgeDist 100.0)
    )
    ;; If no fully clear position exists, maximize fit first, then stay near center.
    (+ (* okCount 10000.0)
       (- 1000.0 (* dx 250.0) (* dy 25.0) (* dist 10.0))
       edgeDist
    )
  )
)

(defun _poly-edge-clearance (verts p / n i j d best)
  (setq n (length verts))
  (setq i 0)
  (setq j (- n 1))
  (setq best nil)
  (while (< i n)
    (setq d (_pt-line-dist p (nth i verts) (nth j verts)))
    (if (or (null best) (< d best))
      (setq best d)
    )
    (setq j i)
    (setq i (+ i 1))
  )
  (if best best 0.0)
)

(defun _poly-scanline-point (verts bbox / xmin ymin xmax ymax
                                   height steps i y hits pair width bestPt bestWidth)
  (setq xmin (nth 0 bbox))
  (setq ymin (nth 1 bbox))
  (setq xmax (nth 2 bbox))
  (setq ymax (nth 3 bbox))
  (setq height (- ymax ymin))

  (if (<= height 0.0)
    nil
    (progn
      (setq steps 64)
      (setq i 1)
      (setq bestPt nil)
      (setq bestWidth 0.0)

      (while (< i steps)
        ;; Skip the exact top/bottom of the box so we do not land on edges.
        (setq y (+ ymin (* height (/ i (+ steps 0.0)))))
        (setq hits (_scanline-intersections verts y))

        (while (and hits (cdr hits))
          (setq pair (list (car hits) (cadr hits)))
          (setq width (- (cadr pair) (car pair)))
          (if (> width bestWidth)
            (progn
              (setq bestWidth width)
              (setq bestPt (list (/ (+ (car pair) (cadr pair)) 2.0) y 0.0))
            )
          )
          (setq hits (cddr hits))
        )

        (setq i (+ i 1))
      )

      bestPt
    )
  )
)

(defun _scanline-intersections (verts y / hits n i j xi yi xj yj x)
  (setq hits '())
  (setq n (length verts))
  (setq i 0)
  (setq j (- n 1))

  (while (< i n)
    (setq xi (car (nth i verts)))
    (setq yi (cadr (nth i verts)))
    (setq xj (car (nth j verts)))
    (setq yj (cadr (nth j verts)))

    ;; Half-open test prevents double counting when the scanline hits a vertex.
    (if (/= (> yi y) (> yj y))
      (progn
        (setq x
          (+ xi
             (/ (* (- xj xi) (- y yi))
                (- yj yi)
             )
          )
        )
        (setq hits (cons x hits))
      )
    )

    (setq j i)
    (setq i (+ i 1))
  )

  (vl-sort hits '<)
)

(defun _vertical-line-intersections (verts x / hits n i j xi yi xj yj y)
  (setq hits '())
  (setq n (length verts))
  (setq i 0)
  (setq j (- n 1))

  (while (< i n)
    (setq xi (car (nth i verts)))
    (setq yi (cadr (nth i verts)))
    (setq xj (car (nth j verts)))
    (setq yj (cadr (nth j verts)))

    ;; Half-open test prevents double counting when the slice hits a vertex.
    (if (/= (> xi x) (> xj x))
      (progn
        (setq y
          (+ yi
             (/ (* (- yj yi) (- x xi))
                (- xj xi)
             )
          )
        )
        (setq hits (cons y hits))
      )
    )

    (setq j i)
    (setq i (+ i 1))
  )

  (vl-sort hits '<)
)

(defun _lwpoly-verts (ename / ed verts)
  (setq ed (entget ename))
  (setq verts '())
  (foreach d ed
    (if (= (car d) 10)
      (setq verts (append verts (list (cdr d))))
    )
  )
  verts
)

(defun _poly-bbox (verts / p xmin ymin xmax ymax)
  (setq p (car verts))
  (setq xmin (car p))
  (setq xmax (car p))
  (setq ymin (cadr p))
  (setq ymax (cadr p))

  (foreach p (cdr verts)
    (if (< (car p) xmin) (setq xmin (car p)))
    (if (> (car p) xmax) (setq xmax (car p)))
    (if (< (cadr p) ymin) (setq ymin (cadr p)))
    (if (> (cadr p) ymax) (setq ymax (cadr p)))
  )

  (list xmin ymin xmax ymax)
)
