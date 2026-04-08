;; SETANNOTATION
;; Updates the current dimension style settings by changing the
;; system variables that control that dimstyle in the current drawing.
;;
;; Settings applied:
;; - DIMSCALE = user-entered overall scale
;; - DIMLUNIT = 4 (Architectural)
;; - DIMDEC   = 1 (Precision displayed as 1/2" for architectural units)
;; - DIMTIH   = 0 (Text inside extension lines aligns with dim line)
;; - DIMTOH   = 0 (Text outside extension lines aligns with dim line)

(vl-load-com)

(setq c:SETANNONTATION nil)

(defun c:SETANNOTATION ( / scale dimanno)
  (initget 6)
  (setq scale (getreal "\nEnter dimension overall scale: "))

  (cond
    ((null scale)
     (prompt "\nSETANNOTATION cancelled.")
    )
    ((<= scale 0.0)
     (prompt "\nInvalid scale value. Nothing changed.")
    )
    (T
     ;; Fit tab -> Use overall scale of
     (setvar "DIMSCALE" scale)

     ;; Primary Units tab -> Unit Format = Architectural
     (setvar "DIMLUNIT" 4)

     ;; Primary Units tab -> Precision = 1/2"
     ;; For architectural units, DIMDEC controls the displayed precision.
     ;; A value of 1 corresponds to halves.
     (setvar "DIMDEC" 1)

     ;; Text tab -> Text Alignment = Aligned with Dimension Line
     ;; These two settings keep text aligned whether it falls
     ;; inside or outside the extension lines.
     (setvar "DIMTIH" 0)
     (setvar "DIMTOH" 0)

     ;; Warn if annotative dimensions may conflict with DIMSCALE behavior.
     (setq dimanno
       (vl-catch-all-apply 'getvar (list "DIMANNO"))
     )
     (if (and
           (not (vl-catch-all-error-p dimanno))
           (= dimanno 1)
         )
       (prompt
         "\nWarning: Annotative dimensions are enabled (DIMANNO=1), so annotative behavior may affect how DIMSCALE is used."
       )
     )

     (prompt
       (strcat
         "\nSETANNOTATION applied. Overall scale set to "
         (rtos scale 2 3)
         "."
       )
     )
    )
  )
  (princ)
)
