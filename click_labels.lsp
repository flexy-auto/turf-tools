(setq *cl-label-height* 5.0)
(setq *cl-label-width* 15.2130)

(defun _cl-ensure-tags-layer ( / )
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

  (command "_.-LAYER" "_Color" "6" "tags" "")
)

(defun _cl-get-prefix ( / val)
  (setq val (getstring T "\nEnter label prefix <A>: "))
  (if (= val "")
    "A"
    val
  )
)

(defun _cl-get-start-number ( / val)
  (initget 4)
  (setq val (getint "\nEnter starting number <0>: "))
  (if val
    val
    0
  )
)

(defun _cl-make-label (pt text /)
  (entmakex
    (list
      '(0 . "MTEXT")
      '(100 . "AcDbEntity")
      (cons 8 "tags")
      '(100 . "AcDbMText")
      (cons 10 pt)
      (cons 40 *cl-label-height*)
      (cons 41 *cl-label-width*)
      (cons 1 text)
      (cons 71 5)
      (cons 72 5)
    )
  )
)

(defun c:CLICKLABELS ( / prefix num pt)
  (vl-load-com)
  (_cl-ensure-tags-layer)

  (setq prefix (_cl-get-prefix))
  (setq num (_cl-get-start-number))

  (prompt "\nClick points to place labels. Press Enter when finished.")
  (setq pt (getpoint (strcat "\nPick point for " prefix (itoa num) ": ")))

  (while pt
    (_cl-make-label pt (strcat prefix (itoa num)))
    (setq num (+ num 1))
    (setq pt (getpoint (strcat "\nPick point for " prefix (itoa num) ": ")))
  )

  (princ)
)
