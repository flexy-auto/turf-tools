;; LINESELECT
;; Window-select LINE objects and leave them selected.

(setq c:SPEEDTRIM nil)

(defun c:LINESELECT ( / pt1 pt2 ss)
  (prompt "\nPick first corner of cutting-edge window: ")
  (setq pt1 (getpoint))

  (if pt1
    (progn
      (prompt "\nPick opposite corner of cutting-edge window: ")
      (setq pt2 (getcorner pt1))

      (if pt2
        (progn
          (setq ss (ssget "_W" pt1 pt2 '((0 . "LINE"))))
          (if ss
            (progn
              (sssetfirst nil ss)
            )
            (prompt "\nNo LINE objects found in that window.")
          )
        )
        (prompt "\nCommand cancelled.")
      )
    )
    (prompt "\nCommand cancelled.")
  )

  (princ)
)
