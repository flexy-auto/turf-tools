;; SPEEDGANGSTER
;; Window-select LINE objects for cutting edges, then repeatedly
;; trim with two-click fence strokes.

(setq c:LINESELECT nil)

(defun _sg-trim-loop (ss / startPt endPt)
  (setq startPt (getpoint "\nClick start of trim line, or press Enter to finish: "))
  (while startPt
    (setq endPt (getpoint startPt "\nClick end of trim line: "))
    (if endPt
      ;; Run one fence trim stroke, then immediately allow another.
      (command "_.TRIM" ss "" "_Fence" startPt endPt "" "")
      (prompt "\nTrim cancelled for this stroke.")
    )
    (setq startPt (getpoint "\nClick start of trim line, or press Enter to finish: "))
  )
)

(defun c:SPEEDGANGSTER ( / pt1 pt2 ss)
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
              (prompt "\nCutting edges selected.")
              (_sg-trim-loop ss)
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
