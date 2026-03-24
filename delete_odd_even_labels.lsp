(defun _doel-extract-trailing-number (txt / i ch digits)
  (setq i (strlen txt))
  (setq digits "")

  (while (> i 0)
    (setq ch (substr txt i 1))
    (if (and (>= ch "0") (<= ch "9"))
      (setq digits (strcat ch digits))
      (setq i 0)
    )
    (if (> i 0)
      (setq i (- i 1))
    )
  )

  (if (/= digits "")
    (atoi digits)
    nil
  )
)

(defun _doel-delete-match (ss parity / i ent ed txt num deleted)
  (setq deleted 0)

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ed (entget ent))
        (setq txt (cdr (assoc 1 ed)))
        (setq num (_doel-extract-trailing-number txt))

        (if num
          (if (or
                (and (= parity "Even") (= 0 (rem num 2)))
                (and (= parity "Odd") (/= 0 (rem num 2)))
              )
            (progn
              (entdel ent)
              (setq deleted (+ deleted 1))
            )
          )
        )

        (setq i (+ i 1))
      )
    )
  )

  deleted
)

(defun c:DELETEEVENODD ( / choice deleted ss)
  (initget "Odd Even")
  (setq choice (getkword "\nDelete which labels [Odd/Even]? "))

  (if choice
    (progn
      (prompt "\nSelect MTEXT labels to check: ")
      (setq ss (ssget '((0 . "MTEXT"))))
      (if ss
        (progn
          (setq deleted (_doel-delete-match ss choice))
          (prompt
            (strcat
              "\nDeleted "
              (itoa deleted)
              " "
              choice
              " labels."
            )
          )
        )
        (prompt "\nNothing selected.")
      )
    )
    (prompt "\nCommand cancelled.")
  )

  (princ)
)
