;; ==========================================================================
;; Routine: Day 002/1000 - Total Length by Color (DCL Summary)
;; Command: TLENC
;; Description: Window-select any mixed geometry. Displays an on-screen DCL
;;              dialog box summarizing total lengths categorized by color.
;; ==========================================================================

(vl-load-com)

(defun pad-str (s len)
  (while (< (strlen s) len)
    (setq s (strcat s " "))
  )
  s
)

(defun get-color-name (c)
  (cond
    ((= c 1) "1 (Red)")
    ((= c 2) "2 (Yellow)")
    ((= c 3) "3 (Green)")
    ((= c 4) "4 (Cyan)")
    ((= c 5) "5 (Blue)")
    ((= c 6) "6 (Magenta)")
    ((= c 7) "7 (White/Black)")
    (t (strcat "Color " (itoa c)))
  )
)

(defun show-length-dcl (colList / dclFile fp dclId grandTotal outList item)
  (setq dclFile (strcat (getvar "TEMPPREFIX") "color_summary.dcl"))
  (setq fp (open dclFile "w"))
  (write-line "color_len_dialog : dialog {" fp)
  (write-line "  label = \"Total Length by Color Summary\";" fp)
  (write-line "  : list_box {" fp)
  (write-line "    key = \"col_list\";" fp)
  (write-line "    width = 46;" fp)
  (write-line "    height = 12;" fp)
  (write-line "    fixed_width_font = true;" fp)
  (write-line "  }" fp)
  (write-line "  : text {" fp)
  (write-line "    key = \"tot_text\";" fp)
  (write-line "    alignment = centered;" fp)
  (write-line "  }" fp)
  (write-line "  ok_only;" fp)
  (write-line "}" fp)
  (close fp)
  
  (setq dclId (load_dialog dclFile))
  (if (new_dialog "color_len_dialog" dclId)
    (progn
      (setq grandTotal 0.0)
      (setq outList '("COLOR / INDEX       |  TOTAL LENGTH"
                      "--------------------+--------------------"))
      (foreach item colList
        (setq grandTotal (+ grandTotal (cdr item)))
        (setq outList (cons (strcat (pad-str (get-color-name (car item)) 19)
                                    " |  "
                                    (rtos (cdr item) 2 2))
                            outList))
      )
      (setq outList (reverse outList))
      
      (start_list "col_list")
      (mapcar 'add_list outList)
      (end_list)
      
      (set_tile "tot_text" (strcat "GRAND TOTAL: " (rtos grandTotal 2 2) " units"))
      (start_dialog)
      (unload_dialog dclId)
    )
  )
  (if (findfile dclFile) (vl-file-delete dclFile))
)

(defun c:TLENC ( / ss i ent obj len col entData layData colList )
  (princ "\nSelect lines, polylines, arcs, splines, or circles: ")
  (if (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,CIRCLE,ELLIPSE"))))
    (progn
      (setq colList nil)
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq entData (entget ent))
        (setq obj (vlax-ename->vla-object ent))
        (setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
        
        ;; Read explicit color or layer color (ByLayer fallback)
        (setq col (cdr (assoc 62 entData)))
        (if (null col)
          (progn
            (setq layData (tblsearch "LAYER" (cdr (assoc 8 entData))))
            (setq col (abs (cdr (assoc 62 layData))))
          )
        )
        (if (null col) (setq col 7))
        
        ;; Accumulate totals
        (if (assoc col colList)
          (setq colList (subst (cons col (+ (cdr (assoc col colList)) len))
                               (assoc col colList)
                               colList))
          (setq colList (cons (cons col len) colList))
        )
      )
      
      ;; Sort by color index ascending
      (setq colList (vl-sort colList '(lambda (a b) (< (car a) (car b)))))
      
      ;; Launch DCL Summary Box
      (show-length-dcl colList)
    )
    (princ "\nNo valid linear objects selected.")
  )
  (princ)
)
