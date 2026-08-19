;; ==========================================================================
;; Routine: Day 003/1000 - Auto-Numbering Bubble Callouts
;; Command: BNUM
;; Description: Prompts for starting number and text height, then calculates
;;              circle radius automatically for sequential callouts.
;; ==========================================================================

(defun c:BNUM ( / pt th rad num str curTh )
  ;; Default starting number memory
  (if (null *bnum_start*) (setq *bnum_start* 1))
  (setq num (getint (strcat "\nEnter starting number [" (itoa *bnum_start*) "]: ")))
  (if (null num) (setq num *bnum_start*))
  
  ;; Text height prompt with memory
  (setq curTh (getvar "TEXTSIZE"))
  (if (zerop curTh) (setq curTh 2.5))
  (if (null *bnum_th*) (setq *bnum_th* curTh))
  
  (setq th (getdist (strcat "\nSpecify text height [" (rtos *bnum_th* 2 2) "]: ")))
  (if (null th) (setq th *bnum_th*))
  (setq *bnum_th* th)
  
  ;; Automatically calculate optimal circle radius (1.25x text height)
  (setq rad (* th 1.25))
  
  (princ "\nClick anywhere to place numbered bubbles (Press Enter or ESC to finish)...")
  
  (while (setq pt (getpoint "\nPick insertion point: "))
    (setq str (itoa num))
    
    ;; Create Circle using entmake
    (entmake
      (list
        (cons 0 "CIRCLE")
        (cons 10 pt)
        (cons 40 rad)
      )
    )
    
    ;; Create Middle-Center Text using entmake
    (entmake
      (list
        (cons 0 "TEXT")
        (cons 10 pt)
        (cons 11 pt)
        (cons 40 th)
        (cons 1 str)
        (cons 72 1) ; Horizontal: Center
        (cons 73 2) ; Vertical: Middle
      )
    )
    
    (setq num (1+ num))
  )
  
  (setq *bnum_start* num)
  (princ)
)
