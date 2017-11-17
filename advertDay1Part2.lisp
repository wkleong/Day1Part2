
; wkleong@gmail.com
;Day 1: No Time for a Taxicab ---
;Santa's sleigh uses a very high-precision clock to guide its movements, and the clock's oscillator is regulated by stars. Unfortunately, the stars have been stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve all fifty stars by December 25th.
;Collect stars by solving puzzles. Two puzzles will be made available on each day in the advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
;You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", unfortunately, is as close as you can get - the instructions on the Easter Bunny Recruiting Document the Elves intercepted start here, and nobody had time to work them out further.
;The Document indicates that you should start at the given coordinates (where you just landed) and face North. Then, follow the provided sequence: either turn left (L) or right (R) 90 degrees, then walk forward the given number of blocks, ending at a new intersection.
;There's no time to follow such ridiculous instructions on foot, though, so you take a moment and work out the destination. Given that you can only walk on the street grid of the city, how far is the shortest path to the destination?
;For example:
;Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
;R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
;R5, L5, R5, R3 leaves you 12 blocks away.
;How many blocks away is Easter Bunny HQ? The puzzle answer was 231.
;--- Part Two ---
;Then, you notice the instructions continue on the back of the Recruiting Document. Easter Bunny HQ is actually at the first location you visit twice.
;For example, if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East.
;How many blocks away is the first location you visit twice? The puzzle answer was 147.

(defvar orig-document "R5, R4, R2, L3, R1, R1, L4, L5, R3, L1, L1, R4, L2, R1, R4, R4, L2, L2, R4, L4, R1, R3, L3, L1, L2, R1, R5, L5, L1, L1, R3, R5, L1, R4, L5, R5, R1, L185, R4, L1, R51, R3, L2, R78, R1, L4, R188, R1, L5, R5, R2, R3, L5, R3, R4, L1, R2, R2, L4, L4, L5, R5, R4, L4, R2, L5, R2, L1, L4, R4, L4, R2, L3, L4, R2, L3, R3, R2, L2, L3, R4, R3, R1, L4, L2, L5, R4, R4, L1, R1, L5, L1, R3, R1, L2, R1, R1, R3, L4, L1, L3, R2, R4, R2, L2, R1, L5, R3, L3, R3, L1, R4, L3, L3, R4, L2, L1, L3, R2, R3, L2, L1, R4, L3, L5, L2, L4, R1, L4, L4, R3, R5, L4, L1, L1, R4, L2, R5, R1, R1, R2, R1, R5, L1, L3, L5, R2")
;(defvar orig-document "R8, R4, R4, R8")
(setq new-doc (remove #\, orig-document))
(defun from-string-to-list (sss)
  (let ((L (read-from-string 
           (concatenate 'string "(" sss ")"))))
    L))
(setq docList (from-string-to-list new-doc))
(defun gen-step-list (docList)
(let ((stepList ))
  (dolist (stepp docList)
    (setq LL (subseq (string stepp) 0 1))
    (setq RR (parse-integer (subseq (string stepp) 1)))
    (if (= (length stepList) 0)
        (setq stepList (list (list LL RR)))
        (setq stepList (cons (list LL RR) stepList))
    )
  )
  (reverse stepList)
))
(setq stepList (gen-step-list docList))

(defun find-bunny (stepList)
(let ((xCnt 0) (yCnt 0) (DiR "N") givenD givenS)
  (dolist (stepp stepList)
    (setq givenD (car stepp))
    (setq givenS (cadr stepp))
    (cond
      ((string= givenD "R")
        (cond
          ((string= DiR "N") (setq DiR "E") (togglePath xCnt yCnt "+" "x" givenS) (setq xCnt (+ xCnt givenS)))
          ((string= DiR "E") (setq DiR "S") (togglePath xCnt yCnt "-" "y" givenS) (setq yCnt (- yCnt givenS)))
          ((string= DiR "S") (setq DiR "W") (togglePath xCnt yCnt "-" "x" givenS) (setq xCnt (- xCnt givenS)))
          ((string= DiR "W") (setq DiR "N") (togglePath xCnt yCnt "+" "y" givenS) (setq yCnt (+ yCnt givenS)))
        )
      )
      ((string= givenD "L")
        (cond
          ((string= DiR "N") (setq DiR "W") (togglePath xCnt yCnt "-" "x" givenS) (setq xCnt (- xCnt givenS)))
          ((string= DiR "E") (setq DiR "N") (togglePath xCnt yCnt "+" "y" givenS) (setq yCnt (+ yCnt givenS)))
          ((string= DiR "S") (setq DiR "E") (togglePath xCnt yCnt "+" "x" givenS) (setq xCnt (+ xCnt givenS)))
          ((string= DiR "W") (setq DiR "S") (togglePath xCnt yCnt "-" "y" givenS) (setq yCnt (- yCnt givenS)))
        )
      )
    )
  )
(+ (abs xCnt) (abs yCnt))
))

(defun togglePath (xx yy opt axiS amount)
(let (i amtList fpoint (FOUND nil) (ret nil))
    (setq amtList (loop for i from 1 to amount collect i))
    (cond
      ((and (string= opt "+") (string= axiS "x"))
        (dolist (i amtList)
            (toggleGrid (+ i xx) yy)
        ))
      ((and (string= opt "-") (string= axiS "x"))
        (dolist (i amtList)
            (toggleGrid (- xx i) yy)
        ))
      ((and (string= opt "+") (string= axiS "y"))
        (dolist (i amtList)
            (toggleGrid xx (+ i yy))
        ))
      ((and (string= opt "-") (string= axiS "y"))
        (dolist (i amtList)
            (toggleGrid xx (- yy i))
        ))
    )
))

(defvar foundD nil)
(defvar firstPoint nil)

(defun toggleGrid (xx yy)
(let (s1 s2)
    (setq s1 (intern (write-to-string xx)))
    (setq s2 (intern (write-to-string yy)))
    (if (not (get s1 s2))
        (setf (get s1 s2) 1)
        (progn
            (when (not foundD)
                (setq foundD t)
                (setq firstPoint (+ (abs xx) (abs yy))))
        )
    )
))

(format t "~%Day 1 Part 1: The bunny is ~D blocks away!" (find-bunny stepList))
(format t "~%Day 1 Part 2: The bunny's first location visited twice is ~D blocks away!" firstPoint)
