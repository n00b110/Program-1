#lang racket

(define scores (file->lines "scores.txt"))

(displayln (length scores))
(for ([line scores])
  (printf "Line: ~a\n" line))


(define bowler-names
  (remove-duplicates
   (map
    (lambda (line)
      (first (string-split line " ")))
    scores)))


(define (extract-team-names lines)
  (let loop ([lines lines]
             [team-names '()])
    (cond
      [(empty? lines) (remove-duplicates team-names)]
      [(string-contains? (first lines) "Geeks")
       (loop (rest lines) (cons "Geeks" team-names))]
      [(string-contains? (first lines) "Oddballs")
       (loop (rest lines) (cons "Oddballs" team-names))]
      [else (loop (rest lines) team-names)])))



(define team-names (extract-team-names scores))
(for ([name team-names])
  (printf "Team Name: ~a\n" name))

(define (parse-frames lines)
  (regexp-match* #rx"([X0-9]{1,2} ?[/0-9]?)"))

(parse-frames (scores))





(for ([name bowler-names])
  (printf "Bowler Name: ~a\n" name))
