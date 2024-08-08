; music calculation functions

(define *num-octaves* 6)

(define *pitch-classes*
  '(C Db D Eb E F Gb G Ab A Bb B))

(define *octaves* (range 0 *num-octaves*))

; naive, only handles flat classes right now
(define pitch-class->int (hash-table  
  'C 0  'Db 1  'D 2  'Eb 3  'E 4  'F 5  'Gb 6  'G 7  'Ab 8  'A 9  'Bb 10  'B 11))

(define int->pitch-class (hash-table  
  0 'C   1 'Db   2 'D   3 'Eb   4 'E   5 'F   6 'Gb   7 'G   8 'Ab   9 'A   10 'Bb  11 'B))

(define interval->steps (hash-table
  'prf-u 0    'root 0     'min-2 1    'maj-2 2     'min-3 3     'maj-3 4    'prf-4 5  
  'aug-4 6    'dim-5 6    'prf-5 7    'min-6 8     'maj-6 9     'min-7 10   'maj-7 11
  'prf-8 12   'min-9 13   'maj-9 14   'min-10 15   'maj-10 16   'prf-11 17  'aug-11 18  
  'dim-12 18  'prf-12 19  'min-13 20  'maj-13 21   'min-14 22   'maj-14 23
))

; build hashmap for note symbol to pitch/oct/note-nums for fast conversion
; between note (C5, etc) to '(C 0)
(define note->pitch-oct-num (hash-table))
(define pitch-oct->note-num (hash-table))
(define note-symbols (make-vector (* 12 *num-octaves*)))

(dolist (pc *pitch-classes*)
  (dolist (oct *octaves*)
    (let* ((note-sym (symbol (format #f "~s~s" pc oct)))
           (oct-offset (* 12 oct))
           (pitch-int  (pitch-class->int pc))
           (note-num   (+ oct-offset pitch-int)))
      ;(post "pitch" pc "oct" oct "sym" note-sym "note-num" note-num)
      (set! (note->pitch-oct-num note-sym) (list pc oct))
      (set! (pitch-oct->note-num (list pc oct)) note-num)
      (set! (note-symbols note-num) note-sym)
      )))

(define (note->pitch note)
  ((note->pitch-oct-num note) 0))

(define (note->oct note)
  ((note->pitch-oct-num note) 1))

(define (note->note-num note)
  ((note->pitch-oct-num note) 2))

(define chord-intervals (hash-table
  'Maj-7  '(root maj-3 prf-5 maj-7)
  'Min-7  '(root min-3 prf-5 min-7)
  'Dom-7  '(root maj-3 prf-5 min-7)))

(define (interval-between n1 n2)
  "return interval in steps between notes of the form (C . 1) (D .3)"
  (begin))
  
  



        

