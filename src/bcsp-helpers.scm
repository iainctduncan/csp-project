; music calculation functions

(define *num-octaves* 6)

(define *pitches*
  '(C Db D Eb E F Gb G Ab A Bb B))

(define *octaves* (range 0 *num-octaves*))

; naive, only handles flat classes right now
(define pitch->int (hash-table  
  'C 0  'Db 1  'D 2  'Eb 3  'E 4  'F 5  'Gb 6  'G 7  'Ab 8  'A 9  'Bb 10  'B 11))

(define int->pitch (hash-table  
  0 'C   1 'Db   2 'D   3 'Eb   4 'E   5 'F   6 'Gb   7 'G   8 'Ab   9 'A   10 'Bb  11 'B))

; for interval use, we prf-u and dim-5 (no aug-4)
(define interval->semitones (hash-table
  'prf-u 0    'min-2 1    'maj-2 2    'min-3 3     'maj-3 4     'prf-4 5  
  'dim-5 6    'prf-5 7    'min-6 8    'maj-6 9     'min-7 10    'maj-7 11
  'prf-8 12   'min-9 13   'maj-9 14   'min-10 15   'maj-10 16   'prf-11 17  
  'dim-12 18  'prf-12 19  'min-13 20  'maj-13 21   'min-14 22   'maj-14 23
))

(define semitones->interval (hash-table
  0  'prf-u     1  'min-2     2  'maj-2     3  'min-3     4  'maj-3     5  'prf-4   
  6  'dim-5     7  'prf-5     8  'min-6     9  'maj-6     10 'min-7     11 'maj-7 
  12 'prf-8     13 'min-9     14 'maj-9     15 'min-10    16 'maj-10    17 'prf-11 
  18 'dim-12    19 'prf-12    20 'min-13    21 'maj-13    22 'min-14    23 'maj-14 
))

; build hashmap for note symbol to pitch/oct/note-nums for fast conversion
; between note (C5, etc) to '(C 0)
(define note->pitch-oct-num (hash-table))
(define pitch-oct->note-num (hash-table))
(define note-symbols (make-vector (* 12 *num-octaves*)))

; init code to populate the translation hashes above
(dolist (pc *pitches*)
  (dolist (oct *octaves*)
    (let* ((note-sym (symbol (format #f "~s~s" pc oct)))
           (oct-offset (* 12 oct))
           (pitch-int  (pitch->int pc))
           (note-num   (+ oct-offset pitch-int)))
      ;(post "pitch" pc "oct" oct "sym" note-sym "note-num" note-num)
      (set! (note->pitch-oct-num note-sym) (list pc oct note-num))
      (set! (pitch-oct->note-num (list pc oct)) note-num)
      (set! (note-symbols note-num) note-sym)
      )))

(define (note->pitch note)
  ((note->pitch-oct-num note) 0))

(define (note->oct note)
  ((note->pitch-oct-num note) 1))

(define (note->note-num note)
  ((note->pitch-oct-num note) 2))

(define (semitones-between n1 n2)
  "return interval in semi-tones between notes"
  ;(post "(semitones-between)" n1 n2)
  (let* ((note-num-1 (note->note-num n1))
         (note-num-2 (note->note-num n2))
         (semitones (abs (- note-num-1 note-num-2))))
    ;(post "  - semis:" semitones)     
    semitones))       

(define (interval-between n1 n2)
  "return interval between notes as symbol 'maj-3 etc"
  (let* ((note-num-1 (note->note-num n1))
         (note-num-2 (note->note-num n2))
         (semitones (abs (- note-num-1 note-num-2))))
    semitones))       

; todo add more
(define chord-intervals (hash-table
  'Maj7  '(prf-u maj-3 prf-5 maj-7)
  'Min7  '(prf-u min-3 prf-5 min-7)
  'Dom7  '(prf-u maj-3 prf-5 min-7)))

(define chord->semitones (hash-table  
  'Maj7  '(0 4 7 11)
  'Min7  '(0 3 7 10)
  'Dom7  '(0 4 7 10)))

; TODO, add all enharmonics
(define rnum->int (hash-table
  'I 0  'bII 1  'II 2  'bIII 3  'III 4   'IV 5  '#IV 6  'bV 6
  'V 7  'bVI 8  'VI 9  'bVII 10 'VII 11))


(define (root-pitch key-sym root-rnum)
  "return the root pitch for a chord rnum and key"
  (let* ((root-int (rnum->int root-rnum))
         (pitch (int->pitch root-int)))
    ;(post "(root-pitch)" key-sym root-rnum "root-int:" root-int "pitch:" pitch)
    pitch))       

(define (root-chord->note-nums root chord-q)
  "return note-nums in first octave for a chord"
  ;(post "(root-chord->note-nums)" root chord-q)
  (let* ((root-int (rnum->int root))
         (chord-semis (chord->semitones chord-q))
         (chord-note-nums (map (lambda(x)(+ x root-int)) chord-semis)))
    ;(post "  res:" chord-note-nums)
    chord-note-nums))       

(define (root-chord->pitches root chord)
  "return ordered list of pitches for a chord"
  ;(post "(root-chord->pitches)" root chord)
  (let* ((note-nums (root-chord->note-nums root chord))
         (pitches (map (lambda(n)(int->pitch (modulo n 12))) note-nums)))
    pitches))       

; return true if pitches are equal or enharmonically equal
(define (enh-eq? a b)
  ;(post "enh-eq?" a b)
  ; TODO implement the enharmonic later
  (eq? a b))
        
; version of member that works on vectors or lists        
(define (in? needle seq)
  (let ((res #f))
    (dotimes (i (length seq))
      (if (eq? needle (seq i)) (set! res #t)))
    res))  
