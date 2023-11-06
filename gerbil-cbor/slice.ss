(import
  :std/sugar
  :std/contract)

(defstruct u8slice (src start capacity) final: #t)

; end is inclusive
(def (slice-of vec start end: (end #f))
     (using ((vec :~ u8vector?)
             (start :~ (? (and fixnum? positive?))))
            (let (len (u8vector-length vec))
              (when (and end (fx> end len))
                (error ""))
              (make-u8slice vec start (1+ (if end (- end start) (- len start)))))))

(defmethod {ref u8slice}
  (lambda (self k)
    (using ((self :- u8slice)
            (k :~ (? (and fixnum? fxpositive?))))
           (when (fx> k self.capacity)
             ; TODO: better error handling
             (error k "out of bounds"))
           (u8vector-ref self.src (fx+ k self.start)))))

(defmethod {set! u8slice}
  (lambda (self i n)
    (using ((self :- u8slice)
            (i :~ (? (and fixnum? fxpositive?)))
            (n :~ ?fixnum))
           (when (fx> i (u8vector-length self.src))
             ; TODO: better error handling
             (error "Index out of range for backing vector: " i self.src))
           (u8vector-set! self.src (+ self.start i) n))))

(defmethod {lenth u8slice}
  (lambda (self)
    (using (self :- u8slice)
      (displayln "oop"))))
