;;; -*- Gerbil -*-
; Based on RFC 8949: https://www.rfc-editor.org/rfc/rfc8949.html#name-major-types
(import :std/error
        :std/io
        :std/misc/bytes
        :std/sugar)
(export pack)

(defstruct u8slice (src start capacity) final: #t)

(def (slice-of vec start end: (end #f))
     (using ((vec :~ u8vector?)
             (start :~ (? (and fixnum? positive?))))
            
            (let (len (u8vector-length vec))
              (when (and end (fx> end len)))
              (make-u8slice vec (if end end len) len))))

(defmethod {ref u8slice}
  (lambda (self k)
    (using ((self :~ u8slice?)
            (k :~ (? (and fixnum? fxpositive?))))
           (when (fx> k self.end)
             (error k " out of bounds"))
           (u8vector-ref self.src (fx+ k self.start)))))

(defmethod {set! u8slice}
  (lambda (self i n)
    (using ((self :- u8slice)
            (i :~ (? (and fixnum? fxpositive?)))
            (n :~ )))))

(def MAXu8 255)
(def MAXu16 65535)
(def MAXu32 4294967295)

(def (smallest-int-container num)
  (when (not (fixnum? num))
    (error "Must provide fixnum"))
  (cond
    ((fx<= MAXu8 num)
     'u8)
    ((fx<= MAXu16 num)
     'u16)
    ((fx<= MAXu32 num)
     'u32)
    (else 'u64)))

(def (pad-count vec to)
  (let (veclen (len vec))
    (when (> veclen to)
      (error "length of the vector is longer than requested padding"))
    ()))

(def +majortyperegistry+ (make-vector 256 #f))

(def (read-cbor (input (current-input-port)))
  (using (r input : Reader)
    'TODO))

(def (unpack r)
  (using (r : Reader)
    (let (first-byte (make-u8vector 1))
      (Reader-read r first-byte need: 1))))

; Recursively packs almost anything
(def (pack obj writer hook: (hook #f))
     (using (writer : Writer)
            (cond
              ((number? obj)
               (pack-number obj writer))
              ; TODO: other cases
              ; TODO: handle this error more gracefully
              (else (if hook
                      (hook writer obj)
                      (error "incompatible type"))))))

; Handles data item generation by accepting a literal major-type, bitshifting it left,
; and adding the argument. The arg is assumed to be a natural number that corresponds to
; the size of the following item. The major type must not be 7
(defrule (write-sized-data-item writer major-type arg-value)
  (fixnum? major-type)
  ; TODO: check the 
  (let* ((item (fxarithmetic-shift-left major-type 5))
         (item (fxand item )))))

; TODO: there's a lot of space for optimizations here. We allocate a lot more than is
; strictly necessary
(def (pack-number obj writer)
     (using (writer :- Writer)
            (match obj
                   
                   ; when we have a number
                   ((string? obj)
                    ()
                    (and (? fxnum?) (? fxpositive?))
                    ; if the number is small, put it in the lower 5 bits of the major tag
                    (if (fx< 24 obj)
                      ; the first 3 bits must be 0, the rest are the value, so just write
                      ; the value directly. The first 3 are guaranteed to be 0 because
                      ; of the above check.
                      (writer.write-u8 (uint->u8vector obj))
                      ; Identify the number of bits we need so we can efficiently
                      ; pack the number
                      ; TODO: this could maybe be a macro
                      (let ((objbitlen (fxbit-count obj)))
                        (match (smallest-int-container obj)
                               ('u8
                                (writer.write-u8 #u8(0x18)))
                               ('u16
                                (writer.write-u8 #u8(0x19)))
                               ('u32
                                (writer.write-u8 #u8(0x1A))
                                (writer.write-u8 (make-u8vector (fxmodulo objbitlen 4) 0)))
                               ('u64
                                (writer.write-u8 #u8(0x1B))
                                (writer.write-u8 (make-u8vector (fxmodulo objbitlen 8) 0))))
                        (writer.write-u8 (uint->u8vector obj)))))
                   ((and (? fxnum?) (? fxnegative?))
                    (let* ((formattednum (1- (fxabs obj)))
                           (objbitlen (fxbit-count obj)))
                      (match (smallest-int-container obj)
                             ('u8
                              (writer.write-u8 #u8(0x38)))
                             ('u16
                              (writer.write-u8 #u8(0x39)))
                             ('u32
                              (writer.write-u8 #u8(0x3A))
                              (writer.write-u8 (make-u8vector (fxmodulo objbitlen 4) 0)))
                             ('u64
                              (writer.write-u8 #u8(0x3B))
                              (writer.write-u8 (make-u8vector (fxmodulo objbitlen 8) 0))))
                      (writer.write-u8 (uint->u8vector obj))))
                   ((? flonum?)
                    ; We always encode as double-precision floats for now
                    (writer.write-u8 #u8(0xfb))
                    (wirter.write-u8 (double->u8vector obj)))
                   ((? hash-table?)
                    'TODO))))

(def (double->u8vector f)
  (let (buf (make-u8vector 8))
    (u8vector-double-set! buf 0 f big)))

(def (float->u8vector f)
  (let (buf (make-u8vector 4))
    (u8vector-float-set! buf 0 f big)))
