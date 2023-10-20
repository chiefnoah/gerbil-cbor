;;; -*- Gerbil -*-
; Based on RFC 8949: https://www.rfc-editor.org/rfc/rfc8949.html#name-major-types
(import :std/error
        :std/io
        :std/misc/bytes
        :std/sugar)
(export pack)

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

; TODO: there's a lot of space for optimizations here. We allocate a lot more than is
; strictly necessary
(def (pack-number obj writer)
     (using (writer :- Writer)
            (match obj
                   
                   ; when we have a number
                   ((and (? fxnum?) (? fxpositive?))
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
                                (writer.write-u8 (make-u8vector (fxmodulo fxbit-count 4) 0)))
                               ('u64
                                (writer.write-u8 #u8(0x1B))
                                (writer.write-u8 (make-u8vector (fxmodulo fxbit-count 8) 0))))
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
                    ()))))

(def (double->u8vector f)
  (let (buf (make-u8vector 8))
    (u8vector-double-set! buf 0 f big)))

(def (float->u8vector f)
  (let (buf (make-u8vector 4))
    (u8vector-float-set! buf 0 f big)))
