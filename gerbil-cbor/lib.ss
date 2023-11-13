;;; -*- Gerbil -*-
; Based on RFC 8949: https://www.rfc-editor.org/rfc/rfc8949.html#name-major-types
(import
  (only-in "decoder" default-decoder)
  :std/error
  :gerbil/gambit
  :std/io
  :std/contract
  :std/misc/bytes
  :std/sugar)
(export pack
        (struct-out CBORCodec))

(defstruct CBORCodec (encoder decoder) final: #t
  constructor: :init!)

(defmethod {:init! CBORCodec}
  (lambda ((encoder default-encoder)
           (decoder default-decoder))
    (make-CBORCodec encoder decoder)))


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

(def (read-cbor (input (current-input-port)))
  (using (input : BufferedReader)
    'TODO))

(def (unpack r)
  (using (r : BufferedReader)
    (let (first-byte (BufferedReader-read-u8! r))
      '(TODO))))

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
     (using (writer : BufferedWriter)
            (match obj
                   ; when we have a number
                   ((and (? fixnum?) (? fxpositive?))
                    ; if the number is small, put it in the lower 5 bits of the major tag
                    (if (fx< 24 obj)
                      ; the first 3 bits must be 0, the rest are the value, so just write
                      ; the value directly. The first 3 are guaranteed to be 0 because
                      ; of the above check.
                      (BufferedWriter-write-u8 writer (uint->u8vector obj))
                      ; Identify the number of bits we need so we can efficiently
                      ; pack the number
                      ; TODO: this could maybe be a macro
                      (let ((objbitlen (fxbit-count obj)))
                        (match (smallest-int-container obj)
                               ('u8
                                (BufferedWriter-write-u8 writer #u8(24)))
                               ('u16
                                (BufferedWriter-write-u8 writer #u8(25)))
                               ('u32
                                (BufferedWriter-write-u8 writer #u8(26))
                                (BufferedWriter-write-u8 writer (make-u8vector (fxmodulo objbitlen 4) 0)))
                               ('u64
                                (BufferedWriter-write-u8 writer #u8(27))
                                (BufferedWriter-write-u8 writer (make-u8vector (fxmodulo objbitlen 8) 0))))
                        (BufferedWriter-write-u8 writer (uint->u8vector obj)))))
                   ((and (? fixnum?) (? fxnegative?))
                    (let* ((formattednum (1- (fxabs obj)))
                           (objbitlen (##fxbit-count obj)))
                      (match (smallest-int-container obj)
                             ('u8
                              (BufferedWriter-write-u8 writer #u8(56)))
                             ('u16
                              (BufferedWriter-write-u8 writer #u8(57)))
                             ('u32
                              (BufferedWriter-write-u8 writer #u8(58))
                              (BufferedWriter-write-u8 writer (make-u8vector (fxmodulo objbitlen 4) 0)))
                             ('u64
                              (BufferedWriter-write-u8 writer #u8(59))
                              (BufferedWriter-write-u8 writer (make-u8vector (fxmodulo objbitlen 8) 0))))
                      (BufferedWriter-write-u8 writer (uint->u8vector obj))))
                   ((? flonum?)
                    ; We always encode as double-precision floats for now
                    (BufferedWriter-write-u8 writer #u8(251))
                    (BufferedWriter-write-u8 writer (double->u8vector obj)))
                   ((? hash-table?)
                    'TODO))))

(def (double->u8vector f)
  (let (buf (make-u8vector 8))
    (u8vector-double-set! buf 0 f big)))

(def (float->u8vector f)
  (let (buf (make-u8vector 4))
    (u8vector-float-set! buf 0 f big)))
