; This file contains
(import :std/sugar
        :std/contract
        :std/iter
        :std/format
        :std/io)
(export default-decoder)

(def +unmarshal+ (make-vector 256 #f))

; handles formatting major type argument literals into byte
(def (data-item major-type arg)
     (let* ((out (fx+ (fxarithmetic-shift-left major-type 5)
                      arg))
            (bitcount (##fxbit-count out)))
       (if (fx> bitcount 8)
         (error "major type or arg too large. Bit count: " bitcount)
         out)))

(def (extract-raw-arg item buf)
     (using ((item :~ fixnum?)
             (buf :- BufferedReader))
            (fxand 31 item)))


(defrule (register major-type arg method)
         (vector-set! +unmarshal+ (data-item major-type arg) method))

(def (register-range major-type start stop method)
     (using ((major-type :~ fixnum?)
             (start :~ fixnum?)
             (stop :~ fixnum?))
            (for (i (in-range start (fx+ 1 stop)))
                 (register major-type i method))))

#;(default-decoder (open-buffered-reader #u8(18 20)))
(def (default-decoder buffer)
     (using (buffer : BufferedReader)
            ; read the first item
            (let* ((item (buffer.read-u8!))
                   (decode-method (vector-ref +unmarshal+ item)))
              (decode-method item buffer))))

(def (malformed-message item _)
  (error "Malformed message with initial byte " item))

(def (read-u8 item buf)
  (using (buf :- BufferedReader)
    (buf.read-u8!)))
(def (read-u16 item buf)
     (using (buf :- BufferedReader)
            (buf.read-u16)))
(def (read-u32 item buf)
     (using (buf :- BufferedReader)
            (buf.read-u32)))
(def (read-u64 item buf)
     (using (buf :- BufferedReader)
            (buf.read-u64)))

; Set up the jump table for decoding
(begin
  ; positive integers
  (register-range 0 0 23 extract-raw-arg)
  (register 0 24 read-u8)
  (register 0 25 read-u16)
  (register 0 26 read-u32)
  (register 0 27 read-u64)
  (register-range 0 28 31 malformed-message)
  ; negative integers
  (register-range 1 0 23 (lambda (item buf) (fx- -1 (extract-raw-arg item buf))))
  (register 1 24 (lambda (item buf) (- (read-u8 item buf))))
  (register 1 25 read-u16)
  (register 1 26 read-u32)
  (register 1 27 read-u64)
  (register-range 1 28 31 malformed-message)
  ; byte strings
  ; TODO: implement extracting byte slice from here instead of just reading arg
  (register-range 2 0 23 extract-raw-arg)
  (register 2 24 read-u8)
  (register 2 25 read-u16)
  (register 2 26 read-u32)
  (register 2 27 read-u64)
  ; TODO: this actually should indicate an indefinite length message
  (register-range 2 28 31 malformed-message)
  ; utf-8 strings
  ; TODO: implement decoding utf-8 instead of just getting the length
  (register-range 3 0 23 extract-raw-arg)
  (register 3 24 read-u8)
  (register 3 25 read-u16)
  (register 3 26 read-u32)
  (register 3 27 read-u64)
  ; TODO: this actually should indicate an indefinite length message
  (register-range 3 28 31 malformed-message)
  ; array
  ; TODO: implement handling of array members
  (register-range 4 0 23 extract-raw-arg)
  (register 4 24 read-u8)
  (register 4 25 read-u16)
  (register 4 26 read-u32)
  (register 4 27 read-u64)
  ; TODO: this actually should indicate an indefinite length message
  (register-range 4 28 31 malformed-message)
  ; map
  ; TODO: implement handling of mapping members
  (register-range 5 0 23 extract-raw-arg)
  (register 5 24 read-u8)
  (register 5 25 read-u16)
  (register 5 26 read-u32)
  (register 5 27 read-u64)
  ; TODO: this actually should indicate an indefinite length message
  (register-range 5 28 31 malformed-message)
  ; tagged data items
  ; TODO: handle tags
  (register-range 6 0 23 extract-raw-arg)
  (register 6 24 read-u8)
  (register 6 25 read-u16)
  (register 6 26 read-u32)
  (register 6 27 read-u64)
  (register-range 6 28 31 malformed-message)
  ; floating point and others...
  ; TODO: hanlde floats and other data items
  (register-range 7 0 23 extract-raw-arg)
  (register 7 24 read-u8)
  (register 7 25 read-u16)
  (register 7 26 read-u32)
  (register 7 27 read-u64)
  (register-range 7 28 30 malformed-message)
  (register 7 31 (lambda (_) ('END))))
; validates that this works
#;(for ((item (iter +unmarshal+))
      (i (in-iota 256)))
     (begin
       (displayln (format "~a=~a" i item))
       (when (not item)
         (error "Unset item in +unmarshal+"))))
