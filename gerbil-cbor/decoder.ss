; This file contains
(import :std/sugar
        :std/contract
        :std/iter
        :std/format
        :std/text/utf8
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

(def (default-decoder buffer)
     (using (buffer : BufferedReader)
            ; read the first item
            (let* ((item (buffer.read-u8!))
                   (decode-method (vector-ref +unmarshal+ item)))
              (decode-method item buffer))))

(def (malformed-message item _)
  (error "Malformed message with initial byte " item))

(def (read-u8 _ buf)
  (using (buf :- BufferedReader)
    (buf.read-u8!)))
(def (read-u16 _ buf)
     (using (buf :- BufferedReader)
            (buf.read-u16)))
(def (read-u32 _ buf)
     (using (buf :- BufferedReader)
            (buf.read-u32)))
(def (read-u64 _ buf)
     (using (buf :- BufferedReader)
            (buf.read-u64)))

; Reads a list from the buffered and decodes it recursively
(def (read-list item buf f (decoder default-decoder))
  (let f ((count (f item buf))
          (item (decoder buf)))
    (if (= 1 count)
      ; properly terminate the list
      (cons item '())
      (cons item (f (1- count)
                    (decoder buf))))))

; only the value associated with the *last* instance of a key is returned. That is,
; if there are duplicates, we overwrite any existing keys.
(def (read-map item buf f (decoder default-decoder) (table (make-hash-table)))
     (let f ((count (1- (f item buf)))
             (key (decoder buf))
             (value (decoder buf)))
       (begin
         (hash-put! table key value)
         (if (positive? count)
           (f (1- count)
              (decoder buf)
              (decoder buf))
           table))))

; TODO: do this without copying the buffer
(def (read-utf8-string item buf f)
  (utf8->string (read-bytes item buf f)))

(def (read-bytes item buf f)
     (using (buf :- BufferedReader)
            (let* ((count (f item buf))
                   (bytebuffer (make-u8vector count))
                   (readcount (Reader-read buf bytebuffer)))
              bytebuffer)))

(def (read-negative item buf f)
  (fx- -1 (f item buf)))

(defrule (des f r)
  (lambda (item buf)
    (f item buf r)))

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
  (register 1 24 (des read-negative read-u8))
  (register 1 25 (des read-negative read-u16))
  (register 1 26 (des read-negative read-u32))
  ; fixnums can only occupy up to 62-bits, not the full 64 bit register, so we use the
  ; slower `-`. In general, it should only be slower for values that *must* fit into a
  ; 64-bit register because we pack ints into the smallest container
  (register 1 27 (lambda (item buf) (- -1 (read-u64 item buf))))
  (register-range 1 28 31 malformed-message)
  ; byte strings
  (register-range 2 0 23 (des read-bytes extract-raw-arg))
  (register 2 24 (des read-bytes read-u8))
  (register 2 25 (des read-bytes read-u16))
  (register 2 26 (des read-bytes read-u32))
  (register 2 27 (des read-bytes read-u64))
  ; TODO: this actually should indicate an indefinite length message
  (register-range 2 28 31 malformed-message)
  ; utf-8 strings
  (register-range 3 0 23 (des read-utf8-string extract-raw-arg))
  (register 3 24 (des read-utf8-string read-u8))
  (register 3 25 (des read-utf8-string read-u16))
  (register 3 26 (des read-utf8-string read-u32))
  (register 3 27 (des read-utf8-string read-u64))
  ; TODO: this actually should indicate an indefinite length message
  (register-range 3 28 31 malformed-message)
  ; array
  (register-range 4 0 23 (des read-list extract-raw-arg))
  (register 4 24 (des read-list read-u8))
  (register 4 25 (des read-list read-u16))
  (register 4 26 (des read-list read-u32))
  (register 4 27 (des read-list read-u64))
  ; TODO: this actually should indicate an indefinite length message
  (register-range 4 28 31 malformed-message)
  ; map
  (register-range 5 0 23 (des read-map extract-raw-arg))
  (register 5 24 (des read-map read-u8))
  (register 5 25 (des read-map read-u16))
  (register 5 26 (des read-map read-u32))
  (register 5 27 (des read-map read-u64))
  ; TODO: this actually should indicate an indefinite length message
  (register-range 5 28 31 malformed-message)
  ; tagged data items
  ; TODO: handle tags
  (register-range 6 0 23 (des read-map extract-raw-arg))
  (register 6 24 (des read-map read-u8))
  (register 6 25 (des read-map read-u16))
  (register 6 26 (des read-map read-u32))
  (register 6 27 (des read-map read-u64))
  (register-range 6 28 31 malformed-message)
  ; floating point and others...
  ; TODO: hanlde floats and other data items
  (register-range 7 0 23 extract-raw-arg)
  (register 7 24 read-u8)
  (register 7 25 read-u16)
  (register 7 26 read-u32)
  (register 7 27 read-u64)
  (register-range 7 28 30 malformed-message)
  ; The special end terminator for indefinite-length data types
  (register 7 31 (lambda (_ _) ('END))))
; validates that this works
#;(for ((item (iter +unmarshal+))
        (i (in-iota 256)))
     (begin
       (displayln (format "~a=~a" i item))
       (when (not item)
         (error "Unset item in +unmarshal+"))))
