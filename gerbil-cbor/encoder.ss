(import
  :std/sugar
  :std/contract
  "util"
  :std/io
  :std/misc/alist
  :std/srfi/19
  :std/misc/bytes
  :std/error
  :std/text/utf8)
(export default-encoder current-encoder (struct-out cbor-tag))

(defrule (match-encoder writer item (predicate encode) ... rest)
  (match item
    ((? predicate) (encode writer item)) ... rest))

(defstruct cbor-tag (tag item)
  final: #t)

(def (default-hook writer item (encoder (current-encoder)))
     (using (writer :- BufferedWriter)
            (match item
                   ((? date?)
                    (encoder (make-cbor-tag 0 (date->string item))))
                   ; TODO: other types
                   (else (error "Don't know how to serialize item: " item)))))

(def current-hook (make-parameter default-hook))

(def (default-encoder buf item)
     (using (buf : BufferedWriter)
            (match-encoder buf item
              (number? write-number)
              (null? write-null)
              (hash-table? write-hashmap)
              (u8vector? write-u8vector)
              (vector? write-vector)
              (string? write-string)
              (alist? write-alist)
              (cbor-tag? write-tag)
              (##proper-list? write-list)
              (else ((current-hook) buf item)))))

(def current-encoder (make-parameter default-encoder))

(def MAXu8 255)
(def MAXu16 65535)
(def MAXu32 4294967295)

(def (smallest-int-container num)
  (cond
    ((fx<= num MAXu8)
     'u8)
    ((fx<= num MAXu16)
     'u16)
    ((fx<= num MAXu32)
     'u32)
    (else 'u64)))

(defrule (match-for-int-size writer major-type item int-bytes (size type-arg) ...)
         (match (smallest-int-container item)
                (size (begin
                        (BufferedWriter-write-u8 writer (data-item major-type type-arg))
                        (BufferedWriter-write writer int-bytes))) ...))

(def (write-positive-uint writer major-type item)
     (using (writer :- BufferedWriter)
       (if (fx< item 24)
         (writer.write-u8 (data-item 0 item))
         (let (int-bytes (uint->u8vector item))
           (match-for-int-size writer major-type item int-bytes
             ('u8  24)
             ('u16 25)
             ('u32 26)
             ('u64 27))))))

(def (write-number writer item)
     (using ((writer :- BufferedWriter)
             ; TODO: remove this when it's been tested thuroughly
             (item :~ fixnum?))
            (cond
              ((and (integer? item) (positive? item) (fixnum? item))
               (write-positive-uint writer 0 item))
              ((and (integer? item) (negative? item))
               (write-positive-uint writer 1 (abs (1+ item))))
              ((flonum? item)
               (let (buf (make-u8vector 8 0))
                 (u8vector-double-set! buf 0 item big)
                 ; we do not currently support writing single-precision floats
                 (writer.write-u8 (data-item 7 27))
                 (writer.write buf)))
              (else
                (BUG write-number "This function should not be called with non-numbers" item)))))

(def (write-list writer item (encoder (current-encoder)))
  (using ((writer :- BufferedWriter)
          ; this is probably O(n)
          (item :~ ##proper-list?))
    (writer.write-u8 (data-item 4 31))
    (for-each encoder item)
    ; terminate the indefinite sequence
    (writer.write-u8 (data-item 7 31))))

(def (write-u8vector writer item)
  (using ((writer :- BufferedWriter)
          (item :~ u8vector?))
    (write-positive-uint writer 2 (u8vector-length item))
    (writer.write item)))

(def (write-vector writer item (encoder (current-encoder)))
  (using ((writer :- BufferedWriter)
          (item :~ vector?))
    (write-positive-uint writer 4 (vector-length item))
    (vector-for-each encoder)))

(def (write-hashmap writer item (encoder (current-encoder)))
  (using ((writer :- BufferedWriter)
          (item :~ hash-table?))
    (write-positive-uint writer 5 (hash-length item))
    (hash-for-each
      (lambda (key value)
        (encoder writer key)
        (encoder writer value)) item)))

(def (write-alist writer item (encoder (current-encoder)))
  (using ((writer :- BufferedWriter)
          (item :~ alist?))
    (for-each (lambda (pair)
                (encoder (car pair))
                (if (list? (cdr pair))
                  (encoder (cadr pair))
                  (encoder (cdr pair)))))
    (writer.write-u8 (data-item 7 31))))

(def (write-string writer item)
  (using ((writer :- BufferedWriter)
          (item :~ string?))
    (write-positive-uint writer 3 (string-length item))
    (writer.write-u8 (string->utf8 item))))

(def (write-bool writer item)
  (using ((writer :- BufferedWriter)
          (item :~ boolean?))
    (writer.write-u8 (data-item 7 (if item 21 20)))))

(def (write-void writer item)
  (using ((writer :- BufferedWriter)
          (item :~ void?))
    (writer.write-u8 (data-item 7 23))))

(def (write-null writer item)
  (using ((writer :- BufferedWriter)
          (item :~ null?))
    (writer.write-u8 (data-item 7 22))))

(def (write-tag writer item (encoder (current-encoder)))
  (using ((writer :- BufferedWriter)
          (item :- cbor-tag))
    (write-positive-uint writer (item.tag))
    (encoder item.item)))
