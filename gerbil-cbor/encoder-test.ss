(import
  :std/contract
  :std/io
  :std/test
  :std/sugar
  "encoder"
  "decoder")
(export #t)

(defrule (simple-roundtrip arg)
  (stx-fixnum? #'arg)
  (using (writer (open-buffered-writer #f) :- BufferedWriter)
         (default-encoder writer arg)
         (let* ((buffer (get-buffer-output-u8vector writer))
                (_ (displayln "Buffer: " buffer))
                (reader (open-buffered-reader buffer)))
           (check (default-decoder reader) => arg))))

(def cbor-encoder-test
     (test-suite "cbor/encoder"
                 (test-case "roundtrip simple int" (simple-roundtrip 3))
                 (test-case "roundtrip u8 int" (simple-roundtrip 123))
                 (test-case "roundtrip u8 int" (simple-roundtrip 50000))
                 (test-case "roundtrip u32 int" (simple-roundtrip 800000))
                 (test-case "roundtrip u64 int" (simple-roundtrip 1844674407370955161))))
