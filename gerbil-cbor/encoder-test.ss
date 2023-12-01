(import
  :std/contract
  :std/io
  :std/test
  :std/sugar
  "encoder"
  "decoder")
(export #t)


(def (simple-encode-decode arg)
     (using (writer (open-buffered-writer #f) :- BufferedWriter)
            (default-encoder writer arg)
            (let* ((buffer (get-buffer-output-u8vector writer))
                   (reader (open-buffered-reader buffer)))
              (default-decoder reader))))

(defrules roundtrip-check ()
  ((_ arg pred)
   (check (simple-encode-decode arg) ? pred))
  ((_ arg)
   (check (simple-encode-decode arg) => arg)))

(def cbor-roundtrip-test
     (test-suite "cbor/roundtrip"
                 (test-case "simple int" (roundtrip-check 3))
                 (test-case "u8 int" (roundtrip-check 123))
                 (test-case "u16 int" (roundtrip-check 50000))
                 (test-case "u32 int" (roundtrip-check 800000))
                 (test-case "u64 int" (roundtrip-check 1844674407370955161))
                 (test-case "simple negative int" (roundtrip-check -3))
                 (test-case "negative u8 int" (roundtrip-check -123))
                 (test-case "negative u16 int" (roundtrip-check -50000))
                 (test-case "negative u32 int" (roundtrip-check -800000))
                 (test-case "negative u64 int" (roundtrip-check -1844674407370955161))
                 (test-case "void" (roundtrip-check (void)  void?))
                 (test-case "undefined" (roundtrip-check 'undefined))
                 (test-case "false" (roundtrip-check #f))
                 (test-case "true" (roundtrip-check #t))
                 (test-case "string" (roundtrip-check "hello world!"))
                 (test-case "list" (roundtrip-check [1 2 3 4]))
                 (test-case "hash-table" (roundtrip-check (list->hash-table [["my" . "ht"]])))
                 (test-case "vector" (roundtrip-check (vector 1 2 3)))

))
