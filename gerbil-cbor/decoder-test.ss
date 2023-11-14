(import :std/test
        :std/io
        "decoder")
(export cbor-decoder-test)


(def cbor-decoder-test
     (test-suite "cbor/decoder"
                 (test-case "decode simple int"
                            (def testsmallint (open-buffered-reader #u8(16)))
                            (check (default-decoder testsmallint) => 16))
                 (test-case "decode u8"
                            (check (default-decoder (open-buffered-reader #u8(24 32))) => 32))
                 (test-case "decode u16"
                            (check (default-decoder (open-buffered-reader #u8(#x19 #x75 #x30))) => 30000))))
