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
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x19 #x75 #x30)))
                                   => 30000))
                 (test-case "decode u32"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x1a #x0 #xc #x35 #x0)))
                                   => 800000))
                 (test-case "decode u64"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x1b #xff #xff #xff #xff #xff #xff #xff #xff)))
                                   => 18446744073709551615))
                 (test-case "decode negative simple"
                            (check (default-decoder (open-buffered-reader #u8(#x21))) => -2))




                 ))
