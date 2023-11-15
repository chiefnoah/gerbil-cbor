(import :std/test
        :std/io
        :std/text/utf8
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
                 (test-case "decode negative u8"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x38 #x1f))) => -32))
                 (test-case "decode negative u16"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x39 #x75 #x2f))) => -30000))
                 (test-case "decode negative u32"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x3a #x00 #x0c #x34 #xff))) => -800000))
                 (test-case "decode negative u64"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x3b #x63 #x5 #x1f #xf8 #x62 #x1c #x5e #x64))) => -7135144336296795749))
                 (test-case "decode simple list"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x83 #x1 #x2 #x3))) => [1 2 3]))
                 (test-case "decode recursive list"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x83 #x1 #x82 #x2 #x3 #x4)))
                                   => [1 [2 3] 4]))
                 (test-case "decode text"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x6c #x48 #x65 #x6c #x6c
                                                               #x6f #x20 #x77 #x6f #x72 #x6c #x64 #x21)))
                                   => "Hello world!"))
                 (test-case "decode bytes"
                            (check (default-decoder
                                     (open-buffered-reader #u8(#x43 #x30 #x31 #x32)))
                                   => #u8(#x30 #x31 #x32)))))
