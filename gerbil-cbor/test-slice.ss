(import
  "gerbil-cbor/slice"
  :std/test)
(export #t)

(def slice-test
  (test-suite "test :ngp/slice"
    (test-case "basic slice-of set!"
      (let* ((v (make-u8vector 10 4))
             (s (slice-of v 1 end: 5)))
        (u8slice-set s 0 10)
        (check (u8vector-get v 1) => 10)
        (check (u8vector-lenth v) => 4)))))

