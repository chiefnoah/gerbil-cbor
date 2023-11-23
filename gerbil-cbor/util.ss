(export #t)

(def (data-item major-type arg)
       (let* ((out (fx+ (fxarithmetic-shift-left major-type 5) arg))
              (bitlength (##fxlength out)))
         (if (fx> bitlength 8)
           (error "major type or arg too large. Bit count: " bitlength)
           out)))
