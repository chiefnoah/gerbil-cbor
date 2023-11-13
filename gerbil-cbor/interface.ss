(import :std/interface
        :std/io)
(export #t)

(defstruct CBORTag)

(interface CBOREncoder
  (write (out :- BufferedWriter)
         (value)))

(interface CBORDecoder
  (read (src :- BufferedReader)
        (tag :~ fixnum? := #f)))


