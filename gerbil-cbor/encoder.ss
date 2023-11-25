(import
  :std/contract
  :std/io)
(export default-encoder)


(def (default-encoder buf)
     (using (buf : BufferedWriter)
            '()))
