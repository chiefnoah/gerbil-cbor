;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/getopt
        ./lib)
(export main)

;; build manifest; generated during the build
;; defines version-manifest which you can use for exact versioning
(include "../manifest.ss")

(def (main . args)
  (call-with-getopt gerbil-cbor-main args
    program: "gerbil-cbor"
    help: "A one line description of your program"
    ;; commands/options/flags for your program; see :std/getopt
    ;; ...
    ))

(def* gerbil-cbor-main
  ((opt)
   (gerbil-cbor-main/options opt))
  ((cmd opt)
   (gerbil-cbor-main/command cmd opt)))

;;; Implement this if your CLI doesn't have commands
(def (gerbil-cbor-main/options opt)
  (error "Implement me!"))

;;; Implement this if your CLI has commands
(def (gerbil-cbor-main/command cmd opt)
  (error "Implement me!"))
