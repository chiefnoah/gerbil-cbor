#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("gerbil-cbor/lib"
    (exe: "gerbil-cbor/main" bin: "gerbil-cbor")))
