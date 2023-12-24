(import
  "util"
  "encoder"
  "decoder")

(export
  ; encoder
  encoder current-hook object->cbor
  ; decoder
  decoder current-tag-handler max-indefinite-item cbor->object
  ; util
  (struct-out cbor-tag))
