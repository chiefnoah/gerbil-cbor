# CBOR Encoder

The `encoder` for CBOR handles simple Scheme objects such as vectors, lists, numeric
types, booleans, strings, and bytes as you would expect. Custom types such as structs,
records, and objects must make use of the `current-hook` parameter to override the
default behavior, otherwise an error will be raised.

## `encoder`

The `encoder` is the primary entry point to encoding a message using CBOR. It
recursively walks any aggregate types and encodes them. Any types that it does not know
how to encode by default, it attempts to use the `current-hook` function to encode.

## `current-hook`

```scheme (hook writer item) => #!void ```

The `current-hook` is a parameter function that accepts a `BufferedWriter` and an
arbitrary item representing an item that the `encoder` does not know how to encode. The
responsibility of `current-hook` is to attempt to encode `item` and write the resulting
CBOR formatted bytes to `writer`. If the hook implementation does not know how to encode
`item`, it should raise a `error`. The default tag implementation can encode `date?`
types using well-known CBOR tag 0.

It's return value is ignored.

### Tips for encoding your custom types

The easiest way to encode a custom type is to use `make-cbor-tag` and pass in a type
that is understood by the default `encoder`. Alternatively, encode your type as a raw
`u8vector` and pass that directly to the `encoder`. Keep in mind: any custom type that
you encode won't magically be decoded on the other end; you *must* handle any
`cbor-tag`s you encode when decoding, such as with a `tag-handler`.

Here's an example of a simple encoder callback:

```scheme

(import
  :std/contract
  :std/io
  :std/error
  "gerbil-cbor/lib")

(defstruct point (x y) final: #t)
; As of this writing, this is an unassigned tag in the
; [IANA CBOR Tag Registry](https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml)
(def POINT 60003)
(def tag (make-cbor-tag POINT 123))

(def mypoint (make-point 123 45.6))
(def writer (open-buffered-writer #f))
(def (tag-handler writer item)
  ; the writer is *always* a `BufferedWriter`, otherwise the `encoder` application below
  ; will fail before we get to this point.
  (using (writer :- BufferedWriter)
    (match item
      ((? point?)
       (using ((item :- point)
               (tag (make-cbor-tag POINT [item.x item.y]) : cbor-tag))
         ; encode point as a list of it's x, y values
         (encoder writer tag)))
      (else
        (error "Don't know how to encode item" item)))))
; use the current-hook parameter to encode our `point`.
(parameterize ((current-hook tag-hook))
 (encoder writer mypoint))

```
