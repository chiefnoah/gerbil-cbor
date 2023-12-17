# CBOR Encoder

The `encoder` for CBOR handle simple Scheme objects such as vectors, lists,
numeric types, booleans, strings, and bytes as you would expect. Custom types such as
structs, records, and objects must make use of the `current-hook` parameter to
override the default behavior, otherwise an error will be raised.

## `encoder`

The `encoder` is the primary entry point to encoding a message using CBOR. It
recursively walks any aggregate types and encodes them. Any types that it does not know
how to encode by default, it attempts to use the `current-hook` function to encode.

## `current-hook`

```scheme
(hook writer item) => #!void
```

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

