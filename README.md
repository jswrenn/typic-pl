# Typic Prolog

A Prolog model encoding Typic's:
  * `llr.pl`: low-level representation (LLR) of type layouts
  * `llr_to_llr.pl`: checking if one LLR is transmutable to another LLR.

The purpose of this model is to provide a sandbox for experimentation, as type-level programming in Rust is tedious and error-prone. Once ideas are validated here, they can be translated by hand to Rust. For instance, this:
```prolog
% A slot may also be just a bag of bytes, of some `Kind` and `Size`:
impl_Slot(struct_Byte(Kind, Size))
 :- impl_ByteKind(Kind)
 ,  impl_Unsigned(Size).
```

...translates to this:
```prolog
impl<Kind, Size> Slot for Byte<Kind, Size>
where
  Kind: ByteKind,
  Size: Unsigned,
{}
```


