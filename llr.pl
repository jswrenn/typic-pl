% This module defines the low-level representation (LLR) of type layouts, and
% operations for manipulating the LLR.

:- module(llr,
    [ impl_Product/1
    , impl_Coproduct/1
    , impl_Slot/1
    , impl_Size/2
    , impl_Add/3
    , impl_Sub/3
    ]).

:- use_module(typenum).
:- use_module(library(clpfd)).

% PRODUCTS
% At the root, we have Products, representing a sequence of bytes.

% A product may be empty.
impl_Product(struct_PNil).

% Store a Slot:
impl_Product(struct_PCons(H, T))
 :- impl_Slot(H)
 ,  impl_Product(T).

% Or store a Coproduct:
impl_Product(struct_PCons(H, T))
 :- impl_Coproduct(H)
 ,  impl_Product(T).


% COPRODUCTS
% A coproduct represents a choice between alternative layouts.

% It may be a choice from a set of just one possibility:
impl_Coproduct(struct_COne(H))
 :- impl_Product(H).

% Or more than one possibility:
impl_Coproduct(struct_CCons(H, T))
 :- impl_Product(H)
 ,  impl_Coproduct(T).


% SLOTS
% The leaves of the LLR consist are Slots. Each slot represents a sequence of
% bytes carying length and validity restrictions.

% A slot may be a shared reference of some lifetime to a type:
impl_Slot(struct_SharedRef(_L, _T)).

% A slot may be a unique reference of some lifetime to a type:
impl_Slot(struct_UniqueRef(_L, _T)).

% A slot may also be just a bag of bytes, of some `Kind` and `Size`:
impl_Slot(struct_Byte(Kind, Size))
 :- impl_Kind(Kind)
 ,  impl_Unsigned(Size).

% The byte must be non-zero:
impl_Kind(struct_NonZero).

% The byte must be initialized:
impl_Kind(struct_Initialized).

% The byte may be anything, including uninitialized:
impl_Kind(struct_Uninitialized).

% For convenience, we define a `Size` operator that produces the slot's size,
% in bytes:
impl_Size(struct_SharedRef(_L, _T), 8).
impl_Size(struct_UniqueRef(_L, _T), 8).
impl_Size(struct_Byte(_Kind, Size), Size).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Appending a Slot to a Product

% For later convenience, we define a type-level append operation for Product.
% If the slot size of `A` is zero, `B = C`.

% These are probably going to need some extra effort to port to rust to avoid
% overlapping impls.

% Slot size is zero; produce LHS.
impl_Add(LHS, RHS, Output)
 :- impl_Product(LHS)
  , impl_Slot(RHS)
  , impl_Size(RHS, RHS_Size)
  , RHS_Size = 0
  , Output = LHS.

% Slot size is greater than zero; append Slot to LHS.
impl_Add(LHS, RHS, Output)
 :- impl_Product(LHS)
  , impl_Slot(RHS)
  , impl_Size(RHS, RHS_Size)
  , RHS_Size #> 0
  , Output = struct_PCons(RHS, LHS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subtracting a Unsigned from a Slot

% `Slot - Unsigned` produces a new slot of the same type, with `Unsigned` bytes
% subtracted from its size. If the RHS is greater than the size of the LHS, the
% subtraction saturates to `0`.

% TODO: figure out what to do with reference slots.

impl_Sub(struct_Byte(Kind, LHS_Size), RHS, Output)
 :- impl_Slot(RHS)
  , impl_Size(RHS, RHS_Size)
  , impl_SaturatingSub(LHS_Size, RHS_Size, Remainder)
  , Output = struct_Byte(Kind, Remainder).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Subtracking a Slot from a Slot
% `Slot - Slot` denotes that the size of the LHS should be decreased by
% `size(RHS)`, _only_ if the kind of `LHS` is compatible to the kind of `RHS`.

% TODO
