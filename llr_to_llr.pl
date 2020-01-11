% This module defines a `LayoutToLayout` predicate that is satisfied if the LLR
% of a source type is soundly transmutable to the LLR of the destination type.

:- module(llr_to_llr, [impl_LayoutToLayout/2]).
:- use_module(llr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% [Slot|_] × [Slot|_]
% In the basic case, the first elements of both the source and destination
% layouts are Slots. We check whether these elements are compatible, and then
% check whether the remaining items in the layouts are compatible.
impl_LayoutToLayout(
  struct_PCons(T, TPR),
  struct_PCons(U, UPR)
) :-  impl_Slot(T)
  ,   impl_Slot(U)
  ,   impl_Product(TPR)
  ,   impl_Product(UPR)
  %
  %   U must be compatible with T
  ,   impl_Sub(U, T, U_Minus_T)
  %   Drain `U` bytes from `T`.
  ,   impl_Size(U, USize)
  ,   impl_Sub(T, USize, T_Minus_USize)
  %   Add the (possibly) consumed `T` and `U` remainders back on their lists.
  ,   impl_Add(T_Minus_USize, TPR, T_Minus_USize_Plus_TPR)
  ,   impl_Add(U_Minus_T, UPR, U_Minus_T_Plus_UPR)
  %   Check the the remaining items are compatible.
  ,   impl_LayoutToLayout(T_Minus_USize_Plus_TPR, U_Minus_T_Plus_UPR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% [Slot|_] × [Coproduct|_]

% [Slot|_] × [COne|_]
% When faced with a slot in the source layout and a unit coproduct in the
% destination layout, we check whether the source type is compatible with a
% layout consisting of that variant's layout followed the remaining items in
% the destination layout.
impl_LayoutToLayout(
  struct_PCons(T, TPR),
  struct_PCons(struct_COne(UP), UPR)
) :-  Self = struct_PCons(T, TPR)
  ,   impl_Slot(T)
  ,   impl_Product(TPR)
  ,   impl_Product(UP)
  ,   impl_Coproduct(UPR)
  %
  ,   impl_Add(UP, UPR, UP_Plus_UPR)
  ,   impl_LayoutToLayout(Self, UP_Plus_UPR).

% [Slot|_] × [CCons|_]
% If the destination layout has more than one variant, we explore both
% possibilities. First, we choose the first variant of the destination type,
% and see if the source layout is compatible with that choice. If it isn't, we
% check to see if the source layout is compatible with some other variant.
%
% Note the disjunction. When translated to rust, we realize this code as _two_
% implementations. We annotate this trait with `#[marker]` so that Rust allows
% these impls to overlap.
impl_LayoutToLayout(
  struct_PCons(T, TPR),
  struct_PCons(struct_CCons(UP, UCR), UPR)
) :-  Self = struct_PCons(T, TPR)
  ,   impl_Slot(T)
  ,   impl_Product(TPR)
  ,   impl_Product(UP)
  ,   impl_Coproduct(UCR)
  ,   impl_Product(UPR)
  ,   impl_Add(UP, UPR, UP_Plus_UPR)
  %   match the first variant of RHS
  ,   impl_LayoutToLayout(Self, UP_Plus_UPR)
  %   OR match some other variant
  ;   impl_LayoutToLayout(Self, struct_PCons(UCR, UPR)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% [Coproduct|_] × [Slot|_]
% When faced with a coproduct in the source layout and a slot in the destination
% layout, we must ensure that _every_ variant of the source layout is compatible
% with the destination layout.

% [COne|_] × [Slot|_]
% If there is only one variant, we choose it and see if that choice is
% compatible with the destination layout:
impl_LayoutToLayout(
  struct_PCons(struct_COne(TP), TPR),
  struct_PCons(U, UPR)
) :-  impl_Product(TP)
  ,   impl_Product(TPR)
  ,   impl_Slot(U)
  ,   impl_Product(UPR)
  %
  ,   impl_Add(TP, TPR, TP_Plus_TPR)
  ,   impl_LayoutToLayout(TP_Plus_TPR, struct_PCons(U, UPR)).

% [CCons|_] × [Slot|_]
% If there is more than one variant, we assert that they are all compatible:
impl_LayoutToLayout(
  struct_PCons(struct_CCons(TP, TCR), TPR),
  struct_PCons(U, UPR)
) :-  impl_Product(TP)
  ,   impl_Coproduct(TCR)
  ,   impl_Product(TPR)
  ,   impl_Slot(U)
  ,   impl_Product(UPR)
  %
  ,   impl_Add(TP, TPR, TP_Plus_TPR)
  %   the first variant must be compatible with lhs
  ,   impl_LayoutToLayout(TP_Plus_TPR, struct_PCons(U, UPR))
  %   AND all successive variants must be compatible with the RHS
  ,   impl_LayoutToLayout(struct_PCons(TCR, TPR), struct_PCons(U, UPR)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% [Coproduct|_] × [Coproduct|_]
% Lastly, we consider the case in which we are faced with a coproduct in both
% the source layout and the destination layout. All variants of the source
% layout must have some compatible variant in the destination layout.

% [COne|_] × [COne|_]
% If there is just one variant in each, merely choose those variants and check
% their compatibility:
impl_LayoutToLayout(
  struct_PCons(struct_COne(TP), TPR),
  struct_PCons(struct_COne(UP), UPR)
) :-  impl_Product(TP)
  ,   impl_Product(TPR)
  ,   impl_Product(UP)
  ,   impl_Product(UPR)
  %
  ,   impl_Add(TP, TPR, TP_Plus_TPR)
  ,   impl_Add(UP, UPR, UP_Plus_UPR)
  ,   impl_LayoutToLayout(TP_Plus_TPR, UP_Plus_UPR).

% [COne|_] × [CCons|_]
% If the destination layout has more than one variant, we check that the lone
% variant of the source layout is compatible with at least one of them. Again,
% note the disjunction.
impl_LayoutToLayout(
  struct_PCons(struct_COne(TP), TPR),
  struct_PCons(struct_CCons(UP, UCR), UPR)
) :-  impl_Product(TP)
  ,   impl_Product(TPR)
  ,   impl_Product(UP)
  ,   impl_Coproduct(UCR)
  ,   impl_Product(UPR)
  %
  %   The lone variant of the LHS is compatible with the first variant of the RHS.
  ,   impl_Add(TP, TPR, TP_Plus_TPR)
  ,   impl_Add(UP, UPR, UP_Plus_UPR)
  ,   impl_LayoutToLayout(TP_Plus_TPR, UP_Plus_UPR)
  %   OR, it's compatible with some other variant of the LHS.
  ;   impl_LayoutToLayout(TP_Plus_TPR, struct_PCons(UCR, UPR)).

% [CCons|_] × [COne|_]
% All variants of LHS must be compatible with the lone variant of LHS.
impl_LayoutToLayout(
  struct_PCons(struct_CCons(TP, TCR), TPR),
  struct_PCons(struct_COne(UP), UPR)
) :-  impl_Product(TP)
  ,   impl_Coproduct(TCR)
  ,   impl_Product(TPR)
  ,   impl_Product(UP)
  ,   impl_Product(UPR)
  %
  ,   impl_Add(TP, TPR, TP_Plus_TPR)
  ,   impl_Add(UP, UPR, UP_Plus_UPR)
  ,   impl_LayoutToLayout(TP_Plus_TPR, UP_Plus_UPR)
  ,   impl_LayoutToLayout(struct_PCons(TCR, TPR), UP_Plus_UPR).

% [CCons|_] × [CCons|_]
% Each variant of the LHS must be compatible with some variant of the RHS.
impl_LayoutToLayout(
  struct_PCons(struct_CCons(TP, TCR), TPR),
  struct_PCons(struct_CCons(UP, UCR), UPR)
) :-  impl_Product(TP)
  ,   impl_Coproduct(TCR)
  ,   impl_Product(TPR)
  ,   impl_Product(UP)
  ,   impl_Coproduct(UCR)
  ,   impl_Product(UPR)
  %
  %   The first variant has a match in LHS.
  ,   impl_LayoutToLayout(
        struct_PCons(struct_COne(TP), TPR),
        struct_PCons(struct_CCons(UP, UCR), UPR))
  %   AND subsequent variants have matches in LHS.
  ,   impl_LayoutToLayout(
        struct_PCons(TCR, TPR),
        struct_PCons(struct_CCons(UP, UCR), UPR)).