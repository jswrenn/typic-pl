:- module(typenum, [impl_Unsigned/1]).
:- use_module(library(clpfd)).

impl_Unsigned(N) :- N #>= 0.