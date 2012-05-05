
:- module( awesome, [
        execute/3,

        view/1,
        view/2,
        view/3,
        view/4,

        register_view/1
]).

:- dynamic awesome_predicates/4.

execute( Controller, Action, Params ) :-
    call( Controller:Action, Params ).

view( View ) :- run_view( View, [] ).
view( View, A ) :- run_view( View, [ A ] ).
view( View, A, B ) :- run_view( View, [ A, B ] ).
view( View, A, B, C ) :- run_view( View, [ A, B, C ] ).

run_view( View, Args ) :-
    length( Args, Arity ),
    awesome_predicates( view, Arity, View, Pred ),
    call( Pred, Args ).

register_view( Pred ) :-
    register_predicate( view, Pred ).

/**
 * 
 */
register_predicate( Where, Pred ) :-
    functor( Pred, Name, Arity ),
    assertz( awesome_predicates(Where, Arity, Name, Pred) ).