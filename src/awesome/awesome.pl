
:- module( awesome, [
        execute/3
]).

execute( Controller, Action, Params ) :-
    call( Controller:Action, Params ).
