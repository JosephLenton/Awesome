
/**
 * Awesome Prolog Web Framework
 * 
 * This is the main entry point for using this.
 * Currently it doesn't include anything, apart
 * from stubs for calling into other bits, for
 * testing.
 */

:- module( awesome, [
    start/0,
    start/1,

    start_dev/0,
    start_dev/1,

    restart/1
]).

:- use_module( util ).
:- use_module( server, [
    server_start/1,
    server_start/2,

    server_start_dev/1,
    server_start_dev/2,

    server_restart/1
]).

awesome:restart( S ) :-
    server_restart( S ).

awesome:start_dev :-
    server_start_dev( _ ).
awesome:start_dev( P ) :-
    server_start_dev( P, _ ).

awesome:start :-
    server_start( _ ).
awesome:start( P ) :-
    server_start( P, _ ).

