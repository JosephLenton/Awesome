
:- module( loading, [
        locate/2,
        load/2,
        load_controller/1
]).

:- dynamic loading:locations/2, loading:locate/2, load_controller/1, load/2.

locations( _, _ ).

locate( is_atom(LogicalName), is_atom(Location) ) :-
    assert( locations(LogicalName, Location) ).
locate( LogicalName, Location ) :-
    locations( LogicalName, Location ).

load( _, []).
load( Location, [ Controller | Controllers ]) :-
    load( Location, Controller ),
    load( Location, Controllers ).
load( Location, Item ) :-
    downcase_atom( Item, File ),
    atom_concat([ './../', Location, '/', File, '.pl' ], '', Path ),
    load_files( Path, if(changed) ).

load_controller( Controller ) :-
    locate( controller, Location ),
    load( Location, Controller ).
