
:- module( template, [
    op( 1200, fy, template ),
    template/1
]).

:- use_module( 'write/html' ).
:- use_module( 'awesome' ).

template( Pred ) :-
  write( '#### template ###'),
  register_view( Pred ).
