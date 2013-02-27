
:- module( users, [
    view/1
]).

:- use_module( '../src/awesome/write/html' ).

view( user_not_found ) :-
  not_found @ (
    h1( 'the given user was not found' ),
    p( 'Tried searching, but to no avail' )
  ).

view( User ) :-
  user @ (
    title @ 'hey there user!',
    p( 'Welcome back!' )
  ).
