
:- use_module( '../src/awesome/write/html' ).

view( index, User ) :-
  user @ (
    title @ 'hey there user!',
    p( 'Welcome back!' )
  ).

view( user_not_found ) :-
  not_found @ (
    h1( 'the given user was not found' ),
    p( 'Tried searching, but to no avail' )
  ).

:- view( user_not_found ).