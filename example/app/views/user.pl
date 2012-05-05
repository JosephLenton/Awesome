
:- use_module( '../awesome/template' ).

template view_index( User ) :-
  user @ (
    title @ 'hey there user!'
    p( 'Welcome back!' )
  ).

template user_not_found :-
  not_found @ (
    h1( 'the given user was not found' ),
    p( 'Tried searching, but to no avail' )
  ).
