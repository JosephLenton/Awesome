
:- module( controller_user, [
    index/0,
    index/1
] ).

index( ID ) :-
  number( ID ),
  view( user, User ).

index( _ ) :-
  view( user_not_found ).

index :-
  session( id, MyID ),
  index( MyID ).

index :-
  view( user_not_found ).
