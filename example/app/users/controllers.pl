
/*
 * /users
 * 
 * List all users.
 */

get( users ) :-
  users:view( not_found ).

get( users ) :-
  users:get_all_users( Users ),
  users:view( list, Users ).

/*
 * /users/edit/<user id>
 * 
 * Used for editing a particular user. Allowed if admin,
 * or if that user.
 */

get( users, edit, ID ) :-
  number( ID ),
  ( session(id, ID) ; is_admin ),
  users:get_user( ID, User ),
  users:view( edit, User ).

get( users, edit, _ ) :-
  users:view( edit_not_allowed ).

get( users, edit ) :-
  session( id, MyID ),
  get( users, edit, MyID ).

get( users, edit ) :-
  users:view( edit_not_allowed ).

/*
 * /users/me
 * /users/id
 * 
 * Used for displaying a particular user.
 */

get( users, me ) :-
  session( id, MyID ),
  get( users, MyID ).

get( users, ID ) :-
  number( ID ),
  users:get_username( ID, Username ),
  users:view( Username ).

get( users, _ ) :-
  users:view( not_found ).
