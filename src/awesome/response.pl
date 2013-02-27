/**
 * Response
 * ========
 * 
 */

:- module( response, [
        new/1,

        html/2,
        html/3,

        header/2,
        header/3,

        format/1
]).

new(
        response(
                [ 'Content-Type: text/html; charset=UTF-8' ],
                []
        )
).

html( response(_, HTML), HTML ).
html( HTML, response(Header, _), response(Header, HTML) ).

header( response(Header, _), Header ).
header( Header, response(_, HTML), response(Header, HTML) ).

% todo
mime. 

format( response(Header, HTML) ) :-
    format_header( Header ),
    user:format( '~n~n' ),
    format_html( HTML ).

format_header( [] ).
format_header( Header ) :-
    foldl( format_foldl, Header, false, _ ),
    user:format( '~n~n' ).

format_html( [] ).
format_html( HTML ) :-
    foldl( format_foldl, HTML, false, _ ).

format_foldl( V, false, true ) :- user:format( V ).
format_foldl( V,  true,    _ ) :- user:format( '~n' ), user:format( V ).

