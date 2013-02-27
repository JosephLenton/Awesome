
/**
 * Server
 * ======
 * 
 */

:- module( server, [
    server_start/1,
    server_start/2,
    server_start_dev/1,
    server_start_dev/2,

    server_restart/1,

    server_stop/1,

    server_restart/1,

    server_port_number/2,
    server_is_running/2
]).

:- use_module( response ).

:- use_module( library(apply) ).

:- use_module( library(http/thread_httpd) ).
:- use_module( library(http/http_dispatch) ).

:- http_handler(/, server:request_event_handler, [prefix]).

:- dynamic server_debug/1.

server_debug( false ).

server_start_dev(       S ) :- server_start_inner( 80, dev, S ).
server_start_dev( Port, S ) :- server_start_inner( Port, dev, S ).

server_start(       S ) :- server_start_inner( 80, false, S ).
server_start( Port, S ) :- server_start_inner( Port, false, S ).

server_start_inner( Port, dev  , S ) :-
    retractall( server_debug ),
    assert( server_debug(true) ),
    server_start_inner( Port, false, S ).

server_start_inner( Port, false, server(Port) ) :-
    http_server( http_dispatch, [port(Port)] ).

server_restart( Server = server(Port) ) :-
    server_stop( Server ),
    server_start( Port, _ ).

server_stop( Server ) :-
    server_port_number( Server, Port ),
    server_is_running( Server, true ),
    http_stop_server( Port, [] ).

server_port_number( server(Port), Port ).
server_port_number( _, false ).

server_is_running( server(_Port), true ).
server_is_running( _, false ).

/**
 * The actual event fired by the HTTP server.
 */
request_event_handler( Req ) :-
    catch(
            request_event_handler_inner( Req ),
            E,
            server:handle_error( E )
    ).

request_event_handler_inner( _Req ) :-
    ( server_debug( true ) -> make ; true ),
    handle_request( Req, Res ),
    response:format( Res ).
    
handle_request( Req, Res2 ) :-
    response:new( Res ),
    default_page( Req, Res, Res2 ).

/**
 * Outputs the default page,
 * when this is used,
 * with no other handlers.
 */
default_page( _, Res, ResOut ) :-
    standard_page(
            'AWESOME IS RUNNING',
            '// todo, get started page here',
            Res,
            ResOut
    ).

/**
 * Used for creating the standard pages,
 * displayed by Awesome, when it is not
 * customized.
 */
standard_page( H1, Res, ResOut ) :-
    standard_page( H1, Res, ResOut ).

standard_page( H1, H2, Res, ResOut ) :-
    response:html([
            '<style>',
                'div.awesome_default {',
                        'padding: 128px 0 0 0 !important;',
                        'margin: 0 !important;',
                        'position: fixed !important;',
                        'top: 0;',
                        'left: 0;',
                        'right: 0;',
                        'bottom: 0;',
                        'z-index: 1000000 !important;',
                        'background: #fff !important;',
                        'text-align: center;',
                '}',
                'h1.awesome_default,',
                'h2.awesome_default,',
                'span.awesome_default {',
                        'padding: 0 !important;',
                        'margin: 0 !important;',
                        'color: #333 !important;',
                        'font-family: "Segoe UI", "Segoe WP", "Helvetica Neue", Roboto, sans-serif !important;',
                        'font-weight: 100 !important;',
                '}',
                'h1.awesome_default {',
                    'font-size: 32px !important;',
                '}',
                'h2.awesome_default {',
                    'font-size: 28px !important;',
                    'color: #aaa !important;',
                '}',
                'span.awesome_default {',
                    'font-size: 128px !important;',
                    'vertical-align: text-top;',
                '}',
            '</style>',
            '<div class="awesome_default">',
                '<h1 class="awesome_default">', H1, '</h1>',
                '<h2 class="awesome_default">', H2, '</h2>',
            '</div>'
    ], Res, ResOut ).

test_page( HTML ) :-
    format( 'Content-type: text/html~n~n' ),
    format( HTML ).

status_403( _ ) --> status( '403', 'content was just not here' ).
status_404( _ ) --> status( '404', 'content was not found' ).

status( Code       ) --> status( Code, '' ).
status( Code, Info ) --> standard_page(  Code, Info ).

/*
 * Error Handling
 * --------------
 */

/**
 * Outputs a full page, displaying the error in question.
 */
handle_error( E ) :-
    format( 'Content-type: text/html~n~n' ),
    maplist( format, [
        '<style>',
            'div.awesome_err { overflow: auto; padding: 0 !important; margin: 0 !important; position: fixed !important; top: 0 !important; left: 0 !important; right: 0 !important; bottom: 0 !important; margin: 0 !important; padding: 0 !important; z-index: 1000000 !important; background: #1ba1e2 !important; }',
            'h1.awesome_err { padding: 0 32px; color: white !important; font-size: 56px !important; font-family: "Segoe UI", "Segoe WP", "Helvetica Neue", Roboto, sans-serif !important; font-weight: 100 !important; }',
            'pre.awesome_err { color: white !important; padding: 2px 64px !important; font-size: 18px !important; font-family: "Droid Sans Mono", "DejaVu Sans Mono", consolas !important; }',
        '</style>',

        '<div class="awesome_err">',
            '<h1 class="awesome_err">oh noes! </h1>',
            '<pre class="awesome_err">'
    ], _ ),
    
    format_err( E ),

    format( '</pre>' ),
    format( '</div>' ).

format_err( E ) :-
    compound( E ),
    format_term( E, 0 ).
format_err( E ) :-
    print( E ).

format_term( E, N ) :-
    compound( E ),
    E =.. Rest,
    [ Head | Tail ] = Rest,

    format_term_inner( Head, Tail, N ).

format_term( E, N ) :-
    format_tabs( N ),
    print( E ).

format_term_inner( ':', [Module, Fun], N ) :-
    format_tabs( N ),
    print( Module ),
    format( ':' ),
    print( Fun ).
    
format_term_inner( Head, Tail, N ) :-
    N2 is N+1,

    format_tabs( N ),
    print( Head ),
    format( '(~n' ),
    format_term_arr( first, Tail, N2 ),
    format( '~n' ),
    format_tabs( N ),
    format( ')' ).

format_term_arr( _, [], _ ).
format_term_arr( first, [ P | Ps ], N ) :-
    format_term( P, N ),
    format_term_arr( not_first, Ps, N ).

format_term_arr( not_first, [ P | Ps ], N ) :-
    format( ',~n' ),
    format_term( P, N ),
    format_term_arr( not_first, Ps, N ).

format_tabs( 0 ).
format_tabs( N ) :-
    format( '    ' ),
    N2 is N-1,
    format_tabs( N2 ).

/**
 * The server that actually runs the request.
 */
execute_url( Type, Resource ) :-
  parse_url_query( Resource, ResourceParts, QueryParts ),
  execute( Type, ResourceParts, QueryParts ).

/**
 * The same as execute_url, only this expects
 * Params to be the resource parts, whilst
 * QueryParams is the query parts also in an array.
 */
execute( Type, Params, QueryParams ) :-
  retractall( request ),
  set_requests( QueryParams ),
  !,
  call_controller( Type, Destination ).

call_controller( Type, Destination ) :-
  validate_type( Type ),
  apply( Type, Destination ).
call_controller( _, _ ).

call_controller( R, _, _ ) :-
  status_404( R ).

validate_type( get  ).
validate_type( post ).
validate_type( _    ) :- fail.

parse_url_query( Resource, Rs, Qs ) :-
  concat_atom( Parts, '?', Resource ),
write( '## pre inner' ), nl,
  parse_url_query_inner( Parts, Rs, Qs ).
parse_url_query_inner( [RIn, QIn], ROut, QOut ) :-
  parse_url( RIn, ROut ),
  parse_query( QIn, QOut ).
parse_url_query_inner( [RIn], ROut, [] ) :-
  parse_url( RIn, ROut ).

parse_url( Resource, RParts ) :-
  split_without_empties( Resource, '/', RParts ).

parse_query( '', [] ).
parse_query( QStr, Qs ) :-
  split_without_empties( QStr, '&', Qs ).

/**
 * Parses query parameters, and sets them as the current
 * request values.
 */
set_requests( [] ).
set_requests( [ Part | Params ] ) :-
  concat_atom( Parts, '=', Part ),
  set_request( Parts ),
  set_requests( Params ).

/**
 * For: foo=blah
 */
set_request( [V, E] ) :-
  asserta( user:request(V, E) ).

/**
 * For standalone parts, like: foo
 */
set_request( [V] ) :-
  asserta( user:request(V, true) ).

split_without_empties( Str, Seperator, Result ) :-
  concat_atom( ResultWithEmpties, Seperator, Str ),
  delete( ResultWithEmpties, '', Result ).

