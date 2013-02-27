
/*
 * The HTML DSL
 */

:- module( html, [
    op( 999, xfy, '@?'  ),
    op( 999,  fy, '@?'  ),
    op( 999, xfy, '@'   ),
    op( 999,  fy, '@'   ),
    op( 999, xfy, '#'   ),
    op( 999,  fy, '#'   ),

    op( 999, xfy, css   ),
    op( 999, xfy, tag   ),

    '@?'/1,
    '@?'/2,
    '@'/1,
    '@'/2,
    '#'/1,
    '#'/2,

    css/2,
    tag/2,

    html/1,
    head/1,
    body/1,

    span/1,
     div/1,
       a/1,
       p/1,

      h1/1,
      h2/1,
      h3/1,
      h4/1,
      h5/1
]).

:- use_module( '../util' ).

/**
 * When true, this will try not to uglify the output too much.
 * No guarantees, it's just a hint in the right direction.
 */
pretty_print( true ).

/**
 * This is the same as using the 'css' operator.
 *
 * usage: class_name @ tag()
 */
@(        Contents ) :- div( Contents ).
@( Klass, Contents ) :- div( [ css => Klass | Contents ] ).

/**
 * Used for creating tags with a class name. This differs
 * from @ and css in that the name is kept and unaltered
 * when the tag is outputted.
 *
 * usage: class_name @ tag()
 */
@?(        Contents ) :- div( Contents ).
@?( Klass, Contents ) :- div( [ class => Klass | Contents ] ).

/**
 * Used for creating tags, whilst setting their id.
 *
 * usage: id_name # tag()
 */
#(     Contents ) :- div( Contents ).
#( ID, Contents ) :- div( [ id => ID | Contents ] ).

/**
 * An operator used to create generic tags.
 * 
 * For example:
 *  html tag
 *    p( 'this is the first paragraph' ),
 *    img( '/images/my_img.png' ),
 *    p( 'then another paragraph' ).
 * 
 * You can also use this to create any tag, even ones this
 * doesn't support.
 */
tag( Name, Contents ) :- parse_tag( Name, Contents ).

/**
 * Css is for creating a new HTML div with the css class
 * given.
 * 
 * This differs from 'class' in that it can be altered by
 * the system, for minification, or complete removal if
 * unused.
 */
css( Klass, Body ) :- div([ css => Klass | Body ]).

/*
 * The actual HTML elements them selves.
 */

html( Contents ) :- parse_tag( html, Contents ).
head( Contents ) :- parse_tag( head, Contents ).
body( Contents ) :- parse_tag( body, Contents ).

span( Contents ) :- parse_tag( span, Contents ).
 div( Contents ) :- parse_tag( div , Contents ).
   a( Contents ) :- parse_tag( a   , Contents ).
   p( Contents ) :- parse_tag( p   , Contents ).

  h1( Contents ) :- parse_tag( h1  , Contents ).
  h2( Contents ) :- parse_tag( h2  , Contents ).
  h3( Contents ) :- parse_tag( h3  , Contents ).
  h4( Contents ) :- parse_tag( h4  , Contents ).
  h5( Contents ) :- parse_tag( h5  , Contents ).

is_maybe_tag( html ).
is_maybe_tag( head ).
is_maybe_tag( body ).
is_maybe_tag( _ ) :- false.

is_self_closing( img   ).
is_self_closing( input ).
is_self_closing( link  ).
is_self_closing( meta  ).
is_self_closing( _ ) :- false.

/**
 * 
 */
parse_tag( Tag, Content ) :-
  is_maybe_tag( Tag ),
  output_tag_maybe( Tag, Content ).
parse_tag( Tag, Content ) :-
  is_self_closing( Tag ),
  output_closing_tag( Tag, Content ).
parse_tag( Tag, Content ) :-
  output_tag( Tag, Content ).

output_tag( Tag, All ) :-
  parse_tag_contents( All, Contents, Attributes ),
  output_tag( Tag, Contents, Attributes ).

output_tag( Tag, Contents, Attributes ) :-
  write_arr([ '<', Tag, Attributes, '>' ]),
  html_end_line,
  output_html( Contents ),
  write_arr([ '</', Tag, '>' ]).

/**
 * Same as output tag, but the outer tags are only used
 * if this has attributes. Otherwise they are skipped and
 * the contents is inlined.
 */
output_tag_maybe( Tag, All ) :-
  parse_tag_contents( All, Contents, Attributes ),
  output_tag_maybe( Tag, Contents, Attributes ).
output_tag_maybe( _Tag, Contents, '' ) :-
  output_html( Contents ).
output_tag_maybe( Tag, Contents, Attributes ) :-
  output_tag( Tag, Contents, Attributes ).

output_closing_tag( Tag, All ) :-
  parse_tag_contents( All, Contents, Attributes ),
  output_closing_tag( Tag, Contents, Attributes ).
output_closing_tag( Tag, [], Attributes ) :-
  write_arr([ '<', Tag, Attributes, ' />' ]).
output_closing_tag( _Tag, _, _Attributes ) :-
  throw( 'Content given for self closing tag' ).

/**
 * Given a list of values, this will seperate them all out,
 * into attribute values and content values.
 */
parse_tag_contents( All, Contents, Attributes ) :-
  AttributesList = [],
  parse_tag_contents_inner( All, Contents, AttributesList ),
  concat_attributes( AttributesList, Attributes ).

/**
 * Returns a list of attributes all concatted together,
 * or an empty string.
 */
concat_attributes( [], '' ).
concat_attributes( is_list(List), Attributes ) :-
  concat_atom( [' '|List], ' ', Attributes ).
concat_attributes( _, '' ).

parse_tag_contents_inner( [], _Contents, _Attributes ).
parse_tag_contents_inner( [A |All], Contents, Attributes ) :-
  parse_tag_contents_inner(   A, Contents, Attributes ),
  parse_tag_contents_inner( All, Contents, Attributes ).
parse_tag_contents_inner( (A, All), Contents, Attributes ) :-
  parse_tag_contents_inner(   A, Contents, Attributes ),
  parse_tag_contents_inner( All, Contents, Attributes ).
parse_tag_contents_inner( ( css => Val), _Contents, [ class, '="', Val, '"' | _Attributes ] ).
parse_tag_contents_inner( (Name => Val), _Contents, [ Name, '="', Val, '"' | _Attributes ] ).
parse_tag_contents_inner( Content, [Content|_Contents], _Attributes ).

write_arr([]).
write_arr( Strs ) :-
  is_list( Strs ),
  concat_atom( Strs, '', Str ),
  write(Str).
write_arr(Str) :-
  write(Str).

output_html( [] ).
output_html( [Content|Contents] ) :-
  output_html(Content),
  output_html(Contents).
output_html( (_ :- Body) ) :-
  call( Body ),
  html_end_line.
output_html( (A, B) ) :-
  output_html( A ),
  output_html( B ).
output_html( Content ) :-
  write( Content ),
  html_end_line.

html_end_line :-
  pretty_print( true ),
  nl.
html_end_line.
