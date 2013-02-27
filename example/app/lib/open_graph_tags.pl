
:- module( open_graph_tags, [
    game:1,
    display/1
]).

:- use_module( '../src/awesome/write/html' ).

game( Project ) :-
    projects:get( Project, [
            title       => Title,
            description => Description,
            url         => Url,
            avatar      => Avatar
    ]),
    open_graph_tags:display([
            title       => Title,
            description => Description,
            url         => Url,
            image       => Avatar
    ]).

display([]).
display([ A => Content|Tags ]) :-
    atom_concat(['og:', A], '', Prop),
    meta(
            property => Prop,
            content  => Content
    ),
    open_graph_tags:display( Tags ).