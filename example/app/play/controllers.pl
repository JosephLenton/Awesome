
is_viewable( Project ) :- play:is_public( Project ).
is_viewable( Project ) :-
    session( id, MyID ),
    play:project_user_id( Project, MyID ).
is_viewable( _ ) :- is_admin.
is_viewable( _ ) :- fail.

get( play, game, Username, ProjectName ) :-
    play:get_project_by_user( Username, ProjectName, Project ),

    projects:get( Project, [ title(Title) ]),

    title( Title ),

    (
            is_viewable( Project ), !,

            open_graph_tags:game( Project ),
            
            play:view( game, Project ),

            build:view( modal_copy_project ),
            build:view( modal_report_game ),

            (
                    ( is_mobile ; is_ipad ),
                    set_js( 'fullscreenUrl', EmbedUrl )
                ;
                    true
            ),
            build:view( end_js )
        ;
            !, play:view( game_private, Project ).
    ).

get( play, game, User, Project ) :- play:view( project_not_found, Project ).
get( play, game, User ) :- get( play, user, User ).

get( play, index ) :- play:view( index ).
get( play, game  ) :- play:view( index ).
get( play        ) :- play:view( index ).