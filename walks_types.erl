-module(walks_types).
-export([plan_route/2]).
-vsn("0.1").
-author({dedaldino,antonio}).


% Specifying spec annotations
% spec annotations are used for functions definition
-spec plan_route(From::position(),To::position()) -> route().

% spec with types defined inside it
% -spec myfile:open_file(FileName, Modes) -> {ok, Handle} | {error, Why} when
%     FileName:: string(),
%     Modes :: [Mode],
%     Mode :: read | write,
%     Handle :: file_handle,
%     Why :: error_term().


% Specifying type annotations
% type annotations are used for arguments/parameters
-type angle() :: {Degrees::0..360, Minutes::0..60, Seconds::0..60}.
-type position() :: {latitude | longitude, angle()}.
% -type point() :: {integer(),integer()}.
-type direction() :: north | south | east | west.
-type route() :: [{go, direction(),integer()}].



plan_route(_A,_B) ->
    if
        erlang:is_tuple(_A), erlang:is_tuple(_B) ->
            {_L, _Angle} = _A,
            {Degrees,_Minutes,_Seconds} = _Angle,
            {_L2, _Angle2} = _B,
            if
                Degrees < 180 ->
                    _Dir = north,
                    [{go, _Dir,_Seconds}];
                Degrees > 180 ->
                    _Dir = south,
                    [{go, _Dir,_Seconds}];
                Degrees =:= 360 ->
                    _Dir = east,
                    [{go, _Dir,_Seconds}];
                true ->
                    [{go, _Minutes,_Seconds}]
            end,
            case Degrees =< 50 and (_Minutes =:= 59) of
                true ->
                    Dict = #{
                        degrees => Degrees,
                        minutes => _Minutes,
                        seconds => _Seconds
                    },
                    Dict;
                    % % create a block
                    % begin
                    % end; % end of block
                false ->
                    "Degrees crashed"
            end;
        is_list(_A), is_list(_B) ->
            X = [X || X <- _A],
            case X of
                1 -> worked;
                0 -> plan_route()
            end;
        true ->
            plan_route()
    end.


plan_route() -> "Dedaldino Mambuenze António".
