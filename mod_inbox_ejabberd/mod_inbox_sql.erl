%%%-------------------------------------------------------------------
%%% @author dedaldino3D
%%% @copyright (C) 2020 dedaldino3D
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 16:59
%%%-------------------------------------------------------------------
-module(mod_inbox_sql).
-author("dedaldino3D").
-include_lib("xmpp/include/xmpp.hrl").
-include("logger.hrl").
-include("ejabber_sql_pt.hrl").
-include("mod_inbox.hrl").


-behaviour(mod_inbox).


% API 
-export([
    get_inbox/3,
    init/2,
    set_inbox/7,
    set_inbox_incr_unread/6,
    reset_unread/4,
    remove_inbox/3,
    clear_inbox/1,
    clear_inbox/2,
    get_inbox_unread/3
    ]).



%% ----------------------------------------------------------------------
%% Types: 
% TODO: export from ejabberd_sql and remove it
%% ----------------------------------------------------------------------

-type sql_query_result() :: {updated,non_neg_integer()} |
                            {error, binary() | atom()} |
                            {selected, [binary()], [[binary()]]} |
                            {selected, [any()]} |
                            ok.

%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------

init(_Host, _Opts) ->
    % TODOS: add...
    ok.

-spec get_inbox(LUsername :: jid:luser(),
                LServer :: jid:lserver(),
                Params :: mod_inbox:get_inbox_params()) -> get_inbox_res().
get_inbox(LUsername, LServer, Params) ->
    case get_inbox_rdbms(LUsername, LServer, Params) of
        {selected, []} ->
            [];
        {selected, Rest} ->
            [decode_row(LServer, R) || R <- Res]
    end.

% ? Be careful, maybe it doesnt work, why: using binary data in ?SQL as string
-spec get_inbox_rdbms(LUser :: jid:luser(),
                    LServer :: jid:lserver(),
                    Params :: mod_inbox:get_inbox_params()) ->
                sql_query_result()
get_inbox_rdbms(LUser, LServer, #{ order := Order } = Params) ->
    OrderSQL = order_to_sql(Order),
    BeginSQL = sql_and_where_timestamp(">=", maps:get(start, Params, undefined)),
    EndSQL = sql_and_where_timestamp("<=", maps:get('end', Params, undefined)),
    HiddenSQL = sql_and_where_unread_count(maps:get(hidden_read, Params, false)),
    Query = ?SQL("select remote_bare_jid, content, unread_count, timestamp from inbox "
                "where luser=%(LUser)s and lserver=%(LServer)s "
            "%(BeginSQL)b %(EndSQL)b %(HiddenSQL)b order by timestamp %(OrderSQL)b"),
    ejabberd_sql:sql_query(LServer, Query).

get_inbox_unread(Username, Server, InterlocutorJID) ->
    RemBareJIDBin = jid:to_string(jid:remove_resource(InterlocutorJID)),
    QuerySQL = ?SQL("select unread_count from inbox "
            "where luser=%(Username)s and lserver=%(Server)s "
            "and remote_bare_jid=%(RemBareJIDBin)b"),
    Res = ejabberd_sql:sql_query(Server, QuerySQL),
    {ok, Val} = check_result(Val),
    %% We read unread_count value when the message is sent and is not yet in receiver inbox
    %% so we have to add +1
    {ok, Val + 1}.


-spec set_inbox(Username, Server, ToBareJid, Content,
                Count, MsgId, Timestamp) -> inbox_write_res() when
                Username :: jid:luser(),
                Server :: jid:lserver(),
                ToBareJid :: binary(),
                Content :: binary(),
                Count :: integer(),
                MsgId :: binary(),
                Timestamp :: integer().
set_inbox(Username, Server, ToBareJid, Content, Count, MsgId, Timestamp) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    LToBareJid = jid:nameprep(ToBareJid),
    % ? Original data
    % *InsertParams = [Username, LServer, LToBareJid,
    % *               Content, Count, MsgId, Timestamp],
    % *UpdateParams = [Content, Count, MsgId, Timestamp],
    % *UniqueKeyValues = [LUsername, LServer, LToBareJid],
    % ? changed version
    % ! Probably timestamp in SQL_UPSERT need to be binary, 
    % ! using integer now
    case ?SQL_UPSERT(
                LServer,
                "inbox",
                ["!luser=%(LUsername)s",
                 "!lserver=%(LServer)s",
                 "!remote_bare_jid=%(LToBareJid)s",
                 "content=%(Content)b",
                 "unread_count=%(Count)d",
                 "msg_id=%(MsgId)b",
                 "timestamp=%(Timestamp)d"
                ]) of
            ok ->
                ok;
            Err ->
                Err
        end.


-spec remove_inbox(Username :: binary(),
                   Server :: binary(),
                   ToBareJid :: binary()) -> ok.
remove_inbox(Username, Server, ToBareJid) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    LToBareJid = jid:nameprep(ToBareJid),
    Res = remove_inbox_rdbms(LUsername, LServer, LToBareJid),
    check_result(Res).

-spec remove_inbox_rdbms(LUsername :: jid:luser(),
                        LServer :: jid:lserver(),
                        ToBareJid :: binary()) -> sql_query_result().
remove_inbox_rdbms(LUsername, LServer, ToBareJid) ->
    Query = ?SQL("delete from inbox where luser=%(LUsername)s "
                "and lserver=%(LServer)s and remote_bare_jid=%(ToBareJid)b"),
    ejabberd_sql:sql_query(LServer, Query).


-spec set_inbox_incr_unread(Username :: binary(),
                            Server :: binary(),
                            ToBareJid :: binary(),
                            Content :: binary(),
                            MsgId :: binary(),
                            Timestamp :: erlang:timestamp()) -> ok | {ok, integer()}.
set_inbox_incr_unread(Username, Server, ToBareJid, Content, MsgId, Timestamp) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    LToBareJid = jid:nameprep(ToBareJid),
    %! Res = Not implemented yet
    %** Res = BackendModule:set_inbox_incr_unread(LUsername, LServer, LToBareJid,
    % ? For postgreSQL only, for now
    Res = set_inbox_incr_unread_pg(LUsername, LServer, LToBareJid,
                                Content, MsgId, Timestamp),
    Content, MsgId, Timestamp),
    %% psql will return {updated, {[UnreadCount]}}
    %% mssql and mysql will return {selected, {[Val]}}
    check_result(Res).



%% ----------------------------------------------------------------------
%% For PostgreSQL DB only
%% ----------------------------------------------------------------------

%%% ! Search about how make a sql query
-spec set_inbox_incr_unread_pg(Username :: jid:luser(),
                            Server :: jid:lserver(),
                            ToBareJid :: binary(),
                            Content :: binary(),
                            MsgId :: binary(),
                            Timestamp :: non_neg_integer()) -> sql_query_result().
set_inbox_incr_unread_pg(Username, Server, ToBareJid, Content, MsgId, Timestamp) ->
    Query = ?SQL("insert into inbox(luser, lserver, remote_bare_jid,"
                "content, unread_count, msg_id, timestamp)"
                " values (%(Username)s, %(Server)s, %(ToBareJid), %(Content), 1"
                " %(MsgId), %(Timestamp)s )"
                " on conflict (luser, lserver, remote_bare_jid) do "
                " update set content=%(Content)s,"
                " unread_count=inbox.unread_count + 1,"
                " msg_id=%(MsgId)s"
                " timestamp=%(Timestamp)s, returning unread_count"),
    ejabberd_sql:sql_query(Server, Query).

%% ----------------------------------------------------------------------
%% Finish
%% ----------------------------------------------------------------------


-spec reset_unread(User :: binary(),
                   Server :: binary(),
                   BareJid :: binary(),
                   MsgId :: binary() | undefined
                    ) -> ok.
reset_unread(Username, Server, ToBareJid, MsgId) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    LToBareJid = jid:nameprep(ToBareJid),
    Res = reset_inbox_unread_rdbms(LUsername, LServer, LToBareJid, MsgId),
    check_result(Res).



-spec reset_inbox_unread_rdbms(Username :: jid:luser(),
                               Server :: jid:lserver(),
                               ToBareJid :: binary(),
                               MsgId :: binary() | undefined) -> sql_query_result().
reset_inbox_unread_rdbms(Username, Server, ToBareJid, undefined) ->
    ejabberd_sql:sql_query(Server, ?SQL("update inbox set unread_count=0 "
                    "where luser=%(Username)s and lserver=%(Server)s "
                    "and remote_bare_jid=%(ToBareJid)b"));
reset_inbox_unread_rdbms(Username, Server, ToBareJid, MsgId) ->
    ejabberd_sql:sql_query(Server, ?SQL("update inbox set unread-count=0 where luser=%(Username)s "
                    "and lserver=%(Server)s and remote_bare_jid=%(ToBareJid)s "
                    "and msg_id=%(MsgId)b")).


-spec clear_inbox(Username :: binary(), Server :: binary()) -> inbox_write_res().
clear_inbox(Username, Server) ->
    LUsername = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    Res = clear_inbox_rdbms(LUsername, LServer),
    check_result(Res).

-spec clear_inbox(Server :: binary()) -> inbox_write_res().
clear_inbox(Server) ->
    LServer = jid:nameprep(Server),
    Res = clear_inbox_rdbms(LServer),
    check_result(Res).



%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------


-spec order_to_sql(Order :: asc | desc) -> binary().
order_to_sql(asc) -> <<"ASC">>;
order_to_sql(desc) -> <<"DESC">>.

-spec sql_and_where_timestamp(Operator :: string(), Timestamp :: erlang:timestamp()) -> iolist().
sql_and_where_timestamp(_Operator, undefined) ->
    [];
sql_and_where_timestamp(Operator, Timestamp) ->
    NumericTimestamp = usec:from_now(Timestamp),
    % ! maybe will throw an ERROR
    [" and timestamp ", Operator, NumericTimestamp].

-spec sql_and_where_unread_count(HiddenRead :: boolean()) -> iolist().
sql_and_where_unread_count(true) ->
    [" and unread_count ", " > ", <<"0">>];
sql_and_where_unread_count(_) ->
    [].

-spec clear_inbox_rdbms(Username :: jid:luser(), Server :: jid:lserver()) -> sql_query_result().
clear_inbox_rdbms(Username, Server) ->
    ejabberd:sql_query(Server, ?SQ;("delete from inbox where luser=%(Username)s "
            " and lserver=%(Server)s")).

-spec clear_inbox_rdbms(Server :: jid:lserver()) -> sql_query_result().
clear_inbox_rdbms(Server) ->
    ejabberd_sql:sql_query(Server, ?SQL("delete from inbox")).



%%%
%%% 
-spec result_to_integer(binary() | integer()) -> integer().
result_to_integer(Int) when is_integer(Int) ->
    Int;
result_to_integer(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin).

-spec decode_row(host(), {username(), binary(), count_bin(), non_neg_integer() | binary()}) ->
    inbox_res().
decode_row(LServer, {Username, Content, Count, Timestamp}) ->
    % TODO: unescape_binary Content
    Data = Content,
    BCount = count_to_bin(Count),
    NumericTimestamp = result_to_integer(Timestamp),
    % TODO: check inbox_res
    {Username, Data, BCount, usec:to_now(NumericTimestamp)}.


count_to_bin(Count) when is_integer(Count) -> integer_to_binary(Count);
count_to_bin(Count) when is_binary(Count) -> Count;


%%%% TODO: UNDERSTAND HOW THIS WORK
check_result({updated, Val}, ValList) when is_list(ValList) ->
    case lists:member(Val, ValList) of
        true ->
            ok;
        _ ->
            {error, {expected_does_not_match, Val, ValList}}
    end;
check_result(Result, _) ->
    {error, {bad_result, Result}}.

check_result({selected, []}) ->
    {ok, 0};

check_result({selected, [{Val}]}) ->
    parse_result(Val);
check_result({updated, _, [{Val}]}) ->
    parse_result(Val);
check_result({updated, _}) ->
    ok;
check_result(Result) ->
    {error, {bad_result, Result}}.

parse_result(Value) when is_integer(Value) ->
    {ok, Value};
parse_result(Value) when is_binary(Value) ->
    {ok, binary_to_integer(Value)};
parse_result(null) ->
    {ok, 0};
parse_result(Value) ->
    {error, {unknown_result_value_type, Value}}.