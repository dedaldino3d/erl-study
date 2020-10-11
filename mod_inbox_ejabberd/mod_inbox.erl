%%%-------------------------------------------------------------------------------------
%%% @author dedaldino3D
%%%-------------------------------------------------------------------------------------

-module(mod_inbox).
-author({"dedaldino3D", "dedaldinoantonio7@gmail.com"}).
% -include_lib("xmpp/include/xmpp.hrl").
-include("ejabberd.hrl").
-include_lib("xmpp.hrl").
-include("logger.erl").
-include("mod_inbox.hrl").

-behaviour(gen_mod). % ? Behaviour changed 

 %% ejabberb module API
-export([
        start/2,
        stop/1,
        reload/3,
        depends/2,
        mod_opt_type/1,
        ]).
-export([get_personal_data/2]).
% ? deps changed to depends
-export([process_iq/4,
        process_iq_conversation/4,
        user_send_packet/4,
        filter_packet/1,
        inbox_unread_count/2,
        remove_user/3
    ]).
-export([clear_inbox/2]).

-callback init(Host, Opts) -> ok | {ok, pid()} when
            Host :: binary(),
            Opts :: gen_mod:opts().

-callback get_inbox(LUsername, Server, Params) -> get_inbox_res() when
                    LUsername :: jid:luser(),
                    LServer :: jid:lserver(),
                    Params :: get_inbox_params().

-callback set_inbox(Username, Server, ToBareJid,
                    Content, Count, MsgId, Timestamp) -> inbox_write_res() when
                    Username :: jid:luser(),
                    Server :: jid:lserver(),
                    ToBareJid :: binary(),
                    Content :: binary(),
                    Count :: integer(),
                    MsgId :: binary(),
                    Timestamp :: integer().

-callback remove_inbox(Username, Server, ToBareJid) -> inbox_write_res() when
                    Username :: jid:luser(),
                    Server :: jid:lserver(),
                    ToBareJid :: integer().

-callback set_inbox_incr_unread(Username, Server, ToBareJid,
                                Content, MsgId, Timestamp) -> {ok, integer()} | ok when
                                Username :: jid:luser(),
                                Server :: jid:lserver(),
                                ToBareJid :: binary(),
                                Content :: binary(),
                                MsgId :: binary(),
                                Timestamp :: erlang:timestamp().

-callback reset_unread(Username, Server, BareJid, MsgId) -> inbox_write_res() when
                        Username :: jid:luser(),
                        Server :: jid:lserver(),
                        BareJid :: binary(),
                        MsgId :: binary().

-callback clear_inbox(Server) -> inbox_write_res() when
                    Server :: jid:lserver().

-callback clear_inbox(Username, Server) -> inbox_write_res() when
                        Username :: jid:luser(),
                        Server :: jid:lserver().

-callback get_inbox_unread(Username, Server, InterlocutorJID) -> {ok, integer()} when
                            Username :: jid:luser(),
                            Server :: jid:lserver(),
                            InterlocutorJID :: jid:jid().

-type get_inbox_params() :: #{
        start => erlang:timestamp(),
        'end' => erlang:timestamp(),
        order => asc | desc,
        hidden_read => true | false
        }.

-type entry() :: [string(), binary()].
-type personal_data() :: [{atom(), [string()], [entry()]}].
-export_type([get_inbox_params/0]).

%%--------------------------------------------------------------------
%% callbacks
%%--------------------------------------------------------------------

-spec get_personal_data(personal_data(), jid:jid()) -> personal_data().
get_personal_data(Acc, #jid({ luser = LUser, lserver = LServer }) ->
    Schema = ["jid", "content", "unread_count", "timestamp"],
    InboxParams = #{
        start => {0,0,0},
        'end' => erlang:timestamp(),
        order => asc,
        hidden_read => false
    },
    % mod_inbox_backend is mod_inbox_rdbms
    Entries = mod_inbox_backend:get_inbox(LUser, LServer, InboxParams),
    ProcessedEntries = lists:map(fun process_entry/1, Entries),
    [{inbox, Schema, ProcessedEntries} | Acc].

process_entry({RemJID, Content, UnreadCount, Timestamp}) ->
    USec = usec:from_now(Timestamp),
    TS = calendar:system_time_to_rfc3339(USec, [{offset, "Z"}, {unit, microsecond}]),
    {RemJID, Content, UnreadCount, TS}.

%%--------------------------------------------------------------------
%% inbox callbacks
%%--------------------------------------------------------------------

-spec depends(jid:lserver(), list()) -> list().
depends(_Host, _Opts) ->
    [{mod_muc, hard}].
    % groupchat_deps(Opts).

-spec start(Host :: binary(), Opts :: gen_mod:opts()) -> ok.
start(Host, Opts) ->
    % ? need to register codec when initializes custom module
    FullOpts = case lists:keyfind(backend, 2, Opts) of
                false ->
                    [{backend, rdms} | Opts];
                _ ->
                    Opts
            end,
    % initialize database module
    Mod = gen_mod:db_mod(Opts, ?MODULE),
    Mod:init(Host, Opts),
    mod_inbox_backend:init(Host, FullOpts),
    mod_disco:register_extra_domain(Host, ?NS_ESL_INBOX),
    IQDisc = gen_mod:get_opt(iqdisc, FullOpts, no_queue),
    %? MucTypes = get_groupchat_types(Host),
    % lists:member(muc, MucTypes) andalso mod_inbox_muc:start(Host),
    ejabberd_hooks:add(hooks(Host)), %%% Hooks
    store_bin_reset_markers(Host, FullOpts),
    % IQ Handlers
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX,
                                ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_ESL_IBOX_CONVERSATION,
                                ?MODULE, process_iq_conversation, IQDisc).


-spec stop(Host :: jid:lserver()) -> ok.
stop(Host) ->
    mod_disco:unregister_extra_domain(Host, ?NS_ESL_INBOX),
    mod_inbox_muc:stop(Host),
    ejabberd_hooks:delete(hooks(Host)),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_ESL_INBOX).


%%%%%%%%%%%%%%%%%%%
%% Process IQ
%%? mongoose_acc:t() to any()
-spec process_iq(From :: jid:jid(),
                To :: jid:jid(),
                Acc :: any(),
                IQ :: xmpp:iq()) -> {stop, any()} | {any(), xmpp:iq()}.
process_iq(_From, _To, Acc, #iq{type = get, sub_el = SubEl} = IQ) ->
    Form = build_inbox_form(),
    SubElWithForm = SubEl#xmlel{ children = [Form]},
    {Acc, IQ#iq{type = result, sub_el = SubElWithForm}};
process_iq(From, _To, Acc, #iq{type = set, id = QueryId, sub_el = QueryEl} = IQ) ->
    Username = From#jid.luser,
    Host = From#jid.server,
    case form_to_params(xmpp:get_subtag(QueryEl, #xdata{})) of
        {error, bad_request, Msg} ->
            % TODO: How generate bad_request from xmpp lib and use translate
            {Acc, IQ#iq{type = error, sub_el = [xmpp:bad_request(<<"en">>, Msg)]}};
        Params ->
            List = mod_inbox_backend:get_inbox(Username, Host, Params),
            forward_messages(List, QueryId, From),
            Res = IQ#iq{type = result, sub_el = [build_result_iq(List)]},
            {Acc, Res}
    end.

%%? mongoose_acc:t() to any()
-spec process_iq_conversation(From :: jid:jid(),
                            To :: jid:jid(),
                            Acc :: any(),
                            IQ :: xmpp:iq()) -> {stop, any()} | {any(), xmpp:iq()}.
process_iq_conversation(From, _To, Acc,
                        #iq{type = set,
                            xmlns = ?NS_ESL_INBOX_CONVERSATION,
                            sub_el = #xmlel{name = <<"reset">>} = ResetStanza} = IQ) ->
    maybe_process_reset_stanza(From, Acc, IQ, ResetStanza).

maybe_process_reset_stanza(From, Acc, IQ, ResetStanza) ->
    case reset_stanza_extract_interlocutor_jid(ResetStanza) of
        {error, Msg} ->
            % TODO: How generate bad_request from xmpp lib and use translate
            {Acc, IQ#iq{type = error, sub_el = [xmpp:bad_request(<<"en">>, Msg)]}};
        InterlocutorJID ->
            process_reset_stanza(From, Acc, IQ, ResetStanza, InterlocutorJID)
    end.

process_reset_stanza(From, Acc, IQ, _ResetStanza, InterlocutorJID) ->
    ok = mod_inbox_utils:reset_unread_count_to_zero(From, InterlocutorJID),
    {Acc, IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"reset">>,
                                attrs = [{<<"xmlns">>, ?NS_ESL_INBOX_CONVERSATION}],
                                children = []}]}}.

%%? mongoose_acc:t() to any()
-spec forward_messages(List :: list(inbox_res()),
                        QueryId :: id(),
                        To :: jid:jid()) -> list(any()).
forward_messages(List, QueryId, To) when is_list(List) ->
    Msgs = [build_inbox_message(El, QueryId) || El <- List],
    [send_message(To, Msg) || MSG <- Msgs].


%%? mongoose_acc:t() to any()
-spec send_message(To :: jid:jid(), Msg :: xmlel()) -> any().
send_message(To, Msg) ->
    BareTo = jid:remove_resource(To),
    ejabberd_sm:route(BareTo, To, Msg).


%%%%%%%%%%%%%%%%%%%
%% Handlers
-spec user_send_packet(Acc :: map(), From :: jid:jid(),
                    To :: jid:jid(),
                    Packet :: xmlel()) -> map()
user_send_packet(Acc, From, To, #xmlel{name = <<"message">>} = Msg) ->
    Host = From#jid.server,
    maybe_process_message(Host, From, To, Msg, outgoing),
    Acc;
user_send_packet(Acc, _From, _To, _Packet) ->
    Acc.

%%? mongoose_acc:t() to any()
-spec inbox_unread_count(Acc :: any(), To :: jid:jid()) -> any().
inbox_unread_count(Acc, To) ->
    % Res = mongoose_acc:get(inbox, unread_count, undefined, Acc),
    get_inbox_unread(undefined, Acc, To).


-type fpacket() :: {From :: jid:jid(),
                    To :: jid:jid(),
                    Acc :: any(),
                    Packet :: xmlel()}.
-spec filter_packet(Value :: fpacket() | drop) -> fpacket() | drop.
filter_packet(drop)  ->
    drop;
filter_packet({From, To, Acc, Packet}) ->
    {From, To, Acc, Packet}.

% filter_packet({From, To, Acc, Msg = #xmlel{name = <<"message">>}}) ->
    % Host = To#jid.server,
    %% In case of PgSQL we can update inbox and obtain unread_count in one query,
    %% so we put it in accumulator here.
    %% In case of MySQL/MsSQL it costs an extra query, so we fetch it only if necessary
    %% (when push notification is created)
    % Acc0 = case maybe_process_message(Host, From, To, Msg, incoming) of
    %     {ok, UnreadCount} ->
    %         mongoose_acc:set(inbox, unread_count, UnreadCount, Acc);
    %     _ ->
    %         Acc
    %     end,
    % {From, To, Acc, Msg};

remove_user(Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    mod_inbox_backend:clear_inbox(LUser, LServer),
    Acc.


-spec maybe_process_message(Host :: host(),
                            From :: jid:jid(),
                            To :: jid:jid(),
                            Msg :: xmlel(),
                            Dir :: outgoing | incoming) -> ok | {ok, integer()}.

maybe_process_message(Host, From, To, Msg, Dir) ->
    case should_be_stored_in_inbox(Msg) andalso inbox_owner_exists(From, To, Dir) of
        true ->
            Type = get_message_type(Msg),
            maybe_process_acceptable_message(Host, From, To, Msg, Dir, Type);
        false ->
            ok
    end.


-spec inbox_owner_exists(From :: jid:jid(),
                        To :: jid:jid(),
                        Dir :: outgoing | incoming) -> boolean().
inbox_owner_exists(From, _To, outgoing) ->
    ejabberd_auth:user_exists(From#jid.luser, From#jid.lserver);
inbox_owner_exists(_From, To, incoming) ->
    ejabberd_auth:user_exists(To#jid.luser, To#jid.lserver).

maybe_process_acceptable_message(Host, From, To, Msg, Dir, one2one) ->
    process_message(Host, From, To, Msg, Dir, one2one);
maybe_process_acceptable_message(Host, From, To, Msg, Dir, groupchat) ->
    process_message(Host, From, To, Msg, Dir, groupchat).


-spec process_message(Host :: host(),
                      From :: jid:jid(),
                      To :: jid:jid(),
                      Message :: xmlel(),
                      Dir :: outgoing | incoming,
                      Type :: one2one | groupchat) -> ok {ok, integer()}.
process_message(Host, From, To, Message, outgoing, one2one) ->
    mod_inbox_one2one:handle_outgoing_message(Host, From, To, Message);
process_message(Host, From, To, Message, incoming, one2one) ->
    mod_inbox_one2one:handle_incoming_message(Host, From, To, Message);
% TODO: adapt it to use mucsub instead muclight 
process_message(Host, From, To, Message, outgoing, groupchat) ->
    mod_inbox_muclight:handle_outgoing_message(Host, From, To, Message);
process_message(Host, From, To, Message, incoming, groupchat) ->
    mod_inbox_muclight:handle_incoming_message(Host, From, To, Message);
process_message(Host, From, To, Mesage, Dir, Type) ->
    ?LOG_WARNING(#{what => inbox_unknown_message,
                    text => <<"Unknown message, was not written into inbox">>,
                    exml_packet => Message,
                    %%%? jid:to_binary() to to_string
                    from_jid => jid:to_string(From), to_jid => jid:to_string(To),
                    SERVER => Host, dir => Dir, inbox_message_type => Type}),
    ok.


%%%%%%%%%%%%%%%%%%%
%% Stanza builders

-spec build_inbox_message(inbox_res(), id()) -> xmlel().
build_inbox_message({_Username, Content, Count, Timestamp}, QueryId) ->
    #xmlel{name = <<"message">>, attrs = [{<<"id">>, mod_inbox_utils:wrapper_id()}],
            children =[build_result_el(Content, QueryId, Count, Timestamp)]}.

-spec build_result_el(content(), id(), count_bin(), erlang:timestamp()) -> xmlel().
build_result_el(Msg, QueryId, BinUnread, Timestamp) ->
    Forwarded = build_forward_el(Msg, Timestamp),
    QueryAttr = [{<<"queryid">>, QueryId} || QueryId =/= undefined, QueryId =/= <<>>],
    #xmlel{name = <<"result">>, attrs =[{<<"xmlns">>, ?NS_ESL_INBOX}, {<<"unread">>, BinUnread}] ++
    QueryAttr, children = [Forwarded]}.

-spec build_result_iq(get_inbox_res()) -> xmlel().
build_result_iq(List) ->
    AllUnread = lists:filter(fun(E) -> E =/= 0 end,
                            [extract_unread_count(E) || E <- List]),
    
    Result = #{<<"count">> => length(List),
                <<"unread-messages">> => lists:sum(AllUnread),
                <<"active-conversations">> => length(AllUnread)},
    ResultBinary = maps:map(fun(K, V) ->
                        build_result_el(K, integer_to_binary(V)) end, Result),
    #xmlel{name = <<"fin">>, attrs = [{<<"xmlns">>, ?NS_ESL_INBOX}],
            children = maps:values(ResultBinary)}.

-spec build_result_el(name_bin(), count_bin()) -> xmlel().
build_result_el(Name, CountBin) ->
    #xmlel{name = Name, children = [#xmlcdata{content = CountBin}]}.

-spec build_forward_el(content(), erlang:timestamp()) -> xmlel().
build_forward_el(Content, Timestamp) ->
    %%% TODO : exml:parse to xmpp:decode
    Parsed = xmpp:decode(Content),
    Delay = build_delay_el(Timestamp),
    #xmlel{name = <<"forwarded">>, attrs = [{<<"xmlns">>, ?NS_FORWARD}],
            children = [Delay, Parsed]}.

-spec build_delay_el(Timestamp :: erlang:timestamp()) -> xmlel().
build_delay_el(Timestamp) ->
    USec = usec:from_now(Timestamp),
    TS = calendar:system_time_to_rfc3339(USec, [{offset, "Z"}, {unit, microsecond}]),
    timestamp_to_xml(TS, undefined, undefined).

-spec build_inbox_form() -> xmlel().
build_inbox_form() ->
    OrderOptions = [
                    {<<"Ascending by timestamp">>, <<"asc">>},
                    {<<"Descending by timestamp">>, <<"desc">>}
                ],
    FormFields = [
                    form_field({<<"FORM_TYPE">>, <<"hidden">>, ?NS_ESL_INBOX}),
                    text_single_form_field(<<"start">>),
                    text_single_form_field(<<"end">>),
                    list_single_form_field(<<"order">>, <<"desc">>, OrderOptions),
                    text_single_form_field(<<"hiddend_read">>, <<"false">>),
                ],
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"form">>}],
            children = FormFields }.

-spec text_single_form_field(Var :: binary()) -> xmlel().
text_single_form_field(Var) ->
    #xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}, {<<"type">>, <<"text-single">>}]}.


-spec text_single_form_field(Var :: binary(), DefaultValue :: binary()) -> xmlel().
text_single_form_field(Var, DefaultValue) ->
    #xmlel{name = <<"field">>,
            attrs = [{<<"var">>, Var}, {<<"type">>, <<"text-single">>}, {<<"value">>, DefaultValue}]}.

-spec list_single_form_field(Var :: binary(),
                                Default :: binary(),
                                Options :: [{Label :: binary(), Value :: binary()}]) ->
    xmlel().
list_single_form_field(Var, Default, Options) ->
    Value = form_field_value(Default),
    #xmlel{
        name = <<"field">>,
        attrs = [{<<"var">>, Var}, {<<"type">>, <<"list-single">>}],
        children = [Value | [ form_field_option(Label, OptValue) || {Label, OptValue} <- Options ]]
        }.

-spec form_field_option(Label :: binary(), Value :: binary()) -> xmlel().
form_field_option(Label, Value) ->
    #xmlel{
        name = <<"option">>,
        attrs = [{<<"label">>, Label}],
        children = [form_field_value(Value)]
        }.

-spec form_field_value(Value :: binary()) -> xmlel().
form_field_value(Value) ->
    #xmlel{ name = <<"value">>, children = [#xmlcdata{ content = Value }] }.

%%%%%%%%%%%%%%%%%%%
%% Helpers
get_inbox_unread(Value, Acc, _) when is_integer(Value) ->
    Acc;
get_inbox_unread(undefined, Acc, To) ->
    {User, Server, _} = jid:tolower(To),
    %%? mongoose_acc:from_jid(Acc) to xmpp:get_from(Acc),
    InterlocutorJID = xmpp:get_from(Acc),
    {ok, Count} = mod_inbox_utils:get_inbox_unread(User, Server, InterlocutorJID),
    % mongoose_acc:set(inbox, unread_count, Count, Acc).


hooks(Host) ->
    [
        {remove_user, Host, ?MODULE, remove_user, 50},
        {user_send_packet, Host, ?MODULE, user_send_packet, 70},
        {filter_packet, Host, ?MODULE, filter_packet, 90},
        {inbox_unread_count, Host, ?MODULE, inbox_unread_count, 80},
        get_personal_data, Host, ?MODULE, get_personal_data, 50
    ].

%%%! Removed
% get_groupchat_types(Host) ->
%     % TODO: understand what this line mean and adapt it to mucsub
%     gen_mod:get_module_opt(Host, ?MODULE, groupchat, [muclight]).
%! Finish

%%% TODO: Adapt all muclight to mucsub
-spec muclight_enabled(Host :: binary()) -> boolean().
muclight_enabled(Host) ->
    Groupchats = get_groupchat_types(Host),
    lists:member(muclight, Groupchats).

-spec form_to_params(FormEl :: xmlel() | undefined) ->
    get_inbox_params() | {error, bad_request, Msg :: binary()}.
form_to_params(undefined) ->
    #{ order => desc };
% form_to_params(FormEl) ->
%     %%% TODO: how implement that in ejabberd: parse_xdata_fields
%     ParsedFields = jlib:parse_xdata_fields(xmpp:get_subtags(FormEl, #xdata_field{})),
%     %% TODO: log using logger.hrl
%     ?LOG_DEBUG(#{what => inbox_parsed_form_fields, parsed-fields => ParsedFields}),
%     fields_to_params(ParsedFields, #{ order => desc }).
%%% new implementation
form_to_params(FormEl) ->
    ParsedFields = parse_xdata_fields(xmpp:get_subtags(FormEl, #xdata_field{})),
    %% TODO: log using logger.hrl
    ?LOG_DEBUG(#{what => inbox_parsed_form_fields, parsed-fields => ParsedFields}),
    fields_to_params(ParsedFields, #{ order => desc }).

-spec fields_to_params([{Var :: binary(), Values :: [binary()]}], Acc :: get_inbox_params()) ->
    get_inbox_params() | {error, bad_request, Msg :: binary()}.
fields_to_params([], Acc) ->
    Acc;
fields_to_params([{<<"start">>, [StartISO]} | RFields], Acc) ->
    try calendar:rfc3339_to_system_time(binary_to_list(StartISO), [{unit, microsecond}]) of
        StartStamp ->
            fields_to_params(RFields, Acc#{ start => usec:to_now(StartStamp) })
    case error:Error ->
        %% TODO: log using logger.hrl
        ?LOG_WARNING(#{what => inbox_invalid_form_field,
                            reason => Error, field => start, value = StartISO}),
                            {error, bad_request, invalid_field_value(<<"start">>, StartISO)}
    end;

fields_to_params([{<<"end">>, [EndISO]} | RFields], Acc) ->
    try calendar:rfc3339_to_system_time(binary_to_list(EndISO), [{unit, microsecond}]) of
        EndStamp ->
            fields_to_params(RFields, Acc#{ 'end' => usec:to_now(EndStamp) })
    catch error:Error ->
        %% TODO: log using logger.hrl
        ?LOG_WARNING(#{what => inbox_invalid_form_field,
                        reason => Error, field => 'end', value => EndISO}),
        {error, bad_request, invalid_field_value(<<"end">>, EndISO)}
    end;

fields_to_params([{<<"order">>, [OrderBin]} | RFields], Acc) ->
    case binary_to_order(OrderBin) of
        error ->
            %% TODO: log using logger.hrl
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                            field => order, value => OrderBin}),
            {error, bad_request, invalid_field_value(<<"order">>, OrderBin)};
        Order ->
            fields_to_params(RFields, Acc#{ order => Order })
    end;

fields_to_params([{<<"hidden_read">>, [HiddenRead]} | RFields], Acc) ->
    case binary_to_bool(HiddenRead) of
        error ->
            %% TODO: log using logger.hrl
            ?LOG_WARNING(#{what => inbox_invalid_form_field,
                            field => hidden_read, value => HiddenRead}),
            {error, bad_request, invalid_field_value(<<"hidden_read">>, HiddenRead)};
        Hidden ->
            fields_to_params(RFields, Acc#{ hidden_read => Hidden })
    end;

fields_to_params([{<<"FORM_TYPE">>, _} | RFields], Acc) ->
    fields_to_params(RFields, Acc);
fields_to_params([{Invalid, [InvalidFieldVal]} | _], _) ->
    %% TODO: log using logger.hrl
    ?LOG_INFO(#{what => inbox_invalid_form_field, reason => unknown_field,
                field => Invalid, value => InvalidFieldVal}),
    {error, bad_request, <<"Unknown inbox form field=", Invalid/binary, ", value=", InvalidFieldVal/binary>>}.

-spec binary_to_order(binary()) -> asc | desc | error.
binary_to_order(<<"desc">>) -> desc;
binary_to_order(<<"asc">>) -> asc;
binary_to_order(_) -> error.

-spec binary_to_bool(binary()) -> true | false | error.
binary_to_bool(<<"true">>) -> true;
binary_to_bool(<<"false">>) -> false;
binary_to_bool(_) -> error.


%%% TODO: chat markers 0384 is not supported, create a custom module for it
-spec store_bin_reset_markers(Host :: host(), Opts :: list()) -> boolean().
store_bin_reset_markers(Host, Opts) ->
    ResetMarkers = gen_mod:get_opt(reset_markers, Opts, [displayed]),
    ResetMarkersBin = [mod_inbox_utils:reset_marker_to_bin(Marker) || Marker <- ResetMarkers],
    gen_mod:set_module_opt(Host, ?MODULE, reset_markers, ResetMarkersBin).


-spec get_message_type(Msg :: xmlel()) -> groupchat | one2one.
% get_message_type(Msg) ->
%     case exml_query:attr(Msg, <<"type">>, undefined) of
%         <<"groupchat">> ->
%             groupchat;
%         _ ->
%             one2one
%     end.

%%% new implementation
get_message_type(Msg) ->
    case xmpp:get_type(Msg) of
        <<"groupchat">> ->
            groupchat;
        _ ->
            one2one
    end.

% reset_stanza_extract_interlocutor_jid(ResetStanza) ->
%     case exml_query:attr(ResetStanza, <<"jid">>) of
%         undefined ->
%             {error, invalid_field_value(<<"jid">>, <<"No Interlocutor JID provided">>)};
%         Value ->
%             case jid:from_binary(Value) of
%                 error ->
%                     ?LOG_ERROR(#{what => inbox_invalid_form_field,
%                                     field => jid, value => Value}),
%                     {error, invalid_field_value(<<"jid">>, Value)};
%                 JID -> JID
%             end
%     end.

%%% new implementation
reset_stanza_extract_interlocutor_jid(#xmlel{attrs = Attrs} = ResetStanza) ->
    case fxml:get_attr(<<"jid">>, Attrs) of
        false ->
            {error, invalid_field_value(<<"jid">>, <<"No Interlocutor JID provided">>)};
        {value, Value} ->
            case jid:from_string(Value) of
                error ->
                    ?LOG_ERROR(#{what => inbox_invalid_form_field,
                                    field => jid, value => Value}),
                    {error, invalid_field_value(<<"jid">>, Value)};
                JID -> JID
            end
    end.


-spec clear_inbox(Username :: jid:luser(), Server :: host()) -> inbox_write_res().
clear_inbox(Username, Server) ->
    mod_inbox_utils:clear_inbox(Username, Server).


%%% Loading dependencies
%%% Muclight is not supported, use mucsub instead

%%% TODO: muclight is not supported, adapt to mucsub
%%% ! ALL DELETED
% groupchat_deps(Opts) ->
%     case lists:keyfind(groupchat, 1, Opts) of
%         {groupchat, List} ->
%             muclight_dep(List) ++ muc_dep(List);
%         false ->
%             []
%     end.

% muclight_dep(List) ->
%     case lists:member(muclight, List) of
%         true -> [{mod_muc_light, hard}];
%         false -> []
%     end.

% muc_dep(List) ->
%     case lists:member(muc, Lsit) of
%         true -> [{mod_muc, hard}];
%         false -> []
%     end.
%%% ! FINISH

%%% callbacks funs
% ? not used
callback_funs() ->
    [get_inbox, set_inbox, set_inbox_incr_unread,
    reset_unread, remove_inbox, clear_inbox, get_inbox_unread].


invalid_field_value(Field, Value) ->
    <<"Invalid inbox form field value, field=", Field/binary, ", value=", Value/binary>>.
    


%%%%%%%%%%%%%%%%%%%
%% Message Predicates

-spec should_be_stored_in_inbox(Msg :: xmlel()) -> boolean().
should_be_stored_in_inbox(Msg) ->
    not is_forwarded_message(Msg) andalso
        not is_error_message(Msg) andalso
        not is_offline_message(Msg).

-spec is_forwarded_message(Msg :: xmlel()) -> boolean().
% is_forwarded_message(Msg) ->
%     case exml_query:subelement_with_ns(Msg, ?NS_FORWARD, undefined) of
%         undefined ->
%             false;
%         _ ->
%             true
%     end.
%%%% new implementation
is_forwarded_message(Msg) ->
    case xmpp:get_subtag(Msg, #forwarded{}) of
        false ->
            false;
        _ -> 
            true
    end.


-spec is_error_message(Msg :: xmlel()) -> boolean().
% TODO: implement new function to handle it
% is_error_message(Msg) ->
%     case exml_query:attr(Msg, <<"type">>, undefined) of
%         <<"error">> ->
%             true;
%         _ ->
%             false
%     end.

%%% new implementation
is_error_message(Msg) ->
    case xmpp:get_type(Msg) of
        <<"error">> ->
            true;
        _ ->
            false
    end.

-spec is_offline_message(Msg :: xmlel()) -> boolean().
% is_offline_message(Msg) ->
%     case exml_query:subelement_with_ns(Msg, ?NS_DELAY, undefined) of
%         undefined ->
%             false;
%         _ ->
%             true
%     end.
%%% new implementation
is_offline_message(Msg) ->
    case xmpp:get_subtag(Msg, #delay{}) of
        false ->
            false;
        _ -> 
            true
    end.


extract_unread_count({_, _, Count, _}) ->
    binary_to_integer(Count).

%% Config metrics is unneccessary, drop it
% config_metrics(Host) ->
%     OptsToReport = [{backend, rdbms}], %list of tuples {option, defualt_value}
%     mongoose_module_metrics:opts_for_module(Host, ?MODULE, OptsToReport).

%%%%%%%%%%%% --------------------------------------------------
%%%%%%%%%%%% Parse xdata fields & values, from jlib.erl
%%%%%%%%%%%% --------------------------------------------------

-spec parse_xdata_submit(FormEl :: xmlel()) ->
    invalid | [{VarName :: binary(), Values :: [binary()]}].
parse_xdata_submit(FormEl) ->
    case xmpp:get_type(FormEl) of
        <<"submit">> -> parse_xdata_fields(FormEl#xmlel.children);
        _ -> invalid
    end.

-spec parse_xdata_fields(FormChildren :: [xmlcdata() | xmlel()]) ->
    [{VarName :: binary(), Values :: [binary()]}].
parse_xdata_fields([]) ->
    [];
parse_xdata_fields([#xmlel{ name = <<"field">> } = FieldEl | REls]) ->
    case fxml:get_attr_s(<<"var">>, FieldEl) of
        <<"">> ->
            parse_xdata_fields(REls);
        Var ->
            [ {Var, parse_xdata_values(FieldEl#xmlel.children)} | parse_xdata_fields(REls) ]
    end;
parse_xdata_fields([_ | REls]) ->
    parse_xdata_fields(REls).

-spec parse_xdata_values(VarChildren :: [xmlcdata() | xmlel()]) ->
    Values :: [binary()].
parse_xdata_values([]) ->
    [];
parse_xdata_values([#xmlel{name = <<"value">> } = ValueEl | REls]) ->
    [cdata(ValueEl) | parse_xdata_values(REls)];
parse_xdata_values([_ | REls]) ->
    parse_xdata_values(REls).

%%% maybe use fxml:get_cdata(xmlel()) or fxml:get_tag_cdata(xmlel())
-spec cdata(xmlel()) -> binary().
cdata(#xmlel{children = Children}) ->
    list_to_binary([C || #xmlcdata{content = C} <- Children]).


%%%%%%%%%%%% --------------------------------------------------
%%%%%%%%%%%% from jlib.erl
%%%%%%%%%%%% --------------------------------------------------

-spec timestamp_to_xml(TimestampString :: calendar:rfc3339_string(),
                       FromJID :: jid:jid() | jid:jid() | undefined,
                       Desc :: iodata() | undefined) -> xmlel().
timestamp_to_xml(TimestampString, FromJID, Desc) ->
    Text = case Desc of
               undefined -> [];
               _ -> [#xmlcdata{content = Desc}]
           end,
    From = case FromJID of
               undefined -> [];
               _ -> [{<<"from">>, jid:to_string(FromJID)}]
           end,
    #xmlel{name = <<"delay">>,
           attrs = [{<<"xmlns">>, ?NS_DELAY},
                    {<<"stamp">>, list_to_binary(TimestampString)} | From],
           children = Text}.


-spec form_field({binary(), binary(), binary()}
                    | {binary(), binary()}
                    | {binary(), binary(), binary(), binary()}) -> xmlel().
form_field({Var, Type, Value, Label}) ->
    Field = form_field({Var, Type, Value}),
    Field#xmlel{attrs = [{<<"label">>, Label} | Field#xmlel.attrs]};
form_field({Var, Type, Value}) ->
    Field = form_field({Var, Value}),
    Field#xmlel{attrs = [{<<"type">>, Type} | Field#xmlel.attrs]};
form_field({Var, Value}) ->
    #xmlel{name = <<"field">>,
            attrs = [{<<"var">>, Var}],
            children = [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}]}.

