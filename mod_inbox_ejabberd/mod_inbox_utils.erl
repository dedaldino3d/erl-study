%%%-------------------------------------------------------------------
%%% @author dedaldino3D
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mod_inbox_utils).
% -include_lib("xmpp/include/xmpp.hrl").
-include("xmpp.hrl").
-include("mod_inbox.hrl").
-author({"dedaldino3D", "dedaldinoanonio7@gmail.com"}).


%%%%%%%%%%%%%%%%%%%
%% DB Operations shared by mod_inbox_one2one and mod_inbox_muc
-export([maybe_reset_unread_count/4,
        reset_unread_count_to_zero/2,
        maybe_write_to_inbox/5,
        write_to_sender_inbox/4,
        write_to_receiver_inbox/4,
        clear_inbox/1,
        clear_inbox/2,
        get_reset_markers/1,
        if_chat_marker_get_id/2,
        has_chat_marker/1,
        fill_from_attr/2,
        wrapper_id/0,
        get_option_write_off_changes/1,
        get_option_remove_on_kicked/1,
        reset_marker_to_bin/1,
        get_inbox_unread/3
        ]).


-spec maybe_reset_unread_count(Server :: host(),
                               User :: jid:jid(),
                               Remote :: jid:jid(),
                               Packet :: xmlel()) -> ok.
maybe_reset_unread_count(Server, User, Remote, Packet) ->
    ResetMarkers = mod_inbox_utils:get_reset_markers(Server),
    case mod_inbox_utils:if_chat_marker_get_id(Packet, ResetMarkers) of
        undefined ->
            ok;
        Id ->
            reset_unread_count(User, Remote, Id)
    end.


%% Own implementation of to_bare function: remove resource from JID
% -spec to_bare(jid()) -> jid().
% to_bare(#jid{} = JID) ->
%   JID#jid{resource = <<>>, lresource = <<>>};
% to_bare({U, S, _R}) ->
%   {U, S, <<>>};
% to_bare(error) ->
%   error.

-spec reset_unread_count_to_zero(jid:jid(), jid:jid()) -> ok.
reset_unread_count_to_zero(#jid{luser = FromUsername, lserver = Server}, Remote) ->
    ToBareJid = jid:remove_resource(Remote),
    ok = mod_inbox_backend:reset_unread(FromUsername, Server, ToBareJID, undefined).

-spec reset_unread_count(User :: jid:jid(),
                        Remote :: jid:jid(),
                        MsgId :: id()) -> ok.
reset_unread_count(User, Remote, MsgId) ->
    FromUsername = User#jid.luser,
    Server = User#jid.lserver,    
    ToBareJid = jid:remove_resource(Remote),
    ok = mod_inbox_backend:reset_unread(FromUsername, Server, ToBareJid, MsgId).

-spec write_to_sender_inbox(Server :: host(),
                            Sender :: jid:jid(),
                            Receiver :: jid:jid(),
                            
                            Packet :: xmlel()) -> ok.
write_to_sender_inbox(Server, Sender, Receiver, Packet) ->
    Msgid = get_msg_id(Packet),
    %%%? exml:to_binary(Packet) to fxml:element_to_binary(Packet)
    Content = fxml:element_to_binary(Packet),
    Username = Sender#jid.luser,
    RemoteBareJid = jid:remove_resource(Receiver),
    %% no unread for a user because he writes new messages which assumes he read all previous messages.
    Count = 0,
    Timestamp = erlang:system_time(microsecond),
    ok = mod_inbox_backend:set_inbox(Username, Server, RemoteBareJid,
                                    Content, Count, MsgId, Timestamp).

-spec write_to_receiver_inbox(Server :: host(),
                              Sender :: jid:jid(),
                              Receiver :: jid:jid(),
                              Packet :: xmlel()) -> ok | {ok, integer()}.
write_to_receiver_inbox(Server, Sender, Receiver, Packet) ->
    MsgId = get_msg_id(Packet),
    %%%? exml:to_binary(Packet) to fxml:element_to_binary(Packet)
    Content = fxml:element_to_binary(Packet),
    Username = Receiver#jid.luser,
    RemoteBareJid = jid:encode(jid:remove_resource(Sender)),
    Timestamp = erlang:system_time(microsecond),
    mod_inbox_backend:set_inbox_incr_unread(Username, Server, RemoteBareJid,
                                            Content, MsgId, Timestamp).

-spec clear_inbox(User :: jid:luser(), Server :: host()) -> inbox_write_res().
clear_inbox(User, Server) when is_binary(User) ->
    JidForm = jid:from_string(User),
    ok = mod_inbox_backend:clear_inbox(JidForm#jid.luser, Server).

-spec clear_inbox(Server :: host()) -> inbox_write_res().
clear_inbox(Server) ->
    ok = mod_inbox_backend:clear_inbox(Server).



%%%%%%%%%%%%%%%%%%%
%% Helpers

-spec get_reset_markers(Host :: host()) -> list(marker()).
get_reset_markers(Host) ->
    gen_mod:get_module_opt(Host, mod_inbox, reset_markers, [<<"displayed">>]).

% Get id from an element
-spec if_chat_marker_get_id(Packet :: xmlel(),
                            Markers :: list(marker())) -> undefined | id().
if_chat_marker_get_id(Packet, Markers) when is_list(Markers) ->
    Ids = [if_chat_marker_get_id(Packet, M) || M <- Markers],
    Filtered = [El || El <- Ids, El /= undefined],
    case Filtered of
        [] -> 
            undefined;
        _ -> 
            lists:nth(1, Filtered)
    end;

% if_chat_marker_get_id(Packet, Marker) ->
%     %%% TODO: exml_query:path() removed, CG
%     % Get id from an element
%     case exml_query:path(Packet, [{element, Marker}, {attr, <<"id">>}]) of
%         [Id] ->
%             Id;
%         _ -> 
%             undefined
%     end.

%%% new implementation
if_chat_marker_get_id(Packet, Marker) ->
    case fxml:get_path_s(Packet, [{elem, Marker}, {attr, <<"id">>}]) of
        [Id] ->
            Id;
        _ -> 
            undefined
    end.


-spec has_chat_marker(Packet :: xmlel()) -> boolean().
has_chat_marker(Packet) ->
    has_chat_marker(Packet, all_chat_markers()).

-spec has_chat_marker(Packet :: xmlel(), list(marker())) -> boolean().
% has_chat_marker(Packet, Markers) ->
%     case exml_query:subelement_with_ns(Packet, ?NS_CHAT_MARKERS, no_marker) of
%         no_marker ->
%             false;
%         #xmlel{name = Marker} ->
%             lists:member(Marker, Markers)
%     end.
%%% new implementation
has_chat_marker(Packet, Markers) ->
    case xmpp:get_subtag(Packet, #chatmarker{}) of
        false ->
            false;
        #xmlel{name = Marker} ->
            lists:member(Marker, Markers)
    end.
%%% I think this works
% has_chat_markers(Packet, Markers) ->
%     lists:member(fun(Marker) -> 
%         has_chat_markers1(Marker) end, Markers).

% -spec has_chat_markers1(Name :: binary()) -> binary().
% has_chat_markers1(Name) ->
%     case fxml:get_subtag_with_xmlns(Packet, Name, ?NS_CHAT_MARKERS) of
%         false ->
%             false;
%         #xmlel{name = Name} ->
%             Name
%     end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec maybe_write_to_inbox(Host, User, Remote, Packet, WriteF) -> ok | {ok, integer()} when
    Hst :: host(),
    User :: jid:jid(),
    Remote :: jid:jid(),    
    Packet :: xmlel(),
    %% WriteF is write_to_receiver_inbox/4 or write_to_sender_inbox/4
    WriteF :: fun().
maybe_write_to_inbox(Host, User, Remote, Packet, WriteF) ->
    case mod_inbox_utils:has_chat_marker(Packet) of
        true ->
            ok;
        false ->
            %%% TODO: jid:to_binary() removed, CG
            FromBin = jid:to_string(User),
            Packet2 = mod_inbox_utils:fill_from_attr(Packet, FromBin),
            WriteF(Host, User, Remote, Packet2)
    end.

% Get id from an element
-spec get_msg_id(Msg :: xmlel()) -> binary().
% get_msg_id(#xmlel{name = <<"message">>} = Msg) ->
%     exml_query:attr(Msg, <<"id">>, <<>>).
%%% new implementation
get_msg_id(#xmlel{name = <<"message">>} = Msg) ->
    xmpp:get_id(Msg)

-spec fill_from_attr(Msg :: xmlel(), FromBin :: binary()) -> xmlel().
% fill_from_attr(Msg = #xmlel{attrs = Attrs}, FromBin) ->
%     case exml_query:attr(Msg, <<"from">>, undefined) of
%         undefined ->
%             Msg#xmlel{attrs = [{<<"from">>, FromBin} | Attrs]};
%         _ ->
%             Msg
%     end.
%%% new implementation
fill_from_attr(Msg = #xmlel{attrs = Attrs}, FromBin) ->
    case xmpp:get_from(Msg) of
        undefined ->
            Msg#xmlel{attrs = [{<<"from">>, FromBin} | Attrs]};
        _ ->
            Msg
    end.


-spec wrapper_id() -> id().
wrapper_id() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

-spec get_option_write_aff_changes(Host :: host()) -> boolean().
get_option_write_aff_changes(Host) ->
    gen_mod:get_module_opt(Host, mod_inbox, aff_changes, true).

-spec get_option_remove_on_kicked(Host :: host()) -> boolean().
get_option_remove_on_kicked(Host) ->
    gen_mod:get_module_opt(Host, mod_inbox, remove_on_kicked, true).

-spec reset_marker_to_bin(atom()) -> marker().
reset_marker_to_bin(displayed) -> <<"displayed">>;
reset_marker_to_bin(acknowledged) -> <<"acknowledged">>;
reset_marker_to_bin(received) -> <<"received">>;
%%? TODO: search whats throw in erlang or if is defined in one module outside it
reset_marker_to_bin(Unknown) -> throw({unknown_marker, Unknown}).

get_inbox_unread(User, Server, InterlocutorJID) ->
    mod_inbox_backend:get_inbox_unread(User, Server, InterlocutorJID).

all_chat_markers() ->
    [<<"received">>, <<"displayed">>, <<"acknowledged">>].
