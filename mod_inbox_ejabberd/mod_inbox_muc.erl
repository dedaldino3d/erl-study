%%%-------------------------------------------------------------------
%%% @author dedaldino3D
%%% @copyright (C) 2020, dedaldino3D
%%% @doc
%%%
%%% @end
%%% Created : 6.07.2018
%%%-------------------------------------------------------------------
-module(mod_inbox_muc).
-author("dedaldino3D").
% -include_lib("xmpp/include/xmpp.hrl").
-include("xmpp.hrl").


-export([update_inbox_for_muc/1, start/1, stop/1]).

%% User jid example is "alice@localhost"
-type user_jid() :: jid:jid().
%% Receiver's host in lowercase
-type receiver_host() :: jid:lserver().
-type receiver_bare_user_jid() :: user_jid().
-type room_bare_jid() :: jid:jid().
-type packet() :: xmlel().
% ? from mod_muc in MongooseIm
-type affiliation() :: admin | owner | member | outcast | none.
% ? Types from Mongoo mod_muc_room
-type users_map() :: #{{user(), server(), resource()} => user()}.
-type users_pairs() :: [{{user(), server(), resource()}, user()}].
-type affiliations_map() :: [{{user(), server(), resource()}, mod_muc:affiliation()}].


-type update_inbox_for_muc_payload() :: #{
        room_jid := jid:jid(),
        from_jid := jid:jid(),
        from_room_jid := jid:jid(),
        packet := xmlel(),
        affiliations_map := affiliations_map()
        }.

-export_type([update_inbox_for_muc_payload/0]).


start(Host) ->
    ejabberd_hooks:add(update_inbox_for_muc, Host, ?MODULE, update_inbox_for_muc, 90),
    % TODO check ooptions: if system messages stored ->
    % add hook handler for system messages on hook ie. invitation_sent
    ok.

stop(Host) ->
    ejabberd_hooks:delete(update_inbox_for_muc, Host, ?MODULE, update_inbox_for_muc, 90),
    ok.

% ? from mongoose_hooks
% *** THIS IS A HOOK
-spec update_inbox_for_muc(Server, Info) -> Result when
    Server :: jid:server(),
    Info :: update_inbox_for_muc_payload(),
    Result :: update_inbox_for_muc_payload().
update_inbox_for_muc(Server, Info) ->
    ejabberd_hooks:run_fold(update_inbox_for_muc, Server, Info, []).

% ? Types from Mongoo mod_muc_room, original
% -spec run_update_inbox_for_muc_hook(jid:server(),
%                                     update_inbox_for_muc_payload()) -> ok.
% run_update_inbox_for_muc_hook(ServerHost, HookInfo) ->
%     % * ejabberd_hooks dont have update_inbox_for_muc, create it here
%     mongoose_hooks:update_inbox_for_muc(ServerHost, HookInfo),
%     ok.
% * Changed version
-spec run_update_inbox_for_muc_hook(jid:server(),
                                    update_inbox_for_muc_payload()) -> ok.
run_update_inbox_for_muc_hook(ServerHost, HookInfo) ->
    update_inbox_for_muc(ServerHost, HookInfo),
    ok.

% ! Call; run_update_inbox_for_muc_hook inside a hook 
% ! [muc_filter_message, ...]  find how messages in rooms are handled
% ! check mond_muc_room line 893 in mongoose

% ? from mongoose_hooks
% *** THIS IS A HOOK
%%% @doc The `inbox_unread_count' hook is called to get the number of unread messages in the inbox for a user.
%%? mongoose_acc:t() to any()
-spec inbox_unread_count(LServer, Acc, User) -> Result when
        LServer :: jid.lserver(),
        Acc :: any(),
        User :: jid:jid(),
        Result :: any().
inbox_unread_count(LServer, Acc, User) ->
    ejabberd_hooks:run_fold(inbox_unread_count, LServer, Acc, [User]).

% ? Remove or perform update for Acc (acumulators)
-spec update_inbox_for_muc(Acc) -> Acc when
      Acc :: update_inbox_for_muc_payload().
update_inbox_for_muc(
    #{room_jid := Room,
      from_jid := From,
      from_room_jid := FromRoomJid,
      packet := Packet,
      affiliations_map := AffsMap} = Acc) ->
    F = fun(AffLJID, Affiliation) ->
            case is_allowed_affiliation(Affiliation) of
                true ->
                    % To = jid:to_bare(jid:make(AffLJID)),
                    % ? Changed version
                    To = jid:remove_resource(jid:make(AffLJID)),
                    %% Guess direction based on user JIDs
                    Direction = direction(From, To),
                    Host = To#jid.lserver,
                    % ? Changed version. use xmpp:? or fxml:?
                    Packet2 = replace_from_to(FromRoomJid, To, Packet),
                    update_inbox_for_user(Direction, Host, Room, To, Packet2);
                false ->
                    ok
            end
        end,
    % ? changed version
    maps_foreach(F, AffsMap),
    Acc.


%%%% -----------------
%%%% replace_from_to from jlib in mongoose
%%%% -----------------
-spec replace_from_to_attrs(From :: binary(),
                            To :: binary() | undefined,
                            [{binary(), binary()}]) -> [{binary(), binary()}].
replace_from_to_attrs(From, To, Attrs) ->
    Attrs1 = lists:keydelete(<<"to">>, 1, Attrs),
    Attrs2 = lists:keydelete(<<"from">>, 1, Attrs1),
    Attrs3 = case To of
                 undefined -> Attrs2;
                 _ -> [{<<"to">>, To} | Attrs2]
             end,
    Attrs4 = [{<<"from">>, From} | Attrs3],
    Attrs4.

-spec replace_from_to(From :: {user(), server(), resource()}. | jid:jid(),
                      To :: {user(), server(), resource()}. | jid:jid(),
                      XE :: xmlel()) -> xmlel().
replace_from_to(From, To, XE = #xmlel{attrs = Attrs}) ->
    % TODO: original jid:to_string() do not replace jid:to_binary(), maybe will do not work
    NewAttrs = replace_from_to_attrs(jid:to_string(From),
                                     jid:to_string(To),
                                     Attrs),
    XE#xmlel{attrs = NewAttrs}.

%%%% -----------------
%%%% maps_foreach from mongoose_lib
%%%% -----------------
-spec maps_foreach(fun(), map()) -> ok.
maps_foreach(Fun, Map) when is_function(Fun, 1) ->
    maps:fold(fun(Key, Value, Acc) ->
                      Fun({Key, Value}), Acc
              end, ok, Map);
maps_foreach(Fun, Map) when is_function(Fun, 2) ->
    maps:fold(fun(Key, Value, Acc) ->
                      Fun(Key, Value), Acc
              end, ok, Map).

%%%% -----------------

% ? Changed version
% ! Do not work yet, check affiliations in mod_muc or other similar option
-spec is_allowed_affiliation(affiliation()) -> boolean().
is_allowed_affiliation(outcast) -> false;
is_allowed_affiliation(_)       -> true.

-spec update_inbox_for_user(Direction, Host, Room, To, Packet) -> term() when
      Direction :: incoming | outgoing,
      Host :: receiver_host(),
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
update_inbox_for_user(Direction, Host, Room, To, Packet) ->
    case {is_local_xmpp_host(Host), Direction} of
        {true, outgoing} ->
            handle_outgoing_message(Host, Room, To, Packet);
        {true, incoming} ->
            handle_incoming_message(Host, Room, To, Packet);
        _ ->
            %% We ignore inbox for users on the remote (s2s) hosts
            %% We ignore inbox for components (also known as services or bots)
            ok
    end.

-spec direction(From :: user_jid(), To :: user_jid()) -> incoming | outgoing.
direction(From, To) ->
    case are_bare_equal(From, To) of
        true -> outgoing;
        false -> incoming
    end.

%% @doc Returns true if `are_equal(to_bare(A), to_bare(B))'
%%% ? Move this implementation to other module like jid
-spec are_bare_equal(jid() | ljid(), jid() | ljid()) -> boolean().
are_bare_equal(#jid{luser = LUser, lserver = LServer},
               #jid{luser = LUser, lserver = LServer}) ->
    true;
are_bare_equal(#jid{luser = LUser, lserver = LServer}, {LUser, LServer, _}) ->
    true;
are_bare_equal({LUser, LServer, _}, #jid{luser = LUser, lserver = LServer}) ->
    true;
are_bare_equal({LUser, LServer, _}, {LUser, LServer, _}) ->
    true;
are_bare_equal(_, _) ->
    false.

%% Sender and receiver is the same user
-spec handle_outgoing_message(Host, Room, To, Packet) -> term() when
      Host :: receiver_host(),
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
handle_outgoing_message(Host, Room, To, Packet) ->
    maybe_reset_unread_count(Host, To, Room, Packet),
    maybe_write_to_inbox(Host, To, Room, Packet, fun write_to_sender_inbox/4).

-spec handle_incoming_message(Host, Room, To, Packet) -> term() when
      Host :: receiver_host(),
      Room :: room_bare_jid(),
      To :: receiver_bare_user_jid(),
      Packet :: packet().
handle_incoming_message(Host, Room, To, Packet) ->
    maybe_write_to_inbox(Host, Room, To, Packet, fun write_to_receiver_inbox/4).

maybe_reset_unread_count(Host, User, Room, Packet) ->
    mod_inbox_utils:maybe_reset_unread_count(Host, User, Room, Packet).

maybe_write_to_inbox(Host, User, Remote, Packet, WriteF) ->
    mod_inbox_utils:maybe_write_to_inbox(Host, User, Remote, Packet, WriteF).

write_to_sender_inbox(Server, User, Remote, Packet) ->
    mod_inbox_utils:write_to_sender_inbox(Server, User, Remote, Packet).

write_to_receiver_inbox(Server, User, Remote, Packet) ->
    mod_inbox_utils:write_to_receiver_inbox(Server, User, Remote, Packet).

%% @doc Check, that the host is served by ejabberd.
%% A local host can be used to fire hooks or write into database on this node.
-spec is_local_xmpp_host(jid:lserver()) -> boolean().
is_local_xmpp_host(LServer) ->
    lists:member(LServer, ejabberd_config:get_myhosts()).