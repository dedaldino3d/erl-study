%%%-------------------------------------------------------------------
%%% @author dedaldino3D
%%% @copyright (C) 2020, dedaldino3D
%%% @doc
%%%
%%% @end
%%% Created : 2 Out 2020 8:14
%%%-------------------------------------------------------------------
-module(mod_inbox_one2one).
-author("dedaldino3D").
-include("mod_inbox.hrl").
% * Remove include
% -include("jlib.hrl").
% * Remove include
% -include("mongoose_ns.hrl").
% * For it
-include_lib("xmpp/include/xmpp.hrl").

-export([handle_outgoing_message/4, handle_incoming_message/4]).

% ? Changed exml:element()
-type packet() :: xmlel().

-spec handle_outgoing_message(Host :: jid:server(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet()) -> ok.
handle_outgoing_message(Host, User, Remote, Packet) ->
    maybe_reset_unread_count(Host, User, Remote, Packet),
    maybe_write_to_inbox(Host, User, Remote, Packet, fun write_to_sender_inbox/4).

-spec handle_incoming_message(Host :: jid:server(),
                              User :: jid:jid(),
                              Remote :: jid:jid(),
                              Packet :: packet()) -> ok | {ok, integer()}.
handle_incoming_message(Host, User, Remote, Packet) ->
    maybe_write_to_inbox(Host, User, Remote, Packet, fun write_to_receiver_inbox/4).

maybe_reset_unread_count(Host, User, Remote, Packet) ->
    mod_inbox_utils:maybe_reset_unread_count(Host, User, Remote, Packet).

maybe_write_to_inbox(Host, User, Remote, Packet, WriteF) ->
    mod_inbox_utils:maybe_write_to_inbox(Host, User, Remote, Packet, WriteF).

write_to_sender_inbox(Server, User, Remote, Packet) ->
    mod_inbox_utils:write_to_sender_inbox(Server, User, Remote, Packet).

write_to_receiver_inbox(Server, User, Remote, Packet) ->
    mod_inbox_utils:write_to_receiver_inbox(Server, User, Remote, Packet).
