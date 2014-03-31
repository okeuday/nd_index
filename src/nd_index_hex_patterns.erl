%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Create hexadecimal string patterns==
%%% Patterns that are usable in the trie to match a hexadecimal prefix
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(nd_index_hex_patterns).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([create/2]).

create(Length, GroupsCount)
    when is_integer(Length), Length > 1,
         is_integer(GroupsCount), GroupsCount >= 1 ->
    HexadecimalLength = Length - 1,
    Index = nd_index:new(erlang:make_tuple(HexadecimalLength, 0),
                         erlang:make_tuple(HexadecimalLength, 15)),
    Count = nd_index:count(Index) + 1,
    true = GroupsCount =< Count,
    GroupSize = ceil(Count / GroupsCount),
    GroupSizeAdjust = ((Count div GroupsCount) /= GroupSize),
    create_groups(GroupsCount, GroupSize, GroupSizeAdjust, Index).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

create_groups(GroupsCount, GroupSize, GroupSizeAdjust, Index) ->
    create_groups(GroupsCount, [], GroupSize, GroupSizeAdjust, Index).

create_groups(0, Groups, _, _, _) ->
    lists:reverse(Groups);
create_groups(I, Groups, GroupSize, GroupSizeAdjust, Index) ->
    {Group, NewIndex} = create_group(GroupSize, Index),
    NewI = I - 1,
    {NewGroupSize, NewGroupSizeAdjust} = if
        GroupSizeAdjust =:= true ->
            Count = nd_index:count(NewIndex) + 1,
            V = ceil(Count / NewI),
            {V, (Count div NewI) /= V};
        GroupSizeAdjust =:= false ->
            {GroupSize, false}
    end,
    create_groups(NewI, [Group | Groups],
                  NewGroupSize, NewGroupSizeAdjust, NewIndex).

create_group(GroupSize, Index) ->
    create_group(GroupSize, [], Index).

create_group(0, Group, Index) ->
    {lists:reverse(Group), Index};
create_group(I, Group, Index) ->
    Entry = create_group_string(erlang:tuple_to_list(nd_index:value(Index))),
    create_group(I - 1, [Entry | Group], nd_index:increment(Index)).

create_group_string(L) ->
    create_group_string(L, "*").
    
create_group_string([], Output) ->
    Output;
create_group_string([X | L], Output) ->
    create_group_string(L, [int_to_hex(X) | Output]).

-compile({inline, [{int_to_hex,1}]}).

int_to_hex(I) when 0 =< I, I =< 9 ->
    I + $0;
int_to_hex(I) when 10 =< I, I =< 15 ->
    (I - 10) + $a.

ceil(X) ->
    T = erlang:trunc(X),
    if
        X > T ->
            T + 1;
        true ->
            T
    end.
