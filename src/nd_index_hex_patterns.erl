%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Create hexadecimal string patterns==
%%% Patterns that are usable in the trie to match a hexadecimal prefix
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at gmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2014-2017 Michael Truog
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
    if
        GroupsCount =< Count ->
            ok;
        true ->
            erlang:exit(badarg)
    end,
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
