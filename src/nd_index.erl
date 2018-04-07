%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Increment N-d indexes with a single index==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2010-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2010-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(nd_index).
-author('mjtruog at protonmail dot com').

%% external interface
-export([new/2,
         new/3,
         set/2,
         count/1,
         clear/1,
         empty/1,
         value/1,
         min/1,
         max/1,
         increment/1,
         increment/2,
         increment_seq/2]).

%% store state as tuples with the
%% least significant on the left and most significant on the right
-record(nd_index_state,
    {
        min,
        max,
        value
    }).
-type state() :: #nd_index_state{}.
-export_type([state/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new N dimensional index, set to Min.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Min :: tuple(),
          Max :: tuple()) -> #nd_index_state{}.

new(Min, Max) when tuple_size(Min) == tuple_size(Max), Min =< Max ->
    #nd_index_state{min = Min, max = Max, value = Min}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new N dimensional index, set to Value.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Value :: tuple(),
          Min :: tuple(),
          Max :: tuple()) -> #nd_index_state{}.

new(Value, Min, Max)
    when tuple_size(Min) == tuple_size(Max), Min =< Max,
         tuple_size(Min) == tuple_size(Value), Min =< Value, Value =< Max ->
    #nd_index_state{min = Min, max = Max, value = Value}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the current N dimensional index value.===
%% @end
%%-------------------------------------------------------------------------

-spec set(Value :: tuple(),
          State :: #nd_index_state{}) -> #nd_index_state{}.

set(Value, #nd_index_state{min = Min, max = Max} = State)
    when tuple_size(Min) == tuple_size(Value), Min =< Value, Value =< Max ->
    State#nd_index_state{value = Value}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a count of how many iterations remain.===
%% @end
%%-------------------------------------------------------------------------

-spec count(#nd_index_state{}) -> non_neg_integer().

count(#nd_index_state{min = Min, max = Max, value = Value}) ->
    lists:foldl(fun(Index0, Count0) ->
        Multiplier = if
            Index0 > 1 ->
                lists:foldl(fun(Index1, Count1) ->
                    (erlang:element(Index1, Max) -
                     erlang:element(Index1, Min) + 1) * Count1
                end, 1, lists:seq(1, Index0 - 1));
            true ->
                1
        end,
        (erlang:element(Index0, Max) -
         erlang:element(Index0, Value)) * Multiplier + Count0
    end, 0, lists:seq(1, tuple_size(Value))).

%%-------------------------------------------------------------------------
%% @doc
%% ===Reset the current N dimensional index value to min.===
%% @end
%%-------------------------------------------------------------------------

-spec clear(State :: #nd_index_state{}) -> #nd_index_state{}.

clear(#nd_index_state{min = Min} = State) ->
    State#nd_index_state{value = Min}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if no iterations remain.===
%% @end
%%-------------------------------------------------------------------------

-spec empty(#nd_index_state{}) -> boolean().

empty(#nd_index_state{max = Max, value = Max}) ->
    true;

empty(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the current N dimensional index value.===
%% @end
%%-------------------------------------------------------------------------

-spec value(#nd_index_state{}) -> tuple().

value(#nd_index_state{value = Value}) ->
    Value.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the minimum N dimensional index value.===
%% @end
%%-------------------------------------------------------------------------

-spec min(#nd_index_state{}) -> tuple().

min(#nd_index_state{min = Min}) ->
    Min.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the maximum N dimensional index value.===
%% @end
%%-------------------------------------------------------------------------

-spec max(#nd_index_state{}) -> tuple().

max(#nd_index_state{max = Max}) ->
    Max.

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment the N dimensional index value by 1 and return the new state.===
%% @end
%%-------------------------------------------------------------------------

-spec increment(#nd_index_state{}) ->
    #nd_index_state{} |
    'false'.

increment(State) ->
    increment(1, State).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Increment the N dimensional index value by a single count and return the new state.===
%% @end
%%-------------------------------------------------------------------------

-spec increment(Count :: non_neg_integer(),
                #nd_index_state{}) ->
    #nd_index_state{} |
    'false'.

increment(Count, #nd_index_state{min = Min, max = Max, value = Value})
    when is_integer(Count), Count > 0 ->
    increment(Count, 1, Value, Min, Max).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Increment the N dimensional index value by a single count and return the sequence result with the new state.===
%% all sequences returned are inclusive without overlap
%% @end
%%-------------------------------------------------------------------------

-spec increment_seq(Count :: non_neg_integer(),
                    #nd_index_state{}) ->
    {{tuple(), tuple()}, #nd_index_state{}}.

increment_seq(Count, #nd_index_state{min = Min, max = Max, value = Value0})
    when is_integer(Count), Count > 0 ->
    case increment(Count - 1, 1, Value0, Min, Max) of
        false ->
            {{Value0, Max},
             #nd_index_state{min = Min, max = Max, value = Max}};
        #nd_index_state{value = Max} = State0 ->
            {{Value0, Max}, State0};
        #nd_index_state{value = Value1} = State0 ->
            case increment(1, 1, Value1, Min, Max) of
                false ->
                    {{Value0, Value1}, State0};
                #nd_index_state{} = State1 ->
                    {{Value0, Value1}, State1}
            end
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

increment(0, _, Value, Min, Max) ->
    #nd_index_state{min = Min, max = Max, value = Value};

increment(Count0, Index, Value, Min, Max) when Index =< tuple_size(Value) ->
    Min0 = erlang:element(Index, Min),
    Value0 = erlang:element(Index, Value),
    Span = erlang:element(Index, Max) - Min0 + 1,
    Increment = Count0 + Value0 - Min0,
    Value1 = (Increment rem Span) + Min0,
    Count1 = Increment div Span,
    increment(Count1, Index + 1,
              erlang:setelement(Index, Value, Value1), Min, Max);

increment(_, _, _, _, _) ->
    false.

