%% Copyright (c) 2013
%% Basho Technologies, Inc. <dev@basho.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc Benchmarking escript, based on Google's ProtoBench.java
-module(epb_bench).
-export([main/1]).

-define(MIN_SAMPLE_TIME_US, 2 * 1000 * 1000).
-define(TARGET_TIME_US, 30 * 1000 * 1000).
-define(F(S,A), io:format(S,A)).

main([Descriptor, InputFile]) ->
    case catch run(Descriptor, InputFile) of
        ok ->
            halt(0);
        _ ->
            halt(1)
    end;
main(_) ->
    ?F("Usage: ~s <descriptor type name> <input data>~n"
       "The descriptor type name is the fully-qualified message name,~n"
       "e.g. benchmark.Message1~n",
       [escript:script_name()]).

run(Descriptor, InputFile) ->
    ?F("Benchmarking ~s with file ~s", [Descriptor, InputFile]),
    {Module, MsgName} = find_message(Descriptor),
    {ok, Bin} = file:read_file(InputFile),
    InputSize = size(Bin),
    Msg = Module:decode(MsgName, Bin),

    benchmark("Serialize to iolist", InputSize, 
              fun() ->
                      Module:encode(MsgName, Msg)
              end),
    benchmark("Serialize to binary", InputSize, 
              fun() ->
                      iolist_to_binary(Module:encode(MsgName, Msg))
              end),
    benchmark("Serialize to /dev/null", InputSize, 
              fun() ->
                      file:write_file("/dev/null", Module:encode(MsgName, Msg))
              end),
    benchmark("Deserialize from binary", InputSize,
              fun() ->
                      Module:decode(MsgName, Bin)
              end),
    ok.

benchmark(Name, Size, Fun) ->
    %% Java benchmark runs the action 100 times to invoke JIT. This is
    %% pointless in Erlang because there is no JIT.
    %% [ Fun() || _ <- lists:seq(1,100) ],
    
    %% Run it progressively more times until we've got a reasonable sample
    {Iterations, Elapsed} = execute_until_significant(Fun, 1, execute_times(Fun, 1)),
    
    %% Upscale the sample to the target time.
    NewIterations = trunc((?TARGET_TIME_US / Elapsed) * Iterations),
    NewElapsed = execute_times(Fun, NewIterations),
    ?F("~s: ~B iterations in ~0.3fs; ~0.3fMB/s",
       [Name, NewIterations, (NewElapsed / 1000 / 1000),
        (NewIterations * Size) / (NewElapsed * 1024 * 1024 / 1000 / 1000)]).

execute_times(Fun, I) ->
    L = lists:seq(1, I),
    {Time, _} = timer:tc(fun() -> 
                                 [ Fun() || _ <- L ]
                         end),
    Time.

execute_until_significant(_Fun, Iters, Elapsed) when Elapsed >= ?MIN_SAMPLE_TIME_US ->
    {Iters, Elapsed};
execute_until_significant(Fun, Iters, _Elapsed) ->
    execute_until_significant(Fun, Iters*2, execute_times(Fun, Iters*2)).

find_message(Descriptor) ->
    %% TODO: Use some sweet metaprogramming to find the message name.
    %% For now we'll just assume module.message and convert them to atoms.
    [SModule, SMessage] = re:split(Descriptor, "\\.", [{return, list}]),
    {list_to_atom(SModule), list_to_atom(SMessage)}.
