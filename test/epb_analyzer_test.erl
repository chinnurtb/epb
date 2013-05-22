%% Copyright (c) 2013
%% Basho Technologies, Inc. <dev@basho.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the \"Software\"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%% @doc Semantic analysis tests
-module(epb_analyzer_test).
-ifdef(TEST).
-define(A(Str), epb_analyzer:string("test.proto", Str)).
-include("epb_analysis.hrl").
-include_lib("eunit/include/eunit.hrl").

name_test() ->
    {ok, P} = epb_analyzer:string("name_test.proto", ""),
    ?assertEqual("name_test.proto", P#proto.name).

package_def_test_() ->
    [{"no package def is valid",
      ?_assertMatch({ok, #proto{package=undefined}},
                    ?A("message Foo {}\n"))
      },
     {"single package def is valid",
      ?_assertMatch({ok, #proto{package=["foo", "bar", "baz"]}},
                    ?A("package foo.bar.baz;\nmessage Foo {}\n"))
     },
     {"multiple package definitions is error",
      ?_assertMatch({error, {multiple_packages, L}},
                    ?A("package foo.bar.baz;\nmessage Foo {}\npackage quine;"))
      }].
      
-endif.
