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
%% @doc Semantic analyzer for Protocol Buffers definitions.
-module(epb_analyzer).
-export([string/2, file/1, analyze/2]).

-include("epb_ast.hrl").
-include("epb_analysis.hrl").

-define(PASSES,
        [
         fun check_package/2,
         fun check_imports/2
        ]).

file(Filename) ->
    file(Filename, []).

file(Filename, Env) ->
    case epb_parser:file(Filename) of
        {ok, AST} ->
            analyze(Filename, AST, Env);
        Else -> Else
    end.

string(Name, Str) ->
    case epb_parser:string(Str) of
        {ok, AST} ->
            analyze(Name, AST);
        Else -> Else
    end.

%%-------------------------
%% Analysis high-level
%%-------------------------

%% @doc Invokes semantic analysis on the named file with the given
%% AST.
-spec analyze(string(), [ast_node()]) -> {ok, #proto{}} | {error, any()}.
analyze(Name, AST) ->
    analyze(Name, AST, []).

analyze(Name, AST, Env) ->
    analyze_passes(#proto{name=Name, env=Env}, AST, ?PASSES).


%% @doc Folds through the analyzer passes with early abort.
%% @private
analyze_passes(P, _AST, []) ->
    {ok, P};
analyze_passes(P, AST, [Pass|Rest]) ->
    case Pass(P, AST) of
        {ok, #proto{}=P1} ->
            analyze_passes(P1, AST, Rest);
        {error, _}=Error ->
            Error
    end.


%%-------------------------
%% Analysis checks
%%-------------------------

%% @doc Check and assignment for package declaration.
check_package(P, AST) ->
    case find_packages(AST) of
        [] -> {ok, P};
        [#package{name=#id{names=DeepName}}] ->
            {ok, P#proto{package=DeepName}};
        Packages ->
            {error, {multiple_packages, [Packages]}}
    end.

check_imports(#proto{env=Env}=P, AST) ->
    case epb_import:resolve(find_imports(AST), Env) of
        {ok, Imports} ->
            {ok, P#proto{imports=Imports}};
        {error, Reason} ->
            {error, {import_error, Reason}}
    end.

%%-------------------------
%% Utility functions
%%-------------------------

%% @doc Finds package declarations in the AST.
find_packages(AST) ->
    [ Pkg || Pkg <- AST, is_record(Pkg, package) ].

%% @doc Finds import declarations in the AST.
find_imports(AST) ->
    [ I || I <- AST, is_record(I, import) ].
