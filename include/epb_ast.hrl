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
%% @doc Abstract syntax tree header file
-record(package, {name, line}).
-record(message, {name, decls=[], line}).
-record(extend, {name, decls=[], line}).
-record(enum, {name, decls=[], line}).
-record(enumval, {name, value, options, line}).
-record(service, {name, decls=[], line}).
-record(option, {key, value, line}).
-record(import, {file, public=false, line}).
-record(group, {id, name, decls, rule, line}).
-record(field, {id, name, type, rule, options=[], line}).
-record(rpc, {call, request, response, options=[], line}).
-record(extensions, {min, max, line}).
-record(id, {names, line}).
-record(message_literal, {fields, extend, line}).
-record(field_literal, {name, value, line}).

-type ast_node() :: #package{} | #message{} | #extend{} |
                    #enum{} | #enumval{} | #service{} |
                    #option{} | #import{} | #group{} |
                    #field{} | #rpc{} | #extensions{} |
                    #id{} | #message_literal{} | #field_literal{}.

-spec ast_line(ast_node()) -> pos_integer().
ast_line(#package{line=L}) -> L;
ast_line(#message{line=L}) -> L;
ast_line(#extend{line=L}) -> L;
ast_line(#enum{line=L}) -> L;
ast_line(#enumval{line=L}) -> L;
ast_line(#service{line=L}) -> L;
ast_line(#option{line=L}) -> L;
ast_line(#import{line=L}) -> L;
ast_line(#group{line=L}) -> L;
ast_line(#field{line=L}) -> L;
ast_line(#rpc{line=L}) -> L;
ast_line(#extensions{line=L}) -> L;
ast_line(#id{line=L}) -> L;
ast_line(#message_literal{line=L}) -> L;
ast_line(#field_literal{line=L}) -> L.

-compile([{nowarn_unused_function, [{ast_line,1}]}]).
