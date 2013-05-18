Definitions.

EOL = [\r\n]
WS = [\s\n\t\r\v\f]
WS_NONL = [\s\t\r\v\f]
UNPRINTABLE = [\x00-\x19]
DIGIT = [0-9]
OCTAL_DIGIT = [0-7]
HEX_DIGIT = [0-9a-fA-F]
LETTER = [a-zA-Z_]
ALNUM = [a-zA-Z0-9_]
ESCAPE = [abfnrtv\\\?\'\"]
SYMBOL = ([=,\:;{}\[\]()./-]|\+)
SIGN = (-|\+)
FLOAT = [fF]

Rules.

{UNPRINTABLE}+                                             : skip_token.  %% remove unprintables
{WS}+                                                      : skip_token.  %% remove whitespace
#.*({EOL}+|)                                               : skip_token.  %% sh-style comments
//.*({EOL}+|)                                              : skip_token.  %% C style single-line comments
/\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/                  : skip_token.  %% C-style block comments
{LETTER}{ALNUM}*                                           : {token, select_id_type(TokenChars, TokenLine)}.
\"([^\"]|\\\")*\"                                          : {token, {string, TokenLine, unquote(TokenChars)}}.
\'([^\']|\\\')*\'                                          : {token, {string, TokenLine, unquote(TokenChars)}}.
{SIGN}?(inf|nan)                                           : {token, {float, TokenLine, special_float(TokenChars)}}.
{SIGN}?{DIGIT}+{FLOAT}                                     : {token, {float, TokenLine, float(list_to_integer(trim_float_sentinel(TokenChars)))}}.
{SIGN}?{DIGIT}+([eE]{SIGN}?{DIGIT}+){FLOAT}?               : {token, {float, TokenLine, parse_exp_number(TokenChars)}}.
{SIGN}?{DIGIT}+\.{DIGIT}+?([eE]{SIGN}?{DIGIT}+)?{FLOAT}?   : {token, {float, TokenLine, parse_float(TokenChars)}}.
{SIGN}?0x{HEX_DIGIT}+                                      : {token, {integer, TokenLine, hex_to_int(TokenChars)}}.
{SIGN}?0{OCTAL_DIGIT}+                                     : {token, {integer, TokenLine, oct_to_int(TokenChars)}}.
(0|{SIGN}?[1-9]{DIGIT}*)                                   : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{SYMBOL}                                                   : {token, {list_to_atom(TokenChars), TokenLine}}.

Erlang code.

%% Field types
-define(TYPE, ["double", "float", "int32", "int64", "uint32",
               "uint64", "sint32", "sint64", "fixed32",
               "fixed64", "sfixed32", "sfixed64", "bool",
               "string", "bytes"]).

%% Field rules
-define(RULE, ["required", "optional", "repeated"]).

%% Other keywords
-define(KEYWORD, ["message", "enum", "service", "extend", "option", "import",
                  "package", "public", "extensions", "to", "max", "rpc",
                  "returns", "group"]).

-define(OCT(C), C >= $0, C =< $7).
-define(HEX(C), C >= $0 andalso C =< $9 orelse
                C >= $A andalso C =< $F orelse
                C >= $a andalso C =< $f).

-export([file/1, main/1]).

%% Flip through the reserved words and create tokens of the proper type.
select_id_type(T, Line) ->
    select_id_type(T, Line, [{type, ?TYPE},
                             {rule, ?RULE},
                             {keyword, ?KEYWORD}]).

%% When none of the keywords match, it's a regular identifier
select_id_type(T, Line, []) ->
    {identifier, Line, T};
%% special-casing other keywords so they come out as terminals
select_id_type(T, Line, [{keyword, K}|Rest]) ->
    case lists:member(T, K) of
        true ->
            {list_to_atom(T), Line};
        false ->
            select_id_type(T, Line, Rest)
    end;
select_id_type(T, Line, [{NT, Toks}|Rest]) ->
    case lists:member(T, Toks) of
        true ->
            {NT, Line, list_to_atom(T)};
        false ->
            select_id_type(T, Line, Rest)
    end.

unquote(Str) ->
    %% Quote is at beginning and end of the string, strip them off.
    Stripped = lists:sublist(Str, 2, length(Str)-2),
    unescape(Stripped, []).

unescape([], Acc) ->
    lists:reverse(Acc);
unescape([$\\|Rest0], Acc) ->
    {Char, Rest} = unescape_char(Rest0),
    unescape(Rest, [Char|Acc]);
unescape([C|Rest], Acc) ->
    unescape(Rest, [C|Acc]).

unescape_char([O1,O2,O3|T]) when ?OCT(O1), ?OCT(O2), ?OCT(O3) ->
    {(O1 * 8 + O2)*8 + O3 - 73*$0, T};
unescape_char([O1, O2|T]) when ?OCT(O1), ?OCT(O2) ->
    {(O1 * 8) + O2 - 9*$0, T};
unescape_char([$x,H0,H1|T]) when ?HEX(H0), ?HEX(H1)->
    {list_to_integer([H0,H1], 16), T};
unescape_char([$n|T]) -> {$\n, T};
unescape_char([$r|T]) -> {$\r, T};
unescape_char([$t|T]) -> {$\t, T};
unescape_char([$v|T]) -> {$\v, T};
unescape_char([$b|T]) -> {$\b, T};
unescape_char([$f|T]) -> {$\f, T};
unescape_char([$e|T]) -> {$\e, T};
unescape_char([$s|T]) -> {$\s, T};
unescape_char([$d|T]) -> {$\d, T};
unescape_char([C|T]) ->  {C, T}.

hex_to_int([$+|Hex]) ->
    hex_to_int(Hex);
hex_to_int([$-|Hex]) ->
    hex_to_int(Hex);
hex_to_int("0x"++Hex) ->
    list_to_integer(Hex, 16).

oct_to_int([$+|Oct]) ->
    oct_to_int(Oct);
oct_to_int([$-|Oct]) ->
    oct_to_int(Oct);
oct_to_int([$0|Oct]) ->
    list_to_integer(Oct, 8).

-spec parse_exp_number(string()) -> float().
parse_exp_number(Chars) ->
    %% 52e5 is used in one google example
    case lists:member($., Chars) of
        true ->
            parse_float(Chars);
        false ->
            parse_float(re:replace(Chars, "[eE]", ".0&", [{return, list}]))
    end.

special_float("-inf") -> '-infinity';
special_float("+inf") -> infinity;
special_float("inf") -> infinity;
special_float("-nan") -> nan;
special_float("+nan") -> nan;
special_float("nan") -> nan.

parse_float(Float) ->
    list_to_float(trim_float_sentinel(Float)).

trim_float_sentinel(L) ->
    lists:reverse(trim_float_sentinel0(lists:reverse(L))).

trim_float_sentinel0([$f|T]) -> T;
trim_float_sentinel0([$F|T]) -> T;
trim_float_sentinel0(L) -> L.

file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    string(unicode:characters_to_list(Bin)).

main([Filename]) ->
    io:format("~p~n", [file(Filename)]).
