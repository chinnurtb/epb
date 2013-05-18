-module(epb_grammar_eqc).
-ifdef(EQC).

-compile([{parse_transform, eqc_grammar}, export_all]).
-eqc_grammar({yecc_tokens, "../src/epb_parser.yrl"}).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LITERAL(Atom), Atom() -> {Atom, nat()}).

-define(TYPE, ["double", "float", "int32", "int64", "uint32",
               "uint64", "sint32", "sint64", "fixed32",
               "fixed64", "sfixed32", "sfixed64", "bool",
               "string", "bytes"]).

-define(RULE, ["required", "optional", "repeated"]).

%%===============================
%% PROPERTIES
%%===============================
prop_parse() ->
    ?FORALL(SymbolicExpr,'protobuf'(),
 	    begin
 	        Tokens = eqc_grammar:eval(SymbolicExpr),
                ?WHENFAIL(begin 
                              ?debugFmt("Tokens: ~p~n", [Tokens])
                          end,
                          case epb_parser:parse(Tokens) of
                              {ok, _SyntaxTree} -> true;
                              {error,_} -> false
                          end)
 	    end).

%%===============================
%% GENERATORS
%%===============================
?LITERAL(';').
?LITERAL('=').
?LITERAL('{').
?LITERAL('}').
?LITERAL('[').
?LITERAL(']').
?LITERAL(')').
?LITERAL('(').
?LITERAL('.').
?LITERAL(',').
?LITERAL(':').
?LITERAL('message').
?LITERAL('enum').
?LITERAL('service').
?LITERAL('extend').
?LITERAL('option').
?LITERAL('import').
?LITERAL('package').
?LITERAL('public').
?LITERAL('extensions').
?LITERAL('to').
?LITERAL('max').
?LITERAL('rpc').
?LITERAL('returns').
?LITERAL('group').

identifier() ->
    {identifier, nat(), word()}.

type() ->
    {type, nat(), ?LET(T, elements(?TYPE), list_to_atom(T))}.

rule() ->
    {rule, nat(), ?LET(T, elements(?RULE), list_to_atom(T))}.

string() ->
    {string, nat(), list(letter())}.

float() ->
    {float, nat(), real()}.

integer() ->
    {integer, nat(), int()}.

word() ->
    ?LET({L, Tail}, {letter(), list(alnum())}, [L|Tail]).

letter() ->
    oneof([choose($A,$Z), choose($a,$z)]).

alnum() ->
    oneof([letter(), choose($0, $9), $_]).

-endif.
