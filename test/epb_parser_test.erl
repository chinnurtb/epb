-module(epb_parser_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("fixtures.hrl").

parser_passing_test_() ->
    [
     {File ++ " should parse",
      fun() ->
              {ok, Tokens, _} = epb_scanner:file(File),
              ?assertMatch({ok, _AST}, epb_parser:parse(Tokens))
      end}
     || File <- ?PASSING_FIXTURES ].

parser_failing_test_() ->
    [ {File ++ " should NOT parse",
       fun() ->
               {ok, Tokens, _} = epb_scanner:file(File),
               ?assertMatch({error, {_Line, epb_parser, _Msg}},
                            epb_parser:parse(Tokens))
       end}
      || File <- ?FAILING_FIXTURES ].

-endif.
