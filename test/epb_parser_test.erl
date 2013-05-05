-module(epb_parser_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("fixtures.hrl").

parser_test_() ->
    [
     {File ++ " should parse",
      fun() ->
              {ok, Bin} = file:read_file(File),
              String = unicode:characters_to_list(Bin),
              {ok, Tokens, _} = epb_scanner:string(String),
              ?assertMatch({ok, AST}, epb_parser:parse(Tokens))
      end}
     || File <- ?FIXTURES ].

-endif.
