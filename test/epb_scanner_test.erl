-module(epb_scanner_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-include("fixtures.hrl").

scanner_test_() ->
    [
     {File ++ " should scan",
      fun() ->
              {ok, Bin} = file:read_file(File),
              String = unicode:characters_to_list(Bin),
              ?assertMatch({ok, _Tokens, _Line},
                           epb_scanner:string(String))
      end}
     || File <- ?FIXTURES ].

-endif.
