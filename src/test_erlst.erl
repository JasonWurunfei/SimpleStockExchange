-module(test_erlst).

-export([test_all/0, test_everything/0]).
-export([]). % Remember to export the other functions from Q2.2

-import(test_api, [test_api_all/0]).
-import(test_prop, [test_prop_all/0]).

% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_erlst.

test_all() ->
  io:format("------ Running property tests ------~n"),
  test_prop_all(),
  io:format("------ Running API tests ------~n"),
  test_api_all().

test_everything() ->
  test_all().
