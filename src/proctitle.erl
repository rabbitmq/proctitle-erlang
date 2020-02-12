%% @author The RabbitMQ team
%% @copyright 2020 Pivotal Software, Inc.
%%
%% @doc
%% This module provides functions to manipulate the system process title
%% of the Erlang VM.
%%
%% It allows to change the title which appears in the output of commands
%% such as `ps(1)' and `top(1)'.
%%
%% It relies on OS-specific native code and thus may not work
%% everywhere.

-module(proctitle).

-export([support_available/0,
         set/1,
         set/2,
         reset/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type proctitle() :: binary() | string() | atom().
%% The process title to set.

-type options() :: #{keep_executable => boolean()}.
%% A map of options to modify the behavior.

-spec support_available() -> boolean().
%% @doc
%% Returns if the native library was loaded or not.
%%
%% A user of this library should call this function first to determine
%% if other calls will work.
%%
%% @returns A boolean indicating if the native library was loaded or not.

support_available() ->
    try
        _ = proctitle_nif:module_info(),
        true
    catch
        _:_ ->
            false
    end.

-spec set(proctitle()) -> ok.
%% @doc
%% Sets the process title to `ProcTitle'.
%%
%% The name of the executable is kept before or after the given string
%% if the underlying system call permits it.
%%
%% @param ProcTitle the process title to set.

set(ProcTitle) ->
    set(ProcTitle, #{}).

-spec set(proctitle(), options()) -> ok.
%% @doc
%% Sets the process title to `ProcTitle'.
%%
%% The `keep_executable' option indicates if the name of the executable
%% should be kept kept before or after the given string. The result depends
%% on the underlying system call.
%%
%% @param ProcTitle the process title to set.
%% @param Options a map of options to modify the behavior.

set(ProcTitle, Options) when is_binary(ProcTitle) ->
    proctitle_nif:set(ProcTitle, Options);
set(ProcTitle, Options) when is_list(ProcTitle) ->
    set(list_to_binary(ProcTitle), Options);
set(ProcTitle, Options) when is_atom(ProcTitle) ->
    set(atom_to_list(ProcTitle), Options).

-spec reset() -> ok.
%% @doc
%% Resets the process title to the original command line.
%%
%% The ability to change the process title does not guaranty it is
%% possible to restore the original command line.

reset() ->
    proctitle_nif:reset().

-ifdef(TEST).
set_test() ->
    ?assertEqual(ok, set("testing set/1")),
    ?assertEqual(ok, set("testing set/2", #{})),
    ?assertEqual(ok, set("testing set/2", #{keep_executable => false})),
    ?assertEqual(ok, set("testing set/2", #{keep_executable => true})).

reset_test() ->
    ?assertEqual(ok, reset()).
-endif.
