-module(proctitle_nif).

-export([set/2,
         reset/0]).

-include_lib("host_triple/include/code_loading.hrl").

-on_load(on_load/0).
on_load() ->
    ?load_nif_from_host_triple_dir(proctitle, atom_to_list(?MODULE), 0).

set(_, _) ->
    erlang:nif_error({not_loaded, ?MODULE}).

reset() ->
    erlang:nif_error({not_loaded, ?MODULE}).
