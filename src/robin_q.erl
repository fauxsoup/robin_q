-module(robin_q).

-export([
        new/1,
        next/1,
        set/2
    ]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new(L) when is_list(L) ->
    new(list_to_tuple(L));
new(T) when is_tuple(T) ->
    do_new(T);
new(_) ->
    error(badarg).

do_new(_) ->
    ?nif_stub.

next(_Ref) ->
    ?nif_stub.

set(Q, L) when is_list(L) ->
    set(Q, list_to_tuple(L));
set(Q, T) when is_tuple(T) ->
    do_set(Q, T);
set(_, _) ->
    error(badarg).

do_set(_, _) ->
    ?nif_stub.
