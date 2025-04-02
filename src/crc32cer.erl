
-module(crc32cer).

-export([nif/1,
         nif/2,
         nif_d/1,
         nif_d/2
        ]).

-on_load(init/0).

-spec init() -> ok.
init() ->
  _ = erlang:load_nif(so_path(), 0),
  ok.

-spec nif(iodata()) -> non_neg_integer().
nif(IoData) ->
  nif(0, IoData).

-spec nif(integer(), iodata()) -> non_neg_integer().
nif(_Acc, _IoData) ->
  erlang:nif_error({crc32cer_nif_not_loaded, so_path()}).

-spec nif_d(iodata()) -> non_neg_integer().
nif_d(IoData) ->
  nif(0, IoData).

-spec nif_d(integer(), iodata()) -> non_neg_integer().
nif_d(_Acc, _IoData) ->
  erlang:nif_error({crc32cer_nif_not_loaded, so_path()}).

-spec so_path() -> string().
so_path() ->
  filename:join([get_nif_bin_dir(), "libcrc32cer_nif"]).

get_nif_bin_dir() ->
  {ok, Cwd} = file:get_cwd(),
  get_nif_bin_dir(
    [ code:priv_dir(crc32cer)
    , filename:join([Cwd, "..", "priv"])
    , filename:join(Cwd, "priv")
    , os:getenv("NIF_BIN_DIR")
    ]).

get_nif_bin_dir([]) -> erlang:error(crc32cer_nif_not_found);
get_nif_bin_dir([false | Rest]) -> get_nif_bin_dir(Rest);
get_nif_bin_dir([Dir | Rest]) ->
  case filelib:wildcard(filename:join([Dir, "libcrc32cer_nif*"])) of
    [] -> get_nif_bin_dir(Rest);
    [_ | _] -> Dir
  end.


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
