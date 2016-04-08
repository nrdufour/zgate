-module(zgate_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
        ZParser = {
          zparser, {
            zparser, start, ["/dev/ttyUSB0", b115200, fun zgate_proc:process_packet/2]
           },
          permanent,
          5000,
          worker,
          [zparser]
         },

        ZProc = {
          zgate_proc, {
            zgate_proc, start_link, []
           },
          permanent,
          5000,
          worker,
          [zgate_proc]
         },

	Procs = [ZParser, ZProc],

	{ok, {{one_for_one, 1, 5}, Procs}}.
