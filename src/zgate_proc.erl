-module(zgate_proc).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([process_packet/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          db
         }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_packet(Address, Data) ->
    io:format("Sending a packet!~n"),
    Now = iso8601:format(calendar:universal_time()),
    gen_server:cast(?MODULE, {process, Address, Data, Now}).

%% gen_server.

init([]) ->
    io:format("Starting ZGate Processor~n"),
    Url = "http://localhost:5984",
    S = couchbeam:server_connection(Url, []),
    {ok, Db} = couchbeam:open_or_create_db(S, "testdb", []),
    {ok, #state{db=Db}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({process, Address, Data, Now}, #state{db=Db}=State) ->
    io:format("Processing zigbee packet from ~p: ~p~n", [Address, Data]),

    Trimmed = binary:replace(Data, <<"\r\n">>, <<>>),
    Fields = binary:split(Trimmed, <<";">>, [global, trim_all]),
    KVs = lists:map(fun(X) -> [K,V] = binary:split(X, <<":">>), {K, V} end, Fields),

    BinAddress = list_to_binary(io_lib:format("~.16B", [Address])),
    KVsNAddress = KVs ++ [ { <<"address">>, BinAddress } ],

    WithTime = KVsNAddress ++ [ { <<"received">>, Now } ],

    Doc = { WithTime },
    io:format("Doc: ~p~n", [Doc]),

    {ok, _Doc1} = couchbeam:save_doc(Db, Doc),

    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
