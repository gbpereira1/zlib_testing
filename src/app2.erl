-module(app2).

-behaviour(gen_server).

-include("fake_osa.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%------------------------------------------------------------------------------

init([]) ->
    State = undefined,
    {ok, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(hi, State) ->
  io:format("App2 received hi!"),
  {noreply, State};
handle_info({zip,Data}, State) ->
  io:format("App2 received something, let see...~n"),
  unzip(Data),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

unzip(Data) ->
  Unziped = zlib:uncompress(Data),
  io:format("Received: ~p bytes~n",[byte_size(Unziped)]),
  Bin_osa = parse_raw_to_osa(?FAKE_OSA_DATA),

  if Unziped == Bin_osa ->  io:format("It's the fake OSA!~n");
  true -> io:format("I don't know what it is~n")
  end.



parse_raw_to_osa(Raw_power) ->
  << << X:2/little-signed-integer-unit:8 >> || X <-  Raw_power >>.