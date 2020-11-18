-module(app1).

-behaviour(gen_server).

-include("fake_osa.hrl").
%% API
-export([start_link/0, ping/0, pong/0,say_hi/1,zip_and_send/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PEER, app1_1).

-record(state, {tref}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% peer calls our to get pong
ping() ->
  % code execution is in caller context
  % this server executes only handle_call()
  case gen_server:call(?SERVER, ping) of
    pong ->
      ?PEER:pong(),
      io:format("~p sending pong~n", [?SERVER]);
    _ ->
      io:format("~p failed processing ping~n", [?SERVER])
  end.

%% peer answers our ping by calling pong
pong() ->
  io:format("~p got pong~n", [?MODULE]).

say_hi(Pid) ->
  gen_server:cast(?SERVER, {say_hi,Pid}).

zip_and_send(Pid) ->
  gen_server:cast(?SERVER, {send_zip,Pid}).

%%------------------------------------------------------------------------------

init([]) ->
  State = undefined,
  {ok, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({send_zip,Pid}, State) ->
  Data = get_zip(),
  Pid ! {zip,Data},
  {noreply, State};
handle_cast({say_hi,Pid}, State) ->
  Pid ! hi,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(ping, State) ->
  % note here we are calling a function from module app1_1
  % but code execution is in our context
  io:format("~p calling app1_1:ping()~n", [?SERVER]),
  app1_1:ping(app1),
  {noreply, State};
handle_info(pong, State) ->
  io:format("~p got pong~n", [?SERVER]),
  timer:cancel(State#state.tref),
  TRef = erlang:send_after(5000, ?SERVER, ping, []),
  NewState = #state{tref = TRef},
  {noreply, NewState};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_zip() ->
  Bin_osa = parse_raw_to_osa(?FAKE_OSA_DATA),
  io:format("The size of bin osa is:~p~n",[byte_size(Bin_osa)]),
  % Z = zlib:open(),
  % ok = zlib:deflateInit(Z,9),
  Data = zlib:compress(Bin_osa),
  io:format("The size of Data is:~p~n",[byte_size(Data)]),
  Data.
  % Compress = fun(end_of_data, _Cont) -> [];
  %               (Data, Cont) ->
  %                 [zlib:deflate(Z, Data)|Cont(Read(),Cont)]
  %           end,
  % Compressed = Compress(Read(),Compress),
  % Last = zlib:deflate(Z, [], finish),
  % ok = zlib:deflateEnd(Z),
  % zlib:close(Z),
  % list_to_binary([Compressed|Last]).

parse_raw_to_osa(Raw_power) ->
  << << X:2/little-signed-integer-unit:8 >> || X <-  Raw_power >>.