-module(paxy).
-export([start/1, start/0, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", "Acceptor e", 
                  "Acceptor f", "Acceptor g", "Acceptor h", "Acceptor i", "Acceptor j",
                  "Acceptor k", "Acceptor l", "Acceptor m", "Acceptor n", "Acceptor o",
                  "Acceptor p", "Acceptor q", "Acceptor r", "Acceptor s", "Acceptor t"],
  AccRegister = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t],
  ProposerNames = [
    {"Proposer kurtz", ?RED}, 
    {"Proposer kilgore", ?GREEN}, 
    {"Proposer willard", ?BLUE},
    {"Proposer kurtz 2", ?RED}, 
    {"Proposer kilgore 2", ?GREEN}, 
    {"Proposer willard 2", ?BLUE},
    {"Proposer kurtz 3", ?RED}, 
    {"Proposer kilgore 3", ?GREEN}, 
    {"Proposer willard 3", ?BLUE}
  ],
  PropInfo = [
    {kurtz, ?RED}, 
    {kilgore, ?GREEN}, 
    {willard, ?BLUE},
    {kurtz2, ?RED}, 
    {kilgore2, ?GREEN}, 
    {willard2, ?BLUE},
    {kurtz3, ?RED}, 
    {kilgore3, ?GREEN}, 
    {willard3, ?BLUE}
  ],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      start_proposers(PropIds, PropInfo, AccRegister, Sleep)
  end,
  true.

start() ->
  Sleep = [1,1,1,1,1,1,1,1,1],
  start(Sleep).
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep)
  end.

stop() ->
  stop(a),
  stop(b),
  stop(c),
  stop(d),
  stop(e),
  stop(f),
  stop(g),
  stop(h),
  stop(i),
  stop(j),
  stop(k),
  stop(l),
  stop(m),
  stop(n),
  stop(o),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.
