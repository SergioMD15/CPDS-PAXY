-module(paxy).

-export([startSplitProposers/2, startSplitProposers2/2, startSplitAcceptors/1, stop/0, stop/1]).
-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
startSplitProposers(Sleep, AccepNode) ->
  register(proposers, spawn(fun() -> startSplitProposers2(Sleep, AccepNode) end)).

startSplitProposers2(Sleep, AccepNode) ->
  ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3"],
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
  % computing panel heights
  PropPanelHeight = length(ProposerNames)*50 + 0,
  register(gui_proposers, spawn(fun() -> gui:start_proposers(ProposerNames, PropPanelHeight) end)),
  gui_proposers ! {reqStateProp, self()},

  receive
    {reqStateProp, State} ->
      receive
        {accReg, AccRegister} ->
          {PropIds} = State,
          startProposers(PropIds, PropInfo, AccRegister, Sleep, AccepNode)
      end
  end.

startSplitAcceptors(PropNode) ->
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", "Acceptor e"],
  AccRegister = [a, b, c, d, e],
  {proposers, PropNode} ! {accReg, AccRegister},

  % computing panel heights
  AccPanelHeight = length(AcceptorNames)*50 + 0, %plus the spacer value
  register(gui_acceptors, spawn(fun() -> gui:start_acceptors(AcceptorNames, AccPanelHeight) end)),
  gui_acceptors ! {reqStateAccep, self()},

  receive
    {reqStateAccep, State} ->
      {AccIds} = State,
      startAcceptors(AccIds, AccRegister)
  end.
	
startAcceptors(AccIds, AccReg) ->
	case AccIds of
		[] ->
			ok;
		[AccId|Rest] ->
			[RegName|RegNameRest] = AccReg,
			register(RegName, acceptor:start(RegName, AccId)),
			startAcceptors(Rest, RegNameRest)
	end.

startProposers(PropIds, PropInfo, Acceptors, Sleep, AccepNode) ->
	case PropIds of
		[] ->
			ok;
		[PropId|Rest] ->
			[{RegName, Colour}|RestInfo] = PropInfo,
			[FirstSleep|RestSleep] = Sleep,
			proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, AccepNode),
			startProposers(Rest, RestInfo, Acceptors, RestSleep, AccepNode)
		end.

stop() ->
    stop(a),
    stop(b),
    stop(c),
    stop(d),
    stop(e),
    stop(gui_acceptors),
    stop(gui_proposers).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.
