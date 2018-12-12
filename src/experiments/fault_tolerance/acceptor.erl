-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  pers:open(Name),
  {Pr, Vot, Val, Pn} = pers:read(Name),
  case Pn == na of
    true -> acceptor(Name, Pr, Vot, Val, PanelId),
      pers:store(Name, Pr, Vot, Val, PanelId);
    false -> acceptor(Name, Pr, Vot, Val, Pn)
  end.

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          Proposer ! {promise, Round, Voted, Value},
          % Update gui
          if
            Value == na ->
              Colour = {0, 0, 0};
            true ->
              Colour = Value
          end,
          io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
            [Name, Round, Voted, Value]),
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
              "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          pers:store(Name, Promised, Voted, Value, PanelId),
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          pers:store(Name, Promised, Voted, Value, PanelId),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          Proposer ! {vote, Round},
          case order:goe(Round, Voted) of
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                  "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              pers:store(Name, Promised, Voted, Value, PanelId),
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->

              io:format("[Acceptor ~w] Phase 2: voted ~w promised ~w colour ~w~n", [Name, Voted, Promised, Value]),
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                  "Promised: " ++ io_lib:format("~p", [Promised]), Value},
              pers:store(Name, Promised, Voted, Value, PanelId),
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        false ->
          Proposer ! {sorry, {accept, Voted}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      pers:store(Name, Promised, Voted, Value, PanelId),
      PanelId ! stop,
      ok
  end.
