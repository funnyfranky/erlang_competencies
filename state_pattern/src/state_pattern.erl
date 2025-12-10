-module(state_pattern).

-export([start/0, login/0, logout/0, state/0, loop/1]).

start() ->
    Pid = spawn(?MODULE, loop, [logged_out]),
    register(login_machine, Pid),
    Pid.

login() ->
    login_machine ! {self(), login},
    receive
        Reply -> Reply
    end.

logout() ->
    login_machine ! {self(), logout},
    receive
        Reply -> Reply
    end.

state() ->
    login_machine ! {self(), state},
    receive
        Reply -> Reply
    end.


loop(State) ->
    receive
        {From, login} ->
            case State of
                logged_in ->
                    From ! already_logged_in,
                    loop(State);
                logged_out ->
                    From ! ok,
                    loop(logged_in)
            end;

        {From, logout} ->
            case State of
                logged_out ->
                    From ! already_logged_out,
                    loop(State);
                logged_in ->
                    From ! ok,
                    loop(logged_out)
            end;

        {From, state} ->
            From ! State,
            loop(State)
    end.
