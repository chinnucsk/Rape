-module(rape_web).

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case dispatch(Req, controllers:urls()) of
            none ->
                % No request handler found
                case filelib:is_file(filename:join([DocRoot, Path])) of
                    true ->
                        % If there's a static file, serve it
                        Req:serve_file(Path, DocRoot);
                    false ->
                        % Otherwise the page is not found
                        Req:not_found()
                end;
            Response ->
                Response
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

dispatch(_, []) ->
    none;
dispatch(Req, [{Regexp, Function} | Rest]) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    Match = re:run(Path, Regexp, [global, {capture, all_but_first, list}]),
    case Match of
        {match, [_Args]} ->
            apply(controllers, Function, [Method, Req]);
        _ ->
            dispatch(Req, Rest)
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
