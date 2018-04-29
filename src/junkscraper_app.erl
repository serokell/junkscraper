-module(junkscraper_app).
-export([start/2, stop/1]).

-behaviour(application).

start(_Type, _Args) ->
    {ok, Port} = application:get_env(junkscraper, port),
    {ok, _Pid} = cowboy:start_clear(junkscraper, [{port, Port}], #{middlewares => [junkscraper_hook]}),
    junkscraper_sup:start_link().

stop(_State) ->
    ok.
