-module(junkscraper_github).
-export([request/3, request/4]).

request(Token, Method, Resource) ->
    request(Token, Method, Resource, #{}).

request(Token, Method, Resource, Request) ->
    {ok, ConnPid} = gun:open("api.github.com", 443),
    StreamRef = gun:request(ConnPid, Method, Resource, [
      {<<"authorization">>, [<<"token ">>, Token]},
      {<<"content-type">>, <<"application/json">>},
      {<<"user-agent">>, atom_to_list(?MODULE)}
    ], json:encode(Request)),
    {ok, Response} = gun:await_body(ConnPid, StreamRef),
    {ok, Payload, _Rest} = json:decode(Response),
    Payload.
