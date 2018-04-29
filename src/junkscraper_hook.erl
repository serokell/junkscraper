-module(junkscraper_hook).
-export([execute/2]).

-behaviour(cowboy_middleware).

execute(Req0, _) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    {ok, authenticate(Body, Req1), #{}}.

authenticate(Body, Req) ->
    {ok, Secret} = application:get_env(junkscraper, github_secret),
    Signature0 = cowboy_req:header(<<"x-hub-signature">>, Req),
    Signature1 = encode_signature(crypto:hmac(sha, Secret, Body)),
    if Signature0 == Signature1 ->
	    Event = cowboy_req:header(<<"x-github-event">>, Req),
	    {ok, Payload, _Rest} = json:decode(Body),
	    handle(Event, Payload, Req);
       true ->
	    cowboy_req:reply(400, Req)
    end.

binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) ||
		      X <- binary_to_list(Bin)]).

encode_signature(Bin) ->
    list_to_binary("sha1=" ++ binary_to_hex(Bin)).

handle(<<"pull_request">>, Payload, Req) ->
    #{<<"action">> := Action,
      <<"pull_request">> := #{<<"head">> := HEAD}} = Payload,
    #{<<"repo">> := #{<<"full_name">> := Name},
      <<"ref">> := Ref} = HEAD,
    if Action == <<"closed">> ->
	    spawn(fun() -> delete_ref(Name, Ref) end),
	    cowboy_req:reply(202, Req);
       true ->
	    cowboy_req:reply(200, Req)
    end;
handle(_, _, Req) ->
    cowboy_req:reply(200, Req).

delete_ref(Name, Ref) ->
    {ok, Token} = application:get_env(junkscraper, github_token),
    URL = [<<"/repos/">>, Name, <<"/git/refs/heads/">>, Ref],
    junkscraper_github:request(Token, <<"DELETE">>, URL).
