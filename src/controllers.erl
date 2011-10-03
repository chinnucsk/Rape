-module(controllers).
-export([urls/0, shops/2, add_shop/2, local_client/0]).


urls() ->
    [
     {"^$", shops},
     {"^shops/?$", shops},
     {"^add_shop/?$", add_shop}
    ].

shops('GET', Req) ->
    Shops = fetch_shops([]),
    {ok, HTML} = shops_dtl:render([{shops, Shops}]),
    Req:ok({"text/html", HTML});
shops('POST', Req) ->
    PostData = Req:parse_post(),
    Types = proplists:get_all_values("type", PostData),
    EntityTypes = proplists:get_all_values("entity_type", PostData),
    Shops = fetch_shops(Types),
    {ok, HTML} = shops_dtl:render([{shops, Shops}]),
    Req:ok({"text/html", HTML}).

add_shop('GET', Req) ->
    {ok, HTML} = add_shop_dtl:render([]),
    Req:ok({"text/html", HTML});
add_shop('POST', Req) ->
    Pid = local_client(),
    PostData = Req:parse_post(),
    Types = proplists:get_all_values("type", PostData),
    Shop = mochijson2:encode([{"type", Types} | proplists:delete("type", PostData)]),
    ShopObj = riakc_obj:new(<<"shops">>, undefined, Shop),
    {ok, Key} = riakc_pb_socket:put(Pid, ShopObj),
    add_to_index(Pid, ["shops" | Types], Key),
    Req:respond({302,
                 [{"Location", "/shops"},
                  {"Content-Type", "text/html; charset=UTF-8"}],
                 ""}).

fetch_shops(Types) ->
    Pid = local_client(),
    Keys = case Types of
               [] ->
                   case riakc_pb_socket:get(Pid, <<"shops">>, <<"keys">>) of
                       {ok, KeysObj} ->
                           mochijson2:decode(binary_to_term(riakc_obj:get_value(KeysObj)));
                       {error, notfound} ->
                           []
                   end;
               _ ->
                   lists:usort(find_keys(Types, Pid))
           end,
    Inputs = [{<<"shops">>, Key} || Key <- Keys],
    case riakc_pb_socket:mapred(Pid, Inputs,
                                [{map, {modfun, riak_kv_mapreduce, map_object_value}, none, true}]) of
        {ok, [{0, Res}]} ->
            lists:map(fun({struct, X}) -> X end,
                      [mochijson2:decode(binary_to_term(ShopBin)) || ShopBin <- Res]);
        {ok, []} ->
            []
    end.

find_keys(Types, Pid) ->
    find_keys(Types, Pid, []).

find_keys([], _, Acc) -> Acc;
find_keys([T | Types], Pid, Acc) ->
    Keys = case riakc_pb_socket:get(Pid, list_to_binary(T), <<"keys">>) of
               {ok, KeysObj} ->
                   mochijson2:decode(binary_to_term(riakc_obj:get_value(KeysObj)));
               {error, notfound} ->
                   []
           end,
    find_keys(Types, Pid, Acc ++ Keys).

%% Y U NO HAVE SECONDARY INDEX SUPPORT YET, ERLANG RIAK CLIENT?!?!
add_to_index(_, [], _) ->
    ok;
add_to_index(Pid, [T | Types], Key) ->
    case riakc_pb_socket:get(Pid, list_to_binary(T), <<"keys">>) of
        {ok, IndexObj} ->
            Keys = mochijson2:decode(binary_to_term(riakc_obj:get_value(IndexObj))),
            Keys1 = mochijson2:encode([Key | Keys]),
            IndexObj1 = riakc_obj:update_value(IndexObj, Keys1),
            riakc_pb_socket:put(Pid, IndexObj1),
            add_to_index(Pid, Types, Key);
        {error, notfound} ->
            Keys = mochijson2:encode([Key]),
            IndexObj = riakc_obj:new(list_to_binary(T), <<"keys">>, Keys),
            riakc_pb_socket:put(Pid, IndexObj),
            add_to_index(Pid, Types, Key)
    end.

local_client() ->
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8081),
    Pid.
