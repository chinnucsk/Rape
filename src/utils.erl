-module(utils).
-export([purge/0]).

purge() ->
    [clear_bucket(Name) || Name <- ["shops",
                                    "kukurydza",
                                    "rzepak",
                                    "nawozy",
                                    "warzywa",
                                    "zboza",
                                    "zaprawy",
                                    "nasiona"]].

clear_bucket(Name) ->
    BinName = list_to_binary(Name),
    Pid = controllers:local_client(),
    {ok, Keys} = riakc_pb_socket:list_keys(Pid, BinName),
    [riakc_pb_socket:delete(Pid, BinName, Key) || Key <- Keys].
