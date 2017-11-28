-module(eutil).
-export([urlencode/1,
         md5_hex/1,
         to_list/1, to_binary/1, to_atom/1,
         http_get/1, http_get/2, http_get/4,
         http_post/3, http_post/4,
         term_to_string/1, string_to_term/1,
         term_to_bitstring/1, bitstring_to_term/1,
         get_ets/2, put_ets/3, del_ets/2, get_ets_keys/1, get_ets_keys/2,
         mapskeydelete/3, mapskeyreplace/4, mapskeyfind/3,
         get_cowboy_post_vals/1,
         bool_to_int/1, int_to_bool/1,
         json_encode/1, json_decode/1,
         gen_multi_insert_sql/3, gen_multi_update_sql/3, gen_multi_replace_sql/3,
         utc_string/0, utc_string/1,
         shuffle/1,
         select_by_weight/1, select_amount_by_weight/2,
         get_rand_elem/1, get_rand_elems/2,
         count_atom/0,
         eval/2
        ]).

-include("eutil.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(FLOAT_BIAS, 1022).
-define(MIN_EXP, -1074).
-define(BIG_POW, 4503599627370496).

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
    quote_plus(binary_to_list(Binary));
quote_plus(Float) when is_float(Float) ->
    quote_plus(digits(Float));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

digits(N) when is_integer(N) ->
    integer_to_list(N);
digits(0.0) ->
    "0.0";
digits(Float) ->
    {Frac1, Exp1} = frexp_int(Float),
    [Place0 | Digits0] = digits1(Float, Exp1, Frac1),
    {Place, Digits} = transform_digits(Place0, Digits0),
    R = insert_decimal(Place, Digits),
    case Float < 0 of
        true ->
            [$- | R];
        _ ->
            R
    end.

%% @spec int_pow(X::integer(), N::integer()) -> Y::integer()
%% @doc  Moderately efficient way to exponentiate integers.
%%       int_pow(10, 2) = 100.
int_pow(_X, 0) ->
    1;
int_pow(X, N) when N > 0 ->
    int_pow(X, N, 1).

%% @spec int_ceil(F::float()) -> integer()
%% @doc  Return the ceiling of F as an integer. The ceiling is defined as
%%       F when F == trunc(F);
%%       trunc(F) when F &lt; 0;
%%       trunc(F) + 1 when F &gt; 0.
int_ceil(X) ->
    T = trunc(X),
    case (X - T) of
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.


%% Internal API

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

insert_decimal(0, S) ->
    "0." ++ S;
insert_decimal(Place, S) when Place > 0 ->
    L = length(S),
    case Place - L of
        0 ->
            S ++ ".0";
        N when N < 0 ->
            {S0, S1} = lists:split(L + N, S),
            S0 ++ "." ++ S1;
        N when N < 6 ->
            %% More places than digits
            S ++ lists:duplicate(N, $0) ++ ".0";
        _ ->
            insert_decimal_exp(Place, S)
    end;
insert_decimal(Place, S) when Place > -6 ->
    "0." ++ lists:duplicate(abs(Place), $0) ++ S;
insert_decimal(Place, S) ->
    insert_decimal_exp(Place, S).

insert_decimal_exp(Place, S) ->
    [C | S0] = S,
    S1 = case S0 of
             [] ->
                 "0";
             _ ->
                 S0
         end,
    Exp = case Place < 0 of
              true ->
                  "e-";
              false ->
                  "e+"
          end,
    [C] ++ "." ++ S1 ++ Exp ++ integer_to_list(abs(Place - 1)).


digits1(Float, Exp, Frac) ->
    Round = ((Frac band 1) =:= 0),
    case Exp >= 0 of
        true ->
            BExp = 1 bsl Exp,
            case (Frac =/= ?BIG_POW) of
                true ->
                    scale((Frac * BExp * 2), 2, BExp, BExp,
                          Round, Round, Float);
                false ->
                    scale((Frac * BExp * 4), 4, (BExp * 2), BExp,
                          Round, Round, Float)
            end;
        false ->
            case (Exp =:= ?MIN_EXP) orelse (Frac =/= ?BIG_POW) of
                true ->
                    scale((Frac * 2), 1 bsl (1 - Exp), 1, 1,
                          Round, Round, Float);
                false ->
                    scale((Frac * 4), 1 bsl (2 - Exp), 2, 1,
                          Round, Round, Float)
            end
    end.

scale(R, S, MPlus, MMinus, LowOk, HighOk, Float) ->
    Est = int_ceil(math:log10(abs(Float)) - 1.0e-10),
    %% Note that the scheme implementation uses a 326 element look-up table
    %% for int_pow(10, N) where we do not.
    case Est >= 0 of
        true ->
            fixup(R, S * int_pow(10, Est), MPlus, MMinus, Est,
                  LowOk, HighOk);
        false ->
            Scale = int_pow(10, -Est),
            fixup(R * Scale, S, MPlus * Scale, MMinus * Scale, Est,
                  LowOk, HighOk)
    end.

fixup(R, S, MPlus, MMinus, K, LowOk, HighOk) ->
    TooLow = case HighOk of
                 true ->
                     (R + MPlus) >= S;
                 false ->
                     (R + MPlus) > S
             end,
    case TooLow of
        true ->
            [(K + 1) | generate(R, S, MPlus, MMinus, LowOk, HighOk)];
        false ->
            [K | generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk)]
    end.

generate(R0, S, MPlus, MMinus, LowOk, HighOk) ->
    D = R0 div S,
    R = R0 rem S,
    TC1 = case LowOk of
              true ->
                  R =< MMinus;
              false ->
                  R < MMinus
          end,
    TC2 = case HighOk of
              true ->
                  (R + MPlus) >= S;
              false ->
                  (R + MPlus) > S
          end,
    case TC1 of
        false ->
            case TC2 of
                false ->
                    [D | generate(R * 10, S, MPlus * 10, MMinus * 10,
                                  LowOk, HighOk)];
                true ->
                    [D + 1]
            end;
        true ->
            case TC2 of
                false ->
                    [D];
                true ->
                    case R * 2 < S of
                        true ->
                            [D];
                        false ->
                            [D + 1]
                    end
            end
    end.

unpack(Float) ->
    <<Sign:1, Exp:11, Frac:52>> = <<Float:64/float>>,
    {Sign, Exp, Frac}.

transform_digits(Place, [0 | Rest]) ->
    transform_digits(Place, Rest);
transform_digits(Place, Digits) ->
    {Place, [$0 + D || D <- Digits]}.


frexp_int(F) ->
    case unpack(F) of
        {_Sign, 0, Frac} ->
            {Frac, ?MIN_EXP};
        {_Sign, Exp, Frac} ->
            {Frac + (1 bsl 52), Exp - 53 - ?FLOAT_BIAS}
    end.

%% @spec urlencode([{Key, Value}]) -> string()
%% @doc URL encode the property list.
urlencode(Props) when is_list(Props) ->
    Pairs = lists:foldr(
              fun({K, V}, Acc) ->
                      [quote_plus(K) ++ "=" ++ quote_plus(V) | Acc]
              end, [], Props),
    string:join(Pairs, "&");
urlencode(Maps) ->
    Props = maps:to_list(Maps),
    urlencode(Props).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
md5_hex(S) ->
    Md5_bin = erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_list(Item) when is_binary(Item) ->
    erlang:binary_to_list(Item);
to_list(Item) when is_integer(Item) ->
    erlang:integer_to_list(Item);
to_list(Item) when is_atom(Item) ->
    erlang:atom_to_list(Item);
to_list(Item) when is_list(Item) ->
    Item;
to_list(Item) when is_tuple(Item) ->
    tuple_to_list(Item);
to_list(Item) when is_float(Item) ->
    [String] = io_lib:format("~p", [Item]),
    String.


to_binary(Item) when is_list(Item) ->
    list_to_binary(Item);
to_binary(Item) when is_binary(Item) ->
    Item;
to_binary(Item) when is_integer(Item) ->
    erlang:integer_to_binary(Item);
to_binary(Item) when is_atom(Item) ->
    erlang:atom_to_binary(Item, utf8).

to_atom(Item) when is_list(Item) ->
    list_to_atom(Item);
to_atom(Item) when is_binary(Item) ->
    erlang:binary_to_atom(Item, utf8);
to_atom(Item) when is_atom(Item) ->
    Item.



http_get(URL) ->
    http_get(URL, [], [], []).

http_get(URL, Query) ->
    http_get(URL, [], Query, []).

http_get(URL, Headers, Query, Options) ->
    ReqItems = case is_map(Query) of
                   true -> maps:to_list(Query);
                   false -> Query
               end,
    NewURL = hackney_url:make_url(URL, [], ReqItems),
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(get, NewURL, Headers,
                                                                 <<>>, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    json_decode(ResultBin).


http_post(URL, Headers, Payload) ->
    http_post(URL, Headers, Payload, []).

http_post(URL, Headers, Payload, Options) when is_binary(Payload) ->
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(post, URL, Headers,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    json_decode(ResultBin);
http_post(URL, Headers, PayloadMaps, Options) when is_map(PayloadMaps) ->
    [Header | _] = Headers,
    Payload = case Header of
                  ?URLENCEDED_HEAD -> list_to_binary(urlencode(PayloadMaps));
                  ?JSON_HEAD -> json_encode(PayloadMaps)
              end,
    http_post(URL, Headers, Payload, Options);
http_post(URL, Headers, PayloadItems, Options) when is_list(PayloadItems) ->
    Payload = list_to_binary(urlencode(PayloadItems)),
    http_post(URL, Headers, Payload, Options).


%term_to_string(Term) ->
%    %%  用下面这个方法虽然能保证 C S 这些字符比较好的入库，但会导致 下面问题
%    %%1> binary_to_list(list_to_binary(io_lib:format("~p", [[59, 62, 230]]))).  -> "\";>鎈""
%    %% 	binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).
%    %%use a more effecional way
%    lists:flatten(io_lib:format("~w", [Term])).
%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).


%%************************************************
%% keypos设置为1，set并且 get_ets 和 put_ets 配套使用
get_ets(Ets, Key) ->
    case ets:lookup(Ets, Key) of
        [] -> undefined;
        [{Key, Value}] -> Value
    end.

put_ets(Ets, Key, Value) ->
    ets:insert(Ets, {Key, Value}).

del_ets(Ets, Key) ->
    ets:delete(Ets, Key).

get_ets_keys(Tab) ->
    ets:safe_fixtable(Tab, true),
    Keys = get_ets_keys(Tab, [], ets:first(Tab)),
    ets:safe_fixtable(Tab, false),
    Keys.

get_ets_keys(Tab, Fun) ->
    ets:safe_fixtable(Tab, true),
    Keys = get_ets_keys(Fun, Tab, [], ets:first(Tab)),
    ets:safe_fixtable(Tab, false),
    Keys.


get_ets_keys(_Tab, Keys, '$end_of_table') ->
    Keys;
get_ets_keys(Tab, Keys, Key) ->
    NextKey = ets:next(Tab, Key),
    get_ets_keys(Tab, [Key | Keys], NextKey).

get_ets_keys(_Fun, _Tab, Keys, '$end_of_table') ->
    Keys;
get_ets_keys(Fun, Tab, Keys, Key) ->
    NextKey = ets:next(Tab, Key),
    case Fun(Key) of
        true -> get_ets_keys(Fun, Tab, [Key | Keys], NextKey);
        false -> get_ets_keys(Fun, Tab, Keys, NextKey)
    end.

%%************************************************

%%util:eval("B + A.",[{'A', 0}, {'B', 2}]).
eval(ExprStr, Environ) ->
    BindFun = fun({Arg, Val}, Bindings) ->
                      erl_eval:add_binding(Arg, Val, Bindings)
              end,
    NewBindings = lists:foldl(BindFun, erl_eval:new_bindings(), Environ),

    {ok, Scanned, _} = erl_scan:string(ExprStr),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Result, _NewBindings} = erl_eval:exprs(Parsed, NewBindings),
    Result.

%%%%%%%%%%%%%%%%%%%%%%Maps%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mapskeydelete(What, Key, [H|T]) ->
    case maps:get(Key, H) == What of
        true -> T;
        false -> [H|mapskeydelete(What, Key, T)]
    end;
mapskeydelete(_, _, []) -> [].

mapskeyreplace(What, Key, L, New) when is_list(L), erlang:is_map(New) ->
    mapskeyreplace3(What, Key, L, New).

mapskeyreplace3(What, Key, [H|T], New) ->
    case maps:get(Key, H) == What of
        true -> [New|T];
        false -> [H|mapskeyreplace3(What, Key, T, New)]
    end;
mapskeyreplace3(_, _, [], _) -> [].

mapskeyfind(_What, _Key, []) ->
    false;
mapskeyfind(What, Key, [H|T]) ->
    case maps:get(Key, H) == What of
        true -> H;
        false -> mapskeyfind(What, Key, T)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_cowboy_post_vals(Req) ->
    {ok, OriPostVals, _Req} = cowboy_req:body_qs(Req),
    case OriPostVals of
        [{JsonBin, true}] -> json_decode(JsonBin);
        Other -> maps:from_list(Other)
    end.


%% true转为数字1，false转为数字0    
bool_to_int(Boolean) ->
    case Boolean of
        true -> 1;
        false -> 0;
        undefined -> 0
    end.

%% 数字1转为true，0或其余数字转为false
int_to_bool(Num) ->
    case Num of
        1 -> true;
        0 -> false;
        undefined -> false
    end.


format_sql_values(Values) ->
    Fun = fun(Value) ->
                  case is_binary(Value) orelse is_list(Value) of
                      true -> io_lib:format("'~s'", [Value]);
                      false -> io_lib:format("~p", [Value])
                  end
          end,
    FormatedValues = lists:map(Fun, Values),
    "(" ++ string:join(FormatedValues, ",") ++ ")".

gen_multi_insert_sql(TableName, FieldList, ValuesList) ->
    FieldsStr = "(" ++ string:join([io_lib:format("`~s`", [Field]) || Field <- FieldList], ",") ++ ")",
    ValuesListGen = [format_sql_values(Values) || Values <- ValuesList],
    ValuesStr = string:join(ValuesListGen, ","),
    io_lib:format("INSERT INTO `~s` ~s VALUES ~s", [TableName, FieldsStr, ValuesStr]).

gen_multi_replace_sql(TableName, FieldList, ValuesList) ->
    FieldsStr = "(" ++ string:join([io_lib:format("`~s`", [Field]) || Field <- FieldList], ",") ++ ")",
    ValuesListGen = [format_sql_values(Values) || Values <- ValuesList],
    ValuesStr = string:join(ValuesListGen, ","),
    io_lib:format("REPLACE INTO `~s` ~s VALUES ~s", [TableName, FieldsStr, ValuesStr]).

gen_multi_update_sql(TableName, FieldList, ValuesList) ->
    FieldsStr = "(" ++ string:join([io_lib:format("`~s`", [Field]) || Field <- FieldList], ",") ++ ")",
    ValuesListGen = [format_sql_values(Values) || Values <- ValuesList],
    ValuesStr = string:join(ValuesListGen, ","),
    GenDuplicateFun = fun(Field) ->
                              io_lib:format("`~s`=VALUES(`~s`)", [Field, Field])
                      end,
    DuplicateList = [GenDuplicateFun(Field) || Field <- FieldList],
    DuplicateStr = string:join(DuplicateList, ","),
    io_lib:format("INSERT INTO `~s` ~s VALUES ~s ON DUPLICATE KEY UPDATE ~s", [TableName, FieldsStr, ValuesStr, DuplicateStr]).

json_encode(Maps) ->
    jsone:encode(Maps).

json_decode(Bin) ->
    jsone:decode(Bin).

utc_string() ->
    Time = calendar:universal_time(),
    utc_string(Time).

utc_string(Time) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = Time,
    iolist_to_binary(io_lib:format("~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ",
                                   [Year, Month, Day, Hour, Min, Sec])).


-spec shuffle(L :: list()) ->
    R :: any().
shuffle(L) ->
    L2 = [{rand:uniform(100), Item} || Item <- L],
    [ShuffledItem || {_, ShuffledItem} <- lists:sort(L2)].

-type key() :: any().
-type weight() :: integer.
-type weight_item() :: [{key(), weight()}].
-spec select_by_weight(List :: list(weight_item())) ->
    R :: {ok, key()} | {error, _}.
select_by_weight(List) ->
    AllWeight = lists:sum([Weight || {_Item, Weight} <- List, Weight >= 0]),
    case AllWeight > 0 of
        true ->
            RandomWeight = rand:uniform(AllWeight),
            select_by_weight(List, RandomWeight);
        false ->
            {error, <<"AllWeightMustGreaterThanZero">>}
    end.

select_by_weight(List, RandomWeight) ->
    [{Item, Weight} | RestList] = List,
    if
        RandomWeight =< Weight ->
            {ok, Item};
        RandomWeight > Weight ->
            select_by_weight(RestList, RandomWeight - Weight)
    end.


-spec select_amount_by_weight(List :: list(weight_item()), Amount :: integer) ->
    R :: {ok, list(any)} | {error, _}.
select_amount_by_weight(_List, Amount) when Amount =< 0 ->
    {error, <<"AmountMustGreaterThanZero">>};
select_amount_by_weight(List, Amount) when length(List) == Amount  ->
    SelectedKeys = [Key || {Key, _} <- List],
    {ok, SelectedKeys};
select_amount_by_weight(List, Amount) when length(List) < Amount ->
    {error, <<"ListLengthMustGreaterThanAmount">>};
select_amount_by_weight(List, Amount) ->
    select_amount_by_weight(List, Amount, []).

select_amount_by_weight(_List, 0, SelectedKeys) ->
    SelectedKeys;
select_amount_by_weight([], _Amount, SelectedKeys) ->
    SelectedKeys;
select_amount_by_weight(List, Amount, SelectedKeys) ->
    {ok, SelectedKey} = select_by_weight(List),
    SelectedList = lists:keydelete(SelectedKey, 1, List),
    select_amount_by_weight(SelectedList, Amount-1, [SelectedKey | SelectedKeys]).


-spec get_rand_elem(List :: list()) ->
    R :: undefined | any().
get_rand_elem([]) ->
    undefined;
get_rand_elem(List) ->
    {ok, [Elem]} = get_rand_elems(List, 1),
    Elem.


-spec get_rand_elems(List :: list(), Amount :: integer) ->
    R :: {ok, list()} | {error, _}.
get_rand_elems([], _Amount) ->
    {error, <<"ListEmpty">>};
get_rand_elems(_List, Amount) when Amount =< 0 ->
    {error, <<"ArgAmountIllegal">>};
get_rand_elems(List, Amount) when length(List) == Amount ->
    {ok, List};
get_rand_elems(List, Amount) when length(List) < Amount ->
    {error, <<"NoEnoughListElems">>};
get_rand_elems(List, Amount) ->
    get_rand_elems(Amount, List, length(List), [], 0).

get_rand_elems(_Amount, [], _ListLen, Elems, _ElemsAmount) ->
    {ok, Elems};
get_rand_elems(Amount, _List, _ListLen, Elems, ElemsAmount) when ElemsAmount >= Amount ->
    {ok, Elems};
get_rand_elems(Amount, List, ListLen, Elems, ElemsAmount) ->
    Index = rand:uniform(ListLen),
    Elem = lists:nth(Index, List),
    RestList = lists:delete(Elem, List),
    NewElems = [Elem | Elems],
    get_rand_elems(Amount, RestList, ListLen-1, NewElems, ElemsAmount+1).


-spec count_atom() -> R :: integer.
count_atom() ->
    Info      = erlang:system_info(info),
    Chunks    = binary:split(Info, <<"=">>, [global]),
    [TabInfo] = [X || <<"index_table:atom_tab", X/binary>> <- Chunks],
    Lines     = binary:split(TabInfo, <<"\n">>, [global]),
    Chunks2   = [list_to_tuple(binary:split(L, <<": ">>)) || L <- Lines, L =/= <<>>],
    binary_to_integer(proplists:get_value(<<"entries">>, Chunks2)).

