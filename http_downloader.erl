-module(http_downloader).

-export([start_server/1, init_server/1, start_client/1, read_zips_from_file/1, stats/0, remove_dups/1]).
-export([test/0, test/1]).

stats() ->
    http_downloader_server ! {stats, self()},
    receive
        {stats, Vals} ->
            Vals
    end.

read_lines(Input) ->
    read_lines(Input, io:fread(Input, '', "~d"), []).

read_lines(_, eof, Accum) ->
    {ok, Accum};
read_lines(Input, {ok, [Data]}, Accum) when is_integer(Data) ->
    read_lines(Input, io:fread(Input, '', "~d"), [Data | Accum]).

read_zips_from_file(Filename) ->
    {ok, Input} = file:open(Filename, read),
    {ok, Zips} = read_lines(Input),
    file:close(Input),
    Zips.

read_pics_dir() ->
    {ok, Files} = file:list_dir("pics"),
    [element(1, string:to_integer(hd(string:tokens(X, ".")))) || X <- Files].

remove_dups(List) ->
    remove_dups(read_pics_dir(), List, []).

remove_dups(_, [], Accum) ->
    Accum;
remove_dups(Pics, [Z | Zips], Accum) ->
    case lists:member(Z, Pics) of
        true ->
            remove_dups(Pics, Zips, Accum);
        false ->
            remove_dups(Pics, Zips, [Z | Accum])
    end.

generate_seed() ->
    {S1, S2} = statistics(reductions),
    {S3, _} = statistics(reductions),
    {S1, S2, S3}.

seed_random({S1, S2, S3}) ->
    random:seed(S1, S2, S3).

wait_random() ->
    case random:uniform(4) of
        1 ->
            timer:sleep(timer:seconds(random:uniform(5)));
        2 ->
            timer:sleep(timer:seconds(5 + random:uniform(10)));
        3 ->
            timer:sleep(timer:seconds(10 + random:uniform(15)));
        4 ->
            timer:sleep(timer:seconds(60 + random:uniform(20)))
    end.

user_agent_random() ->
    case random:uniform(4) of
        1 ->
            "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)";
        2 ->
            "Mozilla/5.0 (compatible; Yahoo! Slurp; http://help.yahoo.com/help/us/ysearch/slurp)";
        3 ->
            "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR1.1.4322)";
        4 ->
            "Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.0 (like Gecko)"
    end.

zip_to_string(Zip) ->
    lists:map(fun ($\s) -> $0; (A) -> A end, lists:flatten(io_lib:format("~5B", [Zip]))).

start_server(Zips) ->
    register(http_downloader_server, spawn_link(?MODULE, init_server, [Zips])).

init_server(Zips) ->
    loop_server([], [], [], Zips, []).

loop_server(Clients, [], Errors, [], ZipsComplete) ->
    finish_clients(Clients),
    {ok, Outfile} = file:open("output.log", write),
    io:format(Outfile, "~w~n~w~n~w~n~w~n", [length(ZipsComplete), ZipsComplete, length(Errors), Errors]),
    file:close(Outfile);
loop_server(ClientsIdle, ClientsRun, Errors, Zips, ZipsComplete) ->
    receive
        {client_new, Pid} ->
            io:format("new client~n", []),
            loop_server([Pid | ClientsIdle], ClientsRun, Errors, Zips, ZipsComplete);
        {client_ready, Pid} ->
            io:format("client ready~n", []),
            case lists:member(Pid, ClientsIdle) of
                true ->
                    Pid ! {zip_new, hd(Zips)},
                    loop_server(lists:delete(Pid, ClientsIdle), [Pid | ClientsRun], Errors, tl(Zips), ZipsComplete)
                %% _ ->
                %% loop_server(ClientsIdle, ClientsRun, Errors, [Z | Zips], ZipsComplete)
            end;
        {zip_done, Pid, Zip, Res} ->
            case lists:member(Pid, ClientsRun) of
                true ->
                    write_zip(Zip, Res),
                    Pid ! zip_success,
                    loop_server([Pid | ClientsIdle], lists:delete(Pid, ClientsRun), Errors, Zips, [Zip | ZipsComplete])
                %% _ ->
                %% loop_server(ClientsIdle, ClientsRun, Errors, [Z | Zips], ZipsComplete)
            end;
        {zip_error, Pid, Zip, Error} ->
            io:format("Zip failed: ~s~n", [zip_to_string(Zip)]),
            loop_server([Pid | ClientsIdle], lists:delete(Pid, ClientsRun), [{Zip, Error} | Errors], Zips, ZipsComplete);
        {stats, Pid} ->
            Pid ! {stats, {{clientsrun, length(ClientsRun)}, {clientsidle, length(ClientsIdle)}, {errors, length(Errors)},
                           {zips, length(Zips)}, {zipscomplete, length(ZipsComplete)}}},
            loop_server(ClientsIdle, ClientsRun, Errors, Zips, ZipsComplete)
    after
        timer:hours(2) ->
            loop_server(ClientsIdle, [], [{timeout, {ClientsRun, Zips}} | Errors], [], ZipsComplete)
    end.

finish_clients([]) ->
    ok;
finish_clients([H | R]) ->
    H ! server_done,
    finish_clients(R).

write_zip(Zip, Data) ->
    {ok, Output} = file:open(io_lib:format("pics/~s.gif", [zip_to_string(Zip)]), write),
    io:put_chars(Output, Data),
    file:close(Output).

start_client(Server) ->
    seed_random(generate_seed()),
    Server ! {client_new, self()},
    loop_client(Server, 0).

loop_client(Server, Zipcount) ->
    Server ! {client_ready, self()},
    receive
        {zip_new, Zip} ->
            wait_random(),
            case catch download_zip(Zip) of
                {'EXIT', Reason} ->
                    Server ! {zip_error, self(), Zip, Reason};
                {ok, Data} ->
                    io:format("Download success~n", []),
                    Server ! {zip_done, self(), Zip, Data}
            end,
            loop_client(Server, Zipcount + 1);
        server_done ->
            Zipcount
    end.
   

download_zip(Zip) ->
    {ok, {{_, 200, _}, _, Data}} = http:request(get,
                                                {lists:flatten(io_lib:format("http://web03.bestplaces.net/zip/~s.gif", [zip_to_string(Zip)])), [{"user-agent", user_agent_random()}]}, [], []),
    {ok, Data}.

test() ->
    download_zip(00001).

test(pics) ->
    read_pics_dir().
