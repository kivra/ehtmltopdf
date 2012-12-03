-module(ehtmltopdf).
%%
%% Brokenness: at the time of writing, webkit doesn't support page-break-before,
%%   page-break-after or page-break-inside for inline elements so use block
%%   elements instead. https://bugs.webkit.org/show_bug.cgi?id=5097
%%
%% I'm trying to write this as basically as possible, yet trying not to assume
%% wkhtmltopdf as the actual PDF generating tool, e.g. giving the ehtmltopdf opt
%% 'print_media' results in the parameter --print-media-type to the wkhtmltopdf
%% call. On the other hand it's not worth generalizing for the entire world of
%% PDF tools right now.
%%
%% Lesson learnt: CSS page-break-before is unrelated to the print media type
%% unless you specifically restrict that CSS to the print media type. Probably
%% obvious but not immediately to me :)
%%
%% The version of wkhtmltopdf used right now is "wkhtmltopdf 0.10.0 rc2"

-export([convert/2, test/0]).

%% wkhtmltopdf doesn't seem to work until stdin is closed. Since Erlang ports
%% can't close the pipe, we use the 'wrapper' script to close stdin to wkh..
%% when this token is seen. Not elegant but works and fast enough for now.
-define(EOF,
        "erlang-port-eof-kh9823rheifn8hn398hc3489c4hc489crn3983c238cbb39c").

convert(HTML, Opts) ->
    PrintOpt = lists:member(print_media, Opts),
    PrintOptStr = if
                      PrintOpt -> "--print-media-type";
                      true -> ""
                  end,
    %% Wrap in quotes so wkhtmltopdf's args is exactly one arg to wrapper.
    WHTPOptStr = string:join(["\"", PrintOptStr, "\""], " "),
    {ok, WHTPPath} = application:get_env(ehtmltopdf, wkhtmltopdf_path),
    WrapperPath = code:priv_dir(ehtmltopdf) ++ "/wrapper",
    Command = string:join([WrapperPath, WHTPPath, WHTPOptStr], " "),
    PortOpts = [stream, use_stdio, exit_status, binary],
    Port = erlang:open_port({spawn, Command}, PortOpts),
    erlang:port_command(Port, [HTML,"\n",?EOF,"\n"]),
    %% crash upon non-zero exit status
    {ok, Data, 0} = receive_until_exit(Port, []),
    case erlang:port_info(Port) of
        undefined -> ok;
        _ -> true = erlang:port_close(Port)
    end,
    {ok, Data}.

receive_until_exit(Port, ReverseBuffer) ->
    receive
        {Port, {exit_status, Status}} ->
            Data = iolist_to_binary(lists:reverse(ReverseBuffer)),
            {ok, Data, Status};
        {Port, {data, Data}} ->
            receive_until_exit(Port, [Data | ReverseBuffer])
    end.

test() ->
    HTML =
        "<html><body>"
        "<h1>hello</h1>world"
        "<div style=\"page-break-before: always;\">on next page</div>blah"
        "</body></html>",
    {ok, PDF} = convert(HTML, []),
    file:write_file("test.html", HTML),
    file:write_file("test.pdf", PDF).
