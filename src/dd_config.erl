%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%% Copyright 2012, 2013 Gert Meulyzer

%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @doc
%%% The configuration module for dd.
%%% @end
%%% Created : 19 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_config).
-include("../include/dd_irc.hrl").

-export([load_config/0, convert_config/1, need_to_convert_config/0, make_config/0, write_example_config/1]).
-compile(export_all).

%% config should look like this:
%% {server, "Freenode", "irc.freenode.net", 6667, "dingd2ng", [ "#yfb", "#testerlounge" ], [dd_morning, dd_version, dd_url_handler, dd_le_handler, dd_command_handler] }.
%% we want to convert this to:
%% {server, "Freenode", "irc.freenode.net", 6667, "dingd2ng", [{"#yfb", [dd_morning, dd_version, dd_url_handler, dd_le_handler, dd_command_handler]},
%%                                                             {"#testerlounge", [dd_morning, dd_version, dd_url_handler, dd_le_handler, dd_command_handler]}]}.

load_config() ->
    {ok, ServerConfigs} = application:get_env(dd, serverconfigs),
    [ #serverconfig{name     = Name, 
                    hostname = HostName, 
                    port     = Port, 
                    nick     = Nick, 
                    pass     = Pass, 
                    sasl     = Sasl, 
                    nickserv = NickServ, 
                    ssl      = Ssl, 
                    channels = Channels, 
                    modules  = Modules}
                    || {server, 
                        Name, 
                        HostName, 
                        Port, 
                        Nick, 
                        Pass, 
                        Sasl, 
                        NickServ, 
                        Ssl, 
                        Channels, 
                        Modules} <- ServerConfigs ].   


need_to_convert_config() -> false.

write_example_config(Filename) ->
    Config = [{dd, [{serverconfigs, [ generate_serverconfig("Freenode", "irc.freenode.net", 6667, "d3ngd3ng", "", false, "", false, [ {"#yfb", [ dd_url_handler, dd_command_handler ]}], [dd_freenode]) ]},
                    {tweeters, ["some_tweeter"]},
                    {oauth,
                     [{access_token, "MY_ACCESS_TOKEN"},
                      {access_token_secret, "MY_ACCESS_TOKEN_SECRET"},
                      {api_key, "MY_API_KEY"},
                      {api_key_secret, "MY_API_KEY_SECRET"}]}]}],
    file:write_file(Filename, io_lib:fwrite("~p.\n",[Config])).

convert_config(Filename) ->
    {ok, [Cfg]} = file:consult(Filename),
    [{dd, Config}] = Cfg,
    Servers = proplists:get_value(serverconfigs, Config),
    Tweeters = proplists:get_value(tweeters, Config),
    Oauth = proplists:get_value(oauth, Config),
    [{dd, [{serverconfigs, [ convert_server_config(Server)  || Server <- Servers ]},
           {tweeters, Tweeters},
           {oauth, Oauth}]}].

convert_server_config({server, Name, Host, Port, Nick, Pass, Sasl, NickSrv, Ssl, Chans, Mods}) ->    
    io:format("New style config, post-sasl~n"),
    generate_serverconfig(Name, Host, Port, Nick, Pass, Sasl, NickSrv, Ssl, convert_channels(Chans), Mods);
convert_server_config(_) ->
    io:format("Don't know this type of server config, does dingding even start?"),
    [].

convert_channels(Channels) ->
    [ convert_chan(Chan) || Chan <- Channels ].

convert_chan({Name, ModuleList}) ->
    [{name, Name},
     {modules, ModuleList}].

make_config() ->
    ec_talk:say("Generating config file for dingding"),
    Filename = ec_talk:ask("What file should I write this config to?"),
    io:format("Writing to ~s~n",[Filename]),
    ask_for_serverconfig().
    
generate_serverconfig(Name, Hostname, Port, Nick, Pass, Sasl, NickPass, SSL, Chans, Mods) ->
    [{name, Name},
     {host, Hostname},
     {port, Port},
     {nick, Nick},
     {pass, Pass},
     {sasl, Sasl},
     {nickserv_password, NickPass},
     {ssl, SSL},
     {channels, Chans},
     {modules, Mods}].

ask_for_serverconfig() ->
    Name = ec_talk:ask("How do you want to name this connection?"),
    Host = ec_talk:ask("IP or hostname of the server"),
    Port = ec_talk:ask("Port", number),
    Ssl = ec_talk:ask("Do you want to connect through SSL? (Y/N)", boolean),
    Nick = ec_talk:ask("Bot nickname (this is also your nickserv login)"),
    WantSasl = ec_talk:ask("Do you want SASL authentication? (Y/N)", boolean),
    {Pass, NickServPass} = 
        case WantSasl of
            true -> 
                {ec_talk:ask("Server password"), ec_talk:ask("NickServ password")};
            false ->
                {[], []}
        end,
    ec_talk:say("We'll configure channels now"),
    NrOfChans = ec_talk:ask("How many channels do you want to configure?", number),
    Channels = [ ask_for_channel(Nr)  || Nr <- lists:seq(1, NrOfChans) ],
    Mods = [],
    generate_serverconfig(Name, Host, Port, Nick, Pass, WantSasl, NickServPass, Ssl, Channels, Mods).

prompt_for_bool(Prompt, TrueValue) ->
    case ec_talk:ask(Prompt++" (Y/N)", boolean) of
        true -> TrueValue;
        false -> []
    end.

ask_for_channel(Nr) ->
    io:format("Configuring channel ~p~n",[Nr]),
    Name = ec_talk:ask("Channel Name"),
    Urls = prompt_for_bool("Handle URLs on this channel?", dd_url_handler),
    Commands = prompt_for_bool("Handle commands on this channel?", dd_command_handler),
    {Name, lists:flatten([Name, Urls, Commands])}.
    
    
