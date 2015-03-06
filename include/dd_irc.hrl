%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-ifndef(PRINT).
-define(PRINT(Var), io:format("~n|| DEBUG: ~p:~p - ~p~n|| ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-define(U(X), unicode:characters_to_binary(X)).


-record(ircmsg, {prefix = <<>>, 
                 command = <<>>, 
                 arguments = [], 
                 tail = <<>>}).

-record(serverconfig, {name="unnamed",
                       hostname="chat.freenode.net",
                       port=6667,
                       nick="dingd1ng",
                       pass=false,
                       sasl=false,
                       nickserv=false,
                       ssl=false,
                       channels=[],
                       modules=[],
                       dbpid
                      }).

-record(channelconfig, {name="#channel",
                        modules=[]}).

