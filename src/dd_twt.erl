%%%-------------------------------------------------------------------
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2015, Gert Meulyzer
%%% @doc
%%% This is the new Twitter module for DingDing.
%%% It will be a gen_server that serves up the twitter functionality.
%%% @end
%%% Created : 10 Jan 2015 by Gert Meulyzer <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(dd_twt).

-behaviour(gen_server).
-compile(export_all).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(TIMEOUT, 60*5*1000).
-define(SingleTweetPattern, "https?://twitter.com/(\\w*)/status\\w?\\w?/(\\d*)").

-define(SERVER, ?MODULE).

-record(tweet, {nick,
				name,
				text,
				id}).

-record(state, {
		 subscribers=[]
		 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	%% Create a new ets table for keeping the list of tweets in.
	%% This does not need to be persisted to disk.
	ets:new(twets, [set, named_table]),
	%% When we start up, go get the latest mention IDs and store those
	%% as being "the last ones seen". This way we won't spam the channel
	%% on startup.
	get_and_store_mentions(),
	%% By returning a timeout value here, the handle_info/2 function
	%% will get called after the timeout.
	{ok, #state{}, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(subscribe, From, #state{subscribers=Subs}=State) ->
	NewSubs = [From|Subs],
	{reply, ok, State#state{subscribers=NewSubs}, ?TIMEOUT};
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
	{noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{}=State) ->
	get_mentions_and_return_unseen_ones(),
	{noreply, State, ?TIMEOUT};
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% ETS table helper functions
%%--------------------------------------------------------------------
-spec store_tweet(Tweet) -> true when
	  Tweet :: #tweet{}.
store_tweet(#tweet{id=ID}=T) ->
	ets:insert(twets, {ID, T}).

get_tweet_by_id(ID) ->
	ets:lookup(twets, ID).

%%--------------------------------------------------------------------
%% Twitter specific functions
%%--------------------------------------------------------------------
get_mentions_and_return_unseen_ones() ->
	Mentions = get_mentions(),
	[ case have_tweet(Tweet) of 
		  true -> report_tweet(Tweet);
		  false -> ok
	  end
		  || Tweet <- Mentions ].

report_tweet(_Tweet) ->
	ok.

have_tweet(#tweet{id=ID}) ->
	case ets:lookup(twets, ID) of
		[] -> false;
		[_] -> true
	end.

get_and_store_mentions() ->
	Mentions = get_mentions(),
	[ io:format(tweet_to_line(Mention)++"~n") || Mention <- Mentions ],
	[ store_tweet(Mention) || Mention <- Mentions ].

get_mentions() ->
	JSON = oauth_twitter:get_mentions(1),
	{array, Tweets} = mochijson:decode(JSON),
	[ get_usertimeline_tweet(Twt) || Twt <- Tweets ].

-spec get_usertimeline_tweet({'struct', [any()]}) -> #tweet{}.
get_usertimeline_tweet({struct, Twt}) ->
    {struct, User} = proplists:get_value("user", Twt),
    Nick = proplists:get_value("screen_name", User),
    Name = proplists:get_value("name", User),
    Text = dd_helpers:cleanup_html_entities(proplists:get_value("text", Twt)),
	Id = proplists:get_value("id", Twt),
	#tweet{nick=Nick, name=Name, text=Text, id=Id}.

get_tweet(TweetID) when is_binary(TweetID) ->
    get_tweet(binary_to_list(TweetID));
get_tweet(TweetID) ->
	JSON = oauth_twitter:get_tweet(TweetID),
    get_usertimeline_tweet(mochijson:decode(JSON)).

tweet_to_line(#tweet{id=ID, name=Name, nick=Nick, text=Text}) ->
	io_lib:format("~s (~s): ~s [~p]", [Name, Nick, Text, ID]).

get_two_tweets_for_user(Username) when is_binary(Username) ->
	get_two_tweets_for_user(binary_to_list(Username));
get_two_tweets_for_user(Username) ->
	JSON = oauth_twitter:get_twitter_user_timeline(Username),
    {array, TwtList} = mochijson:decode(JSON),
	[ get_usertimeline_tweet(Twt) || Twt <- TwtList ].

%%--------------------------------------------------------------------
%% URL helper functions
%%--------------------------------------------------------------------
-spec is_tweet(URL :: binary()) -> {true, binary()} | false.
is_tweet(URL) ->
    {ok, Regex} = re:compile(?SingleTweetPattern, [caseless]),
    case re:run(URL, Regex, [{capture, all_but_first, binary}]) of
        {match, [_, TweetID]} ->
            {true, TweetID};
        _ -> false
    end.

%%--------------------------------------------------------------------
%% People allowed to tweet -- from the config file.
%%--------------------------------------------------------------------
nick_allowed_to_tweet(Nickname) when is_binary(Nickname) ->
    nick_allowed_to_tweet(binary_to_list(Nickname));
nick_allowed_to_tweet(Nickname) ->
    {ok, WhiteList} = application:get_env(dd, tweeters),
    lists:member(Nickname, WhiteList).

return_id(RBody) ->
    {struct, Tweet} = mochijson:decode(RBody),
    proplists:get_value("id", Tweet).


tweet(Nickname, Text) when is_binary(Nickname) ->
    tweet(binary_to_list(Nickname), Text);
tweet(Nickname, Text) when is_binary(Text) ->
    tweet(Nickname, binary_to_list(Text));
tweet(Nickname, Text) ->
    case nick_allowed_to_tweet(Nickname) of
        true -> sendtweet(Text);
        false -> disallowed
    end.

sendtweet(Text) ->
	return_id(oauth_twitter:send_tweet(Text)).
