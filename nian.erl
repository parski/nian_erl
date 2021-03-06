% Nian in Erlang
% Pär Strindevall

-module(nian).
-export([solve/1]).
-export([test/0, test_check_word/0, test_solve/0]). % I always export tests separately for flavor and glory.

% Tests

test() ->
	pass = test_check_word(),
	pass = test_solve(),
	pass.

test_check_word() ->
	true = check_word(true, "FOOBAR", "FOOBAR"),
	true = check_word(true, "BARFOO", "FOOBAR"),
	true = check_word(true, "FOOBAR", "FSOOBAR"),
	false, check_word(true, "FSOOBAR", "FOOBAR"),
	false, check_word(true, "F", "OOBAR"),
	false, check_word(true, "FOOBAR", "FOBAR"),
	pass.

test_solve() ->
	false = solve("FOUR"),
	false = solve("TOOMANYLETTERS"),
	pass.

% Type specification

-spec test() -> 'pass'.
-spec test_check_word() -> 'pass'.
-spec test_solve() -> 'pass'.
-spec solve([any()]) -> 'false' | 'ok'.
-spec start(non_neg_integer(),[any()],{non_neg_integer(),non_neg_integer(),non_neg_integer()}) -> 'false' | 'ok'.
-spec reader(pid() | {'file_descriptor',atom() | tuple(),_},[byte()],[string()],{non_neg_integer(),non_neg_integer(),non_neg_integer()}) -> 'ok'.
-spec done([string()],[byte()],{non_neg_integer(),non_neg_integer(),non_neg_integer()}) -> 'ok'.
-spec check_critical(string(),[byte(),...],[string()]) -> [string()].
-spec check_critical_result(boolean(),string(),[byte(),...],[string()]) -> [string()].
-spec check_word(boolean(),string(),[byte()]) -> boolean().
-spec check_list(boolean(),[string()],string()) -> [string()].

% Module functions

-define(CHARCOUNT, 9). % Change this macro value to turn Nian into Elvan, ... , N-an.
% If the CHARCOUNT value is changed the check_critical/3 function must be changed. This could probably be done with lists:nth((?CHARCOUNT - 1) / 2 + 1, Input) for uneven CHARCOUNT values but I try to avoid BIFs. The arithmetics will most likely evaluate with a decimal digit so formatting would need some tinkering.

% solve/1, the landing function. Feed it strings, please.
solve(Input) ->
	StartTime = now(),
	start(length(Input), Input, StartTime).

start(?CHARCOUNT, Input, StartTime) ->
	{ok, IO} = file:open("svenskaOrd.txt", [read, raw, read_ahead]),
	reader(IO, string:to_lower(Input), [], StartTime);
	
start(_, _, _) ->
	false.

reader(File, Checklist, Truths, StartTime) ->
	case file:read_line(File) of
		{ok, Line} ->
			Word = string:strip(Line, both, $\n),
			Truths1 = check_critical(Word, Checklist, Truths),
			reader(File, Checklist, Truths1, StartTime);
		eof ->
			done(Truths, Checklist, StartTime)
		end.
	
done(Truths, Checklist, StartTime) ->
	Time = timer:now_diff(now(), StartTime),
	Anagrams = [X || X <- Truths, length(X) == 9],
	io:format("~n~p matches for input ~p in ~p microseconds:~n~n~p~n~nAnagrams:~n~n~p~n~n",[length(Truths), Checklist, Time, Truths, Anagrams]).

% check_critical/3 checks if the current word contains the critical letter in Checklist.
check_critical(Word, Checklist, Truths) ->
	[_,_,_,_,A,_,_,_,_] = Checklist, % I love Erlang pattern matching.
	check_critical_result(lists:member(A, Word), Word, Checklist, Truths).

% check_critical_result/4 checks the whole word if its a match only if the word contains the critical letter.
check_critical_result(false, _, _, Truths) ->
	Truths;

check_critical_result(true, Word, Checklist, Truths) ->
	check_list(check_word(true, Word, Checklist), Truths, Word).

check_word(true, Word, _) when length(Word) == 0 ->
	true;

check_word(true, Word, Checklist) ->
	[H|_] = Word,
	check_word(lists:member(H, Checklist), lists:delete(H, Word), lists:delete(H, Checklist));

check_word(false, _, _) ->
	false. % Next row
	
check_list(true, Truths, Word) ->
	Truths++[Word];
	
check_list(false, Truths, _) ->
	Truths.