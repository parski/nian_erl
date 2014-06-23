% Nian in Erlang
% Pär Strindevall

-module(nian).
-export([solve/1]).
-export([test/0, test_check_word/0, test_check_critical/0, test_solve/0]). % I always export tests separately for flavor and glory.

% Test

test() ->
	pass = test_check_word(),
	pass = test_check_critical(),
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

test_check_critical() ->
	true = check_critical("FOOBARBAR", "MMMMMMMMA"),
	false = check_critical("FOOBARBAR", "MMMMMMMMM"),
	pass.

test_solve() ->
	false = solve("FOUR"),
	false = solve("TOOMANYLETTERS"),
	pass.

% Type specification

% typer nian.erl

% Module functions

-define(CHARCOUNT, 9). % Change this macro value to turn Nian into Tian, Elvan, ... , N-an.

% solve/1, the landing function. Feed it strings, please.
solve(Input) ->
	{ok, IO} = file:open("svenskaOrd.txt", [read, raw, read_ahead]),
	Line = file:read_line(IO). % How do I read the next line?
	%check_critical(Input, Checklist).
	InputLower = string:to_lower(Input), % At first I had a bunch of to_lower calls in the loop arguments but I dislike BIFs so I added this row
	loop(length(InputLower), length(Line), check_word(true, Line, InputLower), Line, InputLower, [], []). %Change "kalle" to first word

reader(File, Checklist, Truths) ->
	case file:read_line(File) of
		{ok, Line} ->
			Truths1 = loop()
			reader(File, Checklist, Truths1);
		eof ->
			done(Truths)
		end.
	
done(Truths) ->
	Truths.

% check_critical/1 checks if the current word contains the critical letter in Input.
check_critical(Input, Checklist) ->
	[_,_,_,_,A,_,_,_,_] = Input, % I love Erlang pattern matching.
	lists:member(A, Checklist).

check_word(true, Word, _) when length(Word) == 0 ->
	true;

check_word(true, Word, Checklist) ->
	[H|_] = Word,
	check_word(lists:member(H, Checklist), lists:delete(H, Word), lists:delete(H, Checklist));

check_word(false, _, _) ->
	false. % Next row

% loop/x is the generically named brute that keeps the earth spinning.
loop(Length, _, _, _, _, _, _) when Length =/= ?CHARCOUNT ->
	false; % Input error

loop(_, 0, _, _, _, Truths, _) ->
	Truths; % eof. Placeholder? Close file?

loop(?CHARCOUNT, _, false, _, Checklist, Truths, Anagrams) ->
	NewWord = "", % Make this the next word
	loop(?CHARCOUNT, length(NewWord), check_word(true, NewWord, Checklist), NewWord, Checklist, Truths, Anagrams);

loop(?CHARCOUNT, ?CHARCOUNT, true, Word, Checklist, Truths, Anagrams) ->
	NewWord = "", % Make this the next word
	loop(?CHARCOUNT, length(NewWord), check_word(true, NewWord, Checklist), NewWord, Checklist, Truths, Anagrams++[Word]);

loop(?CHARCOUNT, _, true, Word, Checklist, Truths, Anagrams) ->
	NewWord = "nextword", % Make this the next word
	loop(?CHARCOUNT, length(NewWord), check_word(true, NewWord, Checklist), NewWord, Checklist, Truths++[Word], Anagrams).

% loop
% 1) grab word from row
% 2) check critical. if fail go to next if success check the word

% Results for input: "INPUT"
% PUT
% PINT
% PUN
% Anagrams:
% PUTIN
% PNUTI
% PINTU

% Check if check_word reacts on the first letter being wrong. Might have to fix

% Would be cool to have one result list and list comprehension that checks the result word length and prints it as an anagram if it's 9