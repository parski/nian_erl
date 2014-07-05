nian_erl
========

Nian is a puzzle in the Swedish newspaper Svenska Dagbladet. It's laid out in a 3x3 grid with a letter in each box. 

![nian](http://cl.ly/image/1N2N2y3W340z/Screen%20Shot%202014-07-05%20at%2014.20.45%20.png)

With these letters your task is to assemble as many words as possible. Each letter may only be used once and the center letter must always be in the word. Words that use all nine letters are known as anagrams.

This program is an almost legendary assignment at the Department of Computer and Systems Sciences of Stockholm University. It is meant to be solved in Python but on a vacation I decided to implement it in Erlang.

To compile and run `nian_erl` you need Erlang/OTP. This program has only been tested on the following Erlang setup:

`Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]`

The easiest way to compile and run this in your own environment is to start up your trusty `Eshell` and run the following:

```
1> c(nian).
2> nian:solve("<INPUT>").
```

Here is a sample use case:

```
$ erl
Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.10.4  (abort with ^G)
1> c(nian).
{ok,nian}
2> nian:solve("anitsksem").

103 matches for input "anitsksem" in 85321 microseconds:

["anemisk","anis","anse","ansikte","as","ask","asket","ateism","eks","ensak",
 "ensam","ensamt","ess","est","estnisk","etisk","etnisk","etsa","ikast",
 "inkast","insats","inse","insekt","is","isa","ism","kasse","kast","kemist",
 "kines","kisa","kiss","kissa","kista","mas","mask","maskin","mast","mest",
 "minska","minst","miss","missa","misstanke","mista","nesa","sa","sak","same",
 "sams","samt","sank","sankt","sans","satin","sats","se","sekt","semantik",
 "semantisk","semi","sen","sena","senat","set","sia","sik","sikt","sikta",
 "sikte","sina","sinka","sist","sits","sk","ska","skam","skamsen","skans",
 "ske","sken","skena","skina","skit","skita","smak","smek","smeka","smeta",
 "smink","sminka","smita","st","stake","stam","stank","stek","steka","sten",
 "stena","stim","stinka","tes"]

Anagrams:

["misstanke","semantisk"]

ok
3>
```

Remember that `svenskaOrd.txt` needs to be in the same directory as `nian.erl`. The file name is hard coded but this implementation should work with English word lists as well although this is untested.
