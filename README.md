# aoc2018
Advent of Code 2018:
Solutions in R and Python

Completed at 19:55, 5 January 2019

It was the first time I participated in Advent of Code and I have to say: it was an emotional rollercoaster. Naive as I was, after the first couple of days I thought it would be a little bit too easy to keep my interest. But then day 8 happened. This one really kept me up at night. I tried several approaches, hundreds of lines of code, manually created test inputs, but none of my solutions worked on the 'official' input and I just couldn't find out why not. After many brainbreaking hours of agony, suddenly the solution just popped into my head. I wrote about 15 lines of code in 5 minutes and it worked! This was the moment aoc really got me hooked. 

I tried solving every problem with just standard (and maybe pragmatic) R code. It was actually the first time in a couple of years using R, that I encountered problems which R is just not suitable for. Especially with larger strings or vectors, R is totally not efficient compared to other languages. Therefore a couple of solutions are written in Python. Using similar methods, Python was (in some cases) 1000 times faster than R. Obviously there were some tricks or packages I could use, but I also liked refreshing my Python-skills a bit. 

There was one problem I had no clue how to solve (day 22.2). Other people's solutions really opened my eyes for shortest-path-problems. Eventhough it was just a small variation to my own methods, it was a way of thinking I will really use for future problems (see my solution for more information). 

There were also a couple of problems where I PoC'd a solution but I didn't know for sure it was the one I should put time and effort in:
<li> Day 9.2: I knew the brute force method would work, but I also checked the forum if there were other solutions. I learned about linked lists and double ended queues that day. However, I just worked out my own solution in Python.
<li> Day 14.2: similar to 9.2, I checked if the brute force method was a common used method. It was, so I implemented it in python again.
<li> Day 17: I checked if people did this one manually (as I hoped). They did, so I did too. 
<li> Day 19: I checked if I had to put my time in reverse engineering the input or that there was just a trick I was missing. I actually was afraid of reverse engineering but in the end, day 19 was one of my favorite problems. Next time I just go on and reverse engineer this type of problems because I really enjoyed doing it. I was glad to see day 22 had a similar problem. 
<li> Day 23: after writing a pragmatic solution, which had a stochastic chance of working (or not working) I realized it was a typical minimization/maximization problem. It had been some time since I implemented such methods so I checked on the forum what maxi/minimization method they were using. Right after I checked I was a little bit sad I didn't had the perseverance to just solve it completely without help, as I already had the method ready. 

I really had a blast solving the problems of this Advent of Code and above all, I learned some new methods. Especially the shortest-path method, linked lists and double ended queues could really help me with future problems.

I added these notes so I can challenge myself next year
