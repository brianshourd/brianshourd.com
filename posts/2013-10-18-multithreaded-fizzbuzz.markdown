---
title: Multithreaded Fizzbuzz in C++
author: Brian Shourd
date: October 18, 2013
tags: coding, c++
---

Here I am, practicing again. I've never really learned proper
multithreading techniques, and I'm refreshing my C++ skills, so I
decided (after much reading and several even smaller practice projects)
to write a simple multithreaded program.

You may be familiar with the classic FizzBuzz problem. The task is to
write a program that prints out every number from 0 to 100, except that
whenever the number is divisible by 3, it prints out "Fizz" instead, and
whenever it is divisible by 5, it prints out "Buzz" instead. If the
number is divisible by 15, it prints out "FizzBuzz". There is a natural
extension to arbitrary prime (or not) numbers with arbitrary words, e.g.
print "Baz" instead of 7.

To multithread this, we can create a new thread for each `number`/`word`
pair, whose sole job it is to find multiples of `number`. It will place
strings into a queue (either "" if the corresponding number is not
divisible by `number`, or `word` if it is), which is a shared data
structure, and must be protected by mutexes. Moreover, we don't want
this queue to overflow, so it must wait on a signal (from something
emptying the queue) if the queue fills up.

The other thread is a printer, which accesses each of the above queues,
pulls the next entry from them, and prints the appropriate result. So it
might pull "" and "Buzz" from the queues, indicating that the current
number is divisible by 5, but not by 3, and so will print "Buzz". Or it
might pull "" and "", indicating that the current number is relatively
prime to 3 and 5, and then just print the current number.

I decided to split this up as two data structures: 
* a `FizzQueue` class that automatically manages its own thread which
  fills itself up and features a single public method `pop`, retrieving
a single item. It also handles its own mutexes and signals, so that
`pop` can be called by other threads. On being destroyed, it cleans up
its thread, which joins the destroyer's thread.
* a `Printer` class that creates `FizzQueue` instances on demand through
  a public method `addPair(int, string)`. When `execute()` is run, it
will begin printing from the queues until a maximum is reached, at which
point it will destroy the created `FizzQueue` instances and be ready to
start anew.

It was a pretty extreme way to tackle the Fizzbuzz problem, and is
almost certainly extremely inefficient. But it taught me a lot about
handling threads, mutexes, conditions, and about creating objects that
run in their own threads.

You can [find the code on
Github](http://github.com/brianshourd/multithreaded-fizzbuzz).
