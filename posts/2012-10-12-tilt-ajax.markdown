---
title: Thing I Learned Today: AJAX
author: Brian Shourd
date: October 12, 2012
tags: coding
---

Today I learned what AJAX, the buzzword I've been hearing for so long, really is. I always thought that it was a programming language, one of the many that I've never dealt with in the foreign land of web programming. But it turns out that AJAX stands for [Asynchronous Javascript and XML](http://www.w3schools.com/ajax/default.asp), and is just a method/pattern for getting data to and retrieving data from a server without refreshing a web page.

It's a good thing, since this is exactly what I need to do for the project that Hank and I are working on. The project is called MathMail, and it's going to be a Django app geared towards letting students ask math questions of their teachers using real math notation online. A key part is an in-browser visual equation editor, which needs to update a live preview. I'm thinking that's a very AJAXy thing to do. We'll probably look into jQuery, which is another buzzword JS library that I know basically nothing about (although I learned today that it implements AJAX, among other things).
