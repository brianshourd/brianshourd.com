---
title: Thing I Learned Today: Linking Through Many-to-Many Relationships
author: Brian Shourd
date: October 15, 2012
tags: coding, django
---

I'm new to relational databases. I'm not exactly sure how I missed the bus, but I'm seeing this all for the first time while going through the [Django Book](http://www.djangobook.com). The basics are cool - books have many-to-many relationships with authors (because a book may have many authors, and an author may have many books), and one-to-many relationships with publishers (because a book only has one publisher, but a publisher may have many books). The whole idea of using a very basic system of tables to try and encapsulate the data and relationships that we see in the world is fascinating.

But today I learned about [Django's `through` keyword](https://docs.djangoproject.com/en/dev/topics/db/models/#extra-fields-on-many-to-many-relationships) in models. It works in situations where we need to know not just that two things are in a many-to-many relationship, but exact details about that particular relationship.

In my app, it works like this: a `course` can have many `members`, and each `member` can belong to many `courses`. However, their `role` in the course may vary. So I link the `members` and `courses` tables through the `course_membership` table. This table contains a one-to-many reference to the `roles` table, as well as one-to-many references to the `course` and `member` tables.

In this way, I can have the same user belong to many different courses in different roles. As a grad student, I often switch between teaching a course, being an assistant for a course, or being a student in a course. Each of these is a `role`, and has different permissions in the app - in fact, the exact permissions of a given role depend on the course settings.
