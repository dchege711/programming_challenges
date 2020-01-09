---

layout: writeup
title: To-Do List (Easy)
date: 2019-11-03
type: writeup

---

## Description

[Link to original Reddit submission](https://www.reddit.com/r/dailyprogrammer/comments/39ws1x/20150615_challenge_218_easy_todo_list_part_1/)

> Today's challenge will be something slightly different! Atleast I think the
> challenge is meant to be for today? Wait, am I meant to even be submitting today?
>
> Okay maybe I need some help on organising my thoughts before I submit my
> challenge. A to-do list would be fine, just something so that I can organise my
> thoughts!
>
> It should have the following basic functionality

## Formal Inputs & Outputs

### Output description

> Any output that is created should be user-friendly. When I'm viewing my to-do
> list, I should be able to easily discern one list item from another.

#### Examples

Input:

```java
addItem('Take a shower');
addItem('Go to work');
viewList();
```

Output:

```text
Take a shower
Go to work
```

Further Input:

```java
addItem('Buy a new phone');
deleteItem('Go to work');
viewList();
```

Outputs:

```text
Take a shower
Buy a new phone
```
