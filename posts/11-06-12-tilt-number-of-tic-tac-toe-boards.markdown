---
title: Thing I Learned Today: Number of Tic-Tac-Toe Boards
author: Brian Shourd
date: November 6, 2012
tags: coding, haskell, math
math: true
---

I was looking at a problem about Tic-Tac-Toe on
[HackerRank](http://www.hackerrank.com) a few days ago, and it got me
working on a bot-building program using a genetic algorithm. A big part
of that was parameterizing the bots, so that I could give them some kind
of DNA. Now, this is my first time trying anything with genetic
algorithms, so I'm certainly doing this naively, but I thought - hey, I
could just enumerate *all* possible board states, and then each bot
would just be a list of moves corresponding to all possible board
states.

But how many boards are there?

Let's start with an estimate: a tic-tac-toe board has nine entries, and
each one can be either blank, X, or O. That's $3^9 = 19683$
different states. Of course, this is way too many - many of these board
states aren't legal. They could have, for instance, 5 Xs and only 2 Os,
which can't happen unless X cheats.

> Side note: When I was much younger, my mother bought us Tic-Tac-Toe
> Cheerios, which had both Xs and the iconic Os in the same box.
> Delightful. On the front of the box was, of course, a game of
> Tic-Tac-Toe, played with the cereal bits themselves, only it was
> wrong. It had such a state as I just described on it. My mother, bless
> her heart, encouraged me to write to General Mills and inform them of
> the predicament (I believe that I worded it "X cheated!"). To my
> surprise, they sent me back a coupon for a free box of any General
> Mills cereal and promptly cancelled Tic-Tac-Toe Cheerios forever. The
> latter is not likely my fault.

Ok, let's find a better estimate. Let's suppose that X goes first. There are 9 places for X to go. Then it is O's turn, and he/she has only 8 spots to choose from. So far, that makes $9 + 9 \times 8$ boards ($9$ having just one X, $9 \times 8$ having an X and an O). Continuing in this pattern, we find $\sum_{i=1}^9 \frac{9!}{(9-i)!} = 986409$ boards.

That's significatnly worse! What went wrong? Well, we are
double-counting boards here like nobody's business. Our estimate for the number of boards with 2 Xs and 1 O is $9 \times 8 \times 7 = 504$. But there are some ordering issues. This board will be counted
twice:

     X | O |   
    ---+---+---
       | X |   
    ---+---+---
       |   |

It will be counted once for X taking the first turn in the center, and
again if X's first turn is in the corner. And it just get's worse as
turns go on. We have to divide out by the number of ways that we can
reorder the X's. In this case, there are 2 of them, so it is 
$2!$ and there are only $252$ boards with 2 Xs and 1 O on them.
The total number of boards is then

$\sum_{i=0}^9 \frac{9!}{(9-i)! \lfloor \frac{i}{2} \rfloor ! \lceil \frac{i}{2} \rceil !} = 6046$

Much better. In fact, we're actually pretty close now.

But there are still too many. Why? Sometimes the game ends. In
particular, if somebody wins, the game ends. Many of the boards that we
counted with lots of Xs and Os actually have a 3-in-a-row in them, and
thus aren't really valid states. Actually, only the ones where *both*
players have 3-in-a-row are invalid. And counting these gets very
tricky.

So let's ask the computer to do it. I wrote a quick Haskell program to
do this and it found out that there are actuall only 5812 possible board
states.

We can reduce that more, though. Lot's of these boards are just
rotations or reflections of other boards. They're basically the same. If
we take this into consideration, then there are only 765 board states.

But wait, there's more! Lot's of these boards don't have any actual
decisions to make, since the game is either over or there is only one
move left. Eliminating those, we find 593 states.

And if we take away the boards with easy decisions - that is, boards
where there is a 'take the win' or 'block the win' option, we reduce it
still more. Only 96 boards remain. Here is the complete code:

~~~{.haskell}
import Data.List

newtype Board = Board [Char] deriving Eq

instance Show Board where
    show (Board xs) = ("\n" ++) . concat . intersperse "---+---+---\n" . map (\[x,y,z] -> [' ',x] ++ " | " ++ [y] ++ " | " ++ [z] ++ " \n") . groupByThrees $ xs

-- Ex: [0,1,2,3,4,5] -> [[0,1,2],[3,4,5]]
groupByThrees :: [a] -> [[a]]
groupByThrees [] = []
groupByThrees (x:y:z:xs) = ([x,y,z]):(groupByThrees xs)

-- Count the number of times a character occurs in a Board
count :: Board -> Char -> Int
count (Board xs) x = length . filter (==x) $ xs

-- Get all boards that can follow a given board,
-- none if the given board is a winning state or is
-- impossible
getNextBoards :: Board -> [Board]
getNextBoards b@(Board xs) = case (winner b, turn b) of
    (Left "Nobody", Right player) -> map (doMove player) . filter isLegalMove $ [0..8]
    other -> []
    where
        isLegalMove m = case (xs !! m) of
            ' ' -> True
            other -> False
        doMove player m = let (begin, end) = splitAt m xs in
            Board (begin ++ [player] ++ (tail end))

-- Returns Left "Nobody" if the board is illegal
-- Right player if it is player's turn
turn :: Board -> Either String Char
turn b = case ((count b 'X') - (count b 'O')) of
    0 -> Right 'X'
    1 -> Right 'O'
    other -> Left "Nobody"

-- Returns Left "Nobody" if nobody has won,
-- Left "Both" if both have won
-- or Right player if player has won
winner :: Board -> Either String Char
winner (Board xs) = case ((didWin 'X', didWin 'O')) of
    (True, False) -> Right 'X'
    (False, True) -> Right 'O'
    (False, False) -> Left "Nobody"
    (True, True) -> Left "Both"
    where
        didWin p = any ($p) [rowWin xs, colWin xs, diagWin xs]
        rowWin xs p = or . map (all (==p)) $ groupByThrees xs
        colWin xs p = rowWin (concat . transpose . groupByThrees $ xs) p
        diagWin xs p = (all (==p) . map (\n -> (groupByThrees xs) !! n !! n) $ [0,1,2]) || (all (==p) . map (\n -> (groupByThrees xs) !! n !! (2 - n)) $ [0,1,2])

-- Actually return a list of all possible Boards
getAllBoards :: [Board]
getAllBoards = concat . take 10 . iterate moreBoards $ [Board (take 9 $ repeat ' ')] where
    moreBoards :: [Board] -> [Board]
    moreBoards = nub . concat . map getNextBoards

-- Return a list of all possible boards, except that
-- boards which are reflections or rotations of one
-- another are considered the same
getAllBoards' :: [Board]
getAllBoards' = concat . take 10 . iterate moreBoards $ [Board (take 9 $ repeat ' ')] where
    moreBoards :: [Board] -> [Board]
    moreBoards = nubBy (\b1 b2 -> b1 `elem` (symmetries b2)). concat . map getNextBoards

-- Somewhat a misnomer - doesn't actually return the
-- symmetries of b, but all of the rotations and
-- reflections of b
symmetries :: Board -> [Board]
symmetries b = nub . map ($b) $ [i . j | i <- rotations, j <- reflections] where
    rotations :: [Board -> Board]
    rotations = take 4 . iterate (.r) $ id
    reflections :: [Board -> Board]
    reflections = take 2 . iterate (.m) $ id
    r (Board xs) = Board $ concat . transpose . map reverse . groupByThrees $ xs
    m (Board xs) = Board $ concat . map reverse . groupByThrees $ xs

-- Only get those boards for which a decision is to
-- be made (non-winning boards and boards with only one
-- or zero moves left)
getAllDecisions :: [Board]
getAllDecisions = filter condition getAllBoards' where
    condition b = (noWinner b) && ((count b ' ') > 1)

-- Determines if anyone has won the game yet
noWinner b = case (winner b) of
    Left _ -> True
    Right _ -> False

-- Now get only those boards for which a real decision
-- must be made. That is, eliminate boards where there is
-- a 'take the win' or 'block the win' move
getAllRealDecisions :: [Board]
getAllRealDecisions = filter (\b -> (condition1 b && condition2 b)) getAllDecisions where
    condition1 b = all noWinner $ getNextBoards b
    condition2 b = all noWinner . concat . map getNextBoards . getNextBoards $ b
~~~
