# Tic-Tac-Toe

In this week you will learn about _adversarial games_ and
game AIs to implement OCaml 🐫 bots that play
[**tic-tac-toe**](https://en.wikipedia.org/wiki/Tic-tac-toe) and
[**Gomoku**](https://en.wikipedia.org/wiki/Gomoku).

In these exercises you will write a bot to play **TIC TAC TOE** and _improve_ your **TIC TAC TOE bot**!!

## Background

_Tic-tac-toe_ is a game in which two players take turns in placing either
an `O` or an `X` in one square of a **3x3** grid. The winner is the first
player to get **3** of the same symbols in a row.

_Gomoku_ (also commonly referred to as "Omok"), is very similar to tic-tac-toe,
but **bigger**. Two players play on a 15x15 board and the winner is the first
player to get **5** pieces in a row.

You can think of a digital tic-tac-toe board as a "mapping" of "position -> piece"
with the following types:

```ocaml

module Position : sig
  (* Top-left is [{row = 0; column = 0}].

     row indexes increment downwards.

     column indexes increment rightwards. *)
  type t =
    { row    : int
    ; column : int
    }
end

module Piece : sig
  type t =
    | X
    | O
end

```

For example, the board:

```
 X | - | - 
___|___|___
 - | O | X 
___|___|___
 - | O | - 
```

Can be represented as a mapping of:

```
(row, column)
  (0, 0) => X
  (1, 1) => O
  (1, 2) => X
  (2, 1) => O
```

What board does the following "mapping" represent? Is there anything interesting
happening? (Hint: If you were O, what move would you play?) Feel free to edit
the "Answer board" below:

```
(row, column)
  (1, 1) => X
  (0, 0) => O
  (0, 2) => O
  (2, 0) => X
  (2, 1) => X
  (2, 2) => O
```

Answer board:

```
 - | - | - 
 __|___|___
 - | - | - 
 __|___|___
 - | - | - 
```

After you've answered look for a fellow fellow near you and discuss your answers!

## Prep work

First, fork this repository by visiting [this
page](https://github.com/jane-street-immersion-program/game-strategies/fork) and clicking
on the green "Create fork" button at the bottom.

Then clone the fork locally (on your AWS machine) to get started. You can clone a repo on
the command line like this (where `$USERNAME` is your GitHub username):

```sh
$ git clone git@github.com:$USERNAME/game-strategies.git
Cloning into 'game-strategies'...
remote: Enumerating objects: 61, done.
remote: Counting objects: 100% (61/61), done.
remote: Compressing objects: 100% (57/57), done.
remote: Total 61 (delta 2), reused 61 (delta 2), pack-reused 0
Receiving objects: 100% (61/61), 235.81 KiB | 6.74 MiB/s, done.
Resolving deltas: 100% (2/2), done.
```

This repository contains several components:

```sh
.
├── lib
│   ├── main.ml
│   ├── main.mli
├── common
│   ├── game.ml
│   ├── game.mli
│   ├── game_kind.ml
│   ├── game_kind.mli
│   ├── piece.ml
│   ├── piece.mli
│   ├── position.ml
│   ├── position.mli
│   ├── evaluation.ml
│   ├── evaluation.mli
│   ├── ...
├── play
│   ├── ...
├── bin
│   ├── ...
└── README.md
```

You will be working wholly within the `lib` directory, though you'll be making frequent
references to items in the `common` directory, too.

## Exercises

You can think of an AI that plays tic-tac-toe board as a "function" of type
`me:Game.Piece.t -> game:Game.t -> Game.Position.t`.

The `me` parameter is the piece that you're bot is playing as. The `game` is the current
state of the game, most notably including the positions of all the previously-played
pieces. The output of this function is the position that your bot plays on this turn.

Over the course of these exercises you will be gradually building such a function.

Make sure you can build this repo:

```sh
dune build
```

(Feel free to run `dune runtest` but know that the tests do not currently suceed.)

### Exercise 0: Printing the board

Let's start by looking at the `win_for_x` and `non_win` values in `lib/main.ml`, which are
`Game.t`s. Make sure you understand this record and the items it comprises. To test your
understanding - and also to build up an important debugging tool - you'll need to
implement the `print_game` function (found in `lib/main.ml`. There are two expect tests 
which have been written for you; these are currently failing and will pass only when you 
correctly implement the `print_game` function. _(Hint: Consider `List.init`.)_

### Exercise 1: Where can I move?

Each turn, your AI _needs_ to make a decision of "which free available spot" it should
pick. Let's find "all free available spots." Implement `available_moves` in `lib/main.ml`.

```ocaml
val available_moves : Game.t -> Game.Position.t list
```

This function takes a game as input and returns a list of currently-available
positions. You can run this function on the two existing games with the command `dune exec
bin/game_strategies.exe one`. But note that `avaiblable_moves` is a _pure_ function. This 
makes it easy to test via an expect test, which you should write. In addition to the two 
existing games, can you think of a third game which would represent a good test?

### Exercise 2: Is the game over?

One crucial step in authoring our bots is examining a game and determining if the game is
over. To do this, implement `evaluate` function in `lib/main.ml`.

```ocaml
val evaluate : Game.t -> Game.Evaluation.t
```

The returned type represents all the possible states that a game can be in:

```ocaml
module Evaluation : sig
  type t =
    | Illegal_move
    | Game_continues
    | Game_over of { winner : Piece.t option }
end
```

Can you think of why the `winner` is the `Game_over` variant needs to be an option?

### Exercise 3: Is there a winning move?

Now we can really start to put things together. We can detect if the game is already over.
And we can get a list of available moves. One naive technique our bots could employ is to
play a random move from among the available ones. But we can do better than this! If there
is a move which causes us to win, we should probably play it! Discovering all the possible
winning moves is the task of the function `winning_moves` in `lib/main.ml`.

```ocaml
val winning_moves : me:Game.Piece.t -> Game.t -> Game.Position.t list
```

Given the piece we are meant to play, and given the game state, what are all the positions
which - if played - would win us the game? You can test this function via "exercise three"
on the command line, but because this function, too, is pure, you should write an expect
test as well! In addition to the two supplied games, what other games would represent
useful ones to test your `winning_moves` function?

### Exercise 4: Is there a losing move?

The last piece we'll need is the ability to find moves that would cause our bot's
_opponent_ to immediately win. Finding these losing moves is the task of the
`losing_moves` function in `/lib/main.ml`.

```ocaml
val losing_moves : me:Game.Piece.t -> Game.t -> Game.Position.t list
```

This function should return all the moves that immediately lose for the piece specified in
the `me` argument. The good news is that you already have implemented a function which
finds the _winning_ moves for a given piece. Can you use `Piece.flip` to put this
together? Once again, there is a command line instruction to run this example but you
should write an expect test for this function, too.

### Exercise 5: Making moves

Implement `make_move` in `lib/main.ml`. Try using the other functions you have written
to implement a simple bot. Write some expect tests to determine that your bot works as
expected.

### Playing the game

To play the game against another user (or your bot against itself!), first have both players
wait for a game. Note that if both players are on the same machine they must use different
ports. Try `1025` and `1026`:

```
dune exec bin/play.exe -- wait-for-game -port 1025
dune exec bin/play.exe -- wait-for-game -port 1026
```

And then start a game with the following command (replacing screen names as appropriate):

```
dune exe bin/play.exe -- start-game -x localhost:1026 -o localhost:1026 -x-name kelvin -o-name melvin
```

To play omok, add the `-omok` flag to the `start-game` command above.

If a player is on your AWS machine you can use `localhost` as their IP. Otherwise you can get the 
local IP address of AWS machine with:

```
curl http://169.254.169.254/latest/meta-data/local-ipv4
```

These servers will open a graphics window to show the game and call your `make_move`
implementations in `lib/main.ml` to determine moves. Go head-to-head against your fellow fellows!

### Exercise 6: One move ahead

Now that we can detect all the moves available to us and figure out which game states will
cause us to immediately lose, let's write a function to look one move ahead. There is no
scaffolding for this function; you're all on your own.

Write a function called `available_moves_that_do_not_immediately_lose`. As with the
`winning_moves` and `losing_moves` functions, this one should take a `Piece.t`
argument and a `Game.t` argument and return a list of `Position.t`s. The idea is we
want to find all the moves which are legal _AND_ which will not let the opponent win on
the next move (assuming perfect play).

You should write a `Command` for this exercise, similar to the others. Additionally, you
should write at least one expect test for this function. Make sure you devise `Game`s
which illustrate a variety of different situations.

### Exercise 7: Even more moves ahead

We've never mentioned its name thus far, but we've been secretly been implementing
a version of the algorithm ["Minimax"](https://en.wikipedia.org/wiki/Minimax). It
was one of the earlier algorithms to beat humans at complex games like chess,
and it's also the algorithm we'll be implementing next.

Thus far we've implemented an algorithm that can look **1** move into the future
and if it can win/not lose into the future, it can make the best decision. One
immediate idea is:

- What if we look **further** into the future?

Take a moment to intuitively think, for yourself on a piece of paper/on a text
file, if you could look **2** moves into the future on tic tac toe/how would
you pick your next move? What about **3**? Discuss with another fellow or if no
one is available talk to a TA!

Minimax has a couple of parts:

- **score**: What is the current "score" of the game's position? This is a bit
  silly on a game like tic-tac-toe/omok, but you could think of "winning" as
  +infinity score and losing as -infinity score. You could also come up with
  heuristics like, if I see "2" pieces together, then I add 4, if my opponent has
  4 pieces together, then I subtract 50. Or even if I have n pieces together, I
  add `n*n` to my score.
- **maximizing** player: You win when your score reaches +infinity, so you want
 to **maximize** the score.
- **minimizing** player: Your opponent wins when the score reaches -inifinity, so
  they will always want to **minimize** the score.
  
Minimax assumes that both players will play optimally, so minimax assumes that
the **maximizing** player will always pick the move that results in the maximum
score, and that the **minimizing** player picks the smaller score.

Minimax operates under a "max-depth" it will travese to it will take as a parameter
"how many moves into the future" it should try to evaluate. It will then build
a tree like the ones shown in this [wikipedia page](https://en.wikipedia.org/wiki/Minimax)
and run the **score** function on "terminal" nodes that are nodes where
either the game ends (score is infinity) or where the max depth has been reached.

You can use your already implemented `evaluate` function to implement a `score`
function. A very basic version of score can be -infinity for loss, +infinity for win, [0.0] for the game continuing.

Read the pseudocode on minimax from [wikipedia](https://en.wikipedia.org/wiki/Minimax#Pseudocode).

**What is a "terminal node"?**

A terminal node is the game ending by someone winning/losing
or by the game tie'ing due to all of the slots being filled.

**What is the "heuristic value of node"?**

It would be the `score` function you just implemented.

**What is the "child of node"**?

The `available_moves` function

If you have any questions please ask a TA!

Implement minimax by following the pseudocode from wikipedia! Can you solve the entire game?!

### More

This is a list of possible extensions you could implement:

- Change [available_positions] to only pick empty positions that are next to occupied
  pieces to lower the search space. Does the depth you can explore increase?
- Make your score function "heuristic based" by scoring 2/3/4 consecutive pieces
  an "n*n" score.
- Implement a minimax optimization called [alpha beta
  pruning](https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning). Does the depth
  that you can explore increase?
- Make evaluation super fast by using [bit
  masks](https://medium.com/@LukeASalamone/creating-an-ai-for-gomoku-28a4c84c7a52).
- Pick different heuristics for your [score] function.

### Trouble shooting

As you play your game you will find yourself starting and killing lots of servers. If you
lose track of a server that you think you killed, and you seen error like the following 
while trying to start a new one

```
(monitor.ml.Error                     
 (Unix.Unix_error "Address already in use" bind
  "((fd 7) (addr (ADDR_INET 0.0.0.0 1025)))")
...
```

Run the following with the appropriate port to kill the process that is currently bound 
to that port.

```
sudo kill -9 $(sudo lsof -t -i :1025)
```