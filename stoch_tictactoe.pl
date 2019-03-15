:- module(stoch_tictactoe, [move/2,utility/2,winPos/4,drawPos/3,strat_at/2,
                            chance_to_move/1,chance_of/3,eval/2]).

:- use_module(winpos).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(aggregate)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Explain here what the assumptions being made in tictactoe.pl (which
% won't carry over) are, as explained in the handout.
% 
% a) The cut after winPos assumes that the player making the move is the only
% one that can win from the move, but with the stochastic version it is possible
% that one player can make a move with the other player's piece and win for them
%
% b) The utility predicate only considers the 3-item game state, not considering
% the token being used by the current player and therefore whether a win state
% for a certain token is a good thing for the current player
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% True if there is a legal (according to rules) move from Pos to NextPos.
% nextPlayer is implemented in the engine.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement move. Remember to include a case characterising legal moves from
% chance positions. If chance is playing then both the first and second argument
% to the position (aka game state) are the same, i.e.:
%   [chance(P), chance(P), ...].
% The second to last and last arguments to the game state should always remain
% the same, i.e.:
%   move([Player,     PlayingAs,     State,     Board,     Dim, N],
%        [NextPlayer, NextPlayingAs, NextState, NextBoard, Dim, N])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% move(+Pos, -NextPos)

% get_time(Before), test([x,o,play,[0,0,0, 0,0,0, 0,0,0], 3, 3], B, V), get_time(After), Elapsed is floor(After-Before).
% get_time(Before), test([o,o,play,[0,0,0, 0,0,0, 0,o,0], 3, 3], B, V), get_time(After), Elapsed is floor(After-Before).
% profile(test([x,x,play,[0,0,0, 0,o,0, 0,o,0], 3, 3], B, V)).
% profile(test([x,x,play,[x,o,x, o,o,0, o,x,0], 3, 3], B, V)).

% Cutoff tests
% play([x, x, play, [o,o,0,0, 0,x,0,0, 0,x,0,o, 0,o,x,0], 4, 4], 4, x).
% play([x, x, play, [o,o,0,0, x,x,0,0, 0,x,0,o, 0,o,x,0], 4, 4], 4, x).
% play([x, x, play, [o,o,0,0, x,x,0,0, 0,x,o,o, o,o,x,0], 4, 4], 4, x).
% play([x, x, play, [0,0,0,o, 0,x,0,0, 0,x,0,o, x,o,0,0], 4, 4], 3, x).

move([_, _, win |_], _) :- fail, !.

move(Pos, NextPos) :-
    chance_to_move(Pos), !,
    move_chance(Pos, NextPos).

% Player can make move that wins for that player
move([Player, PlayingAs, play, Board,     Dim, N], 
     [NextP,  PlayingAs, win,  NextBoard, Dim, N]) :-
        \+ chance_to_move([Player, PlayingAs, play, Board, Dim, N]),
        nextPlayer(Player, NextP),
        move_aux(PlayingAs, Board, NextBoard),
        winPos(PlayingAs, NextBoard,_,_).

% Player can cause a draw
move([Player, PlayingAs, play, Board,     Dim, N], 
     [NextP,  PlayingAs, draw, NextBoard, Dim, N]) :-
        \+ chance_to_move([Player, PlayingAs, play, Board, Dim, N]),
        nextPlayer(Player, NextP),
        move_aux(PlayingAs, Board, NextBoard),
        drawPos(NextBoard,_,_), !.

% Player can make move that continues play
move([Player, PlayingAs, play, Board,     Dim, N],
     [NextP,  NextP,     play, NextBoard, Dim, N]) :-
        \+ chance_to_move([Player,   PlayingAs, play, Board,  Dim, N]),
        nextPlayer(Player, NextP),
        move_aux(PlayingAs, Board, NextBoard),
        \+ winPos(PlayingAs, NextBoard,_,_).

move_chance([chance(P), chance(P),  play, Board, Dim, N],
            [P,         PlayingAs,  play, Board, Dim, N]) :-
                PlayingAs = x.

move_chance([chance(P), chance(P),  play, Board, Dim, N],
            [P,         PlayingAs,  play, Board, Dim, N]) :-
                PlayingAs = o, !.


% move_aux(+Player, +Board, -NextBoard)
% True if NextBoard is Board whith an empty case replaced by a player mark.
move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement strat_at.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% strat_at(+Pos. -Strat)
strat_at([x|_], max) :- !.
strat_at([o|_], min) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement chance_to_move. chance_to_move is true if in the given position
% the coin is to be flipped i.e. chance is to "move".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chance_to_move(+Pos)
chance_to_move([chance(_), chance(_) |_]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement chance_of.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chance_of(+Pos, +NextPos, -Prob)
% Probability of moving from Pos to NextPos at a chance node.
chance_of([chance(P)|_], [P|_], 0.5) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement utility. Think carefully about what you do and do not know given
% the position.
%
% I have made it so that when a move causes a win, the 'playing as' token is
% retained in the new state so we can know which tile type one.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% utility(+Pos, -Val)
% utility/2 only evaluates the final position.
% We will use  1 when MAX (x) wins
%             -1 when MIN (o) win
%              0 otherwise.
utility([chance(o), x, win |_],  1) :- !.
utility([chance(o), o, win |_], -1) :- !.
utility([chance(x), x, win |_],  1) :- !.
utility([chance(x), o, win |_], -1) :- !.
utility([_, _, draw |_],  0) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3: Describe your evaluation predicates here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% a)
% We can break up the board into sub-boards of a smaller size and see if either
% player wins any of those sub-boards. In the 4x4 case there are 4 3x3 
% sub-boards that can be won, so we can use the number of boards won by each
% player divided by 4 to keep it bounded between -1 and 1. This will encourage 
% the computer to create wining possibilities from the corners of the board that
% it can hopefully complete in the full board with one further tile.
%
% b)
% We can count the number of avenues still available for each player to win. By
% this I mean if each player were able to put down as many tiles as they wanted,
% how many different ways are they able to win from the current board state.
% Again this can be normalised by considering the maximum number of ways a 
% player can win. It encourages the computer to set itself up with multiple 
% possible winning positions, which is a known best technique in the 
% non-stachastic version (forcing your opponent into a postion where they cannot
% prevent you winning in the next move by placing only one tile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3: Compare your evaluation predicates here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% a) This seems like the most intuitive heuristic because it still uses the same
% predicate to test the terminal win position, just with a subset of the board.
% It also generalises nicely to even larger games. 
% This method does however discard some useful information about the full game
% state as winning positions in 3x3 might already be blocked from extending to
% the 4x4 board.
%
% b) This method will be a little more complex to implement, but it doesn't
% discard any information about the full game state. The strategy this method
% encourages is overall better than the other method if it is able to enforce it
% so I think this method will perform better overall.
%
% Play-testing:
% My implementation of expectiminmax seemed to scale very poorly with the number
% of available moves, and in this part with a 4x4 board state - even with the
% cutoff - it was impossible to play games against it from the beginning.
% Doing some play-testing on some later board states:
%
% ?- play([x, x, play, [0,0,o,o, 0,x,0,0, 0,x,0,o, x,o,0,0], 4, 4], 4, x).
%
% I was able to test out the two heuristics despite the performance issues.
% Both methods proved to be good opponents from a board state with 8 remaining
% spaces and a cutoff of 4, both adopting quite a defensive strategy.
% The first heuristic did however result in a somewhat 'defeatist' behaviour in
% that when a game was looking likely for the opponent to win, it would not
% 'try' as actively to block the opponent
%
% The second heuristic was overall harder to beat, as it made moves that both
% added avenues for itself to win but also blocked avenues for the opponent to
% win. It did however sometimes block off more promising avenues of success for
% itself to win when placing the opponent's token because it has no notion of
% whether it is closer to winning one row because it has 2 tokens in it than 
% if it had only one (with the remaining spaces being empty in both cases). An
% improvement would be to also score a possible winning row/column/diagonal with
% the number of its own tokens in that row/column/diagonal.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3: Implement eval, your utility predicate for partially completed games.
% You should implement two of these, but only one should be called eval when
% you run the game. To do this you can name one eval1, the other eval2,
% and write a separate eval which you can change to delegate to one or the other
% during testing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a) Sub-Games:
%eval(Pos, Val) :- subsets(Pos, Val).

% b) Win Possibilities
eval(Pos, Val) :- possibilities(Pos, Val).

eval(_, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test with:
% subsets([x, o, play, [x,x,x,0, 0,0,0,0, o,o,o,0, 0,0,0,0], 4, 4], Val).
% Val = -0.25
%
subsets([P, _, play, 
        [X1,X2,X3,X4, X5,X6,X7,X8, X9,X10,X11,X12, X13,X14,X15,X16],
        _, _], Val) :- % sum sub-wins for player
            subsets1(P, [X1,X2,X3, X5, X6 ,X7,  X9 ,X10,X11], Vala1),
            subsets1(P, [X2,X3,X4, X6, X7, X8,  X10,X11,X12], Vala2),
            subsets1(P, [X5,X6,X7, X9, X10,X11, X13,X14,X15], Vala3),
            subsets1(P, [X6,X7,X8, X10,X11,X12, X14,X15,X16], Vala4),
            Vala12 is Vala1 + Vala2,
            Vala34 is Vala3 + Vala4,
            Vala is Vala12 + Vala34,
            opposite(P, Opp), % sum sub-wins for opponent
            subsets1(Opp, [X1,X2,X3, X5, X6 ,X7,  X9 ,X10,X11], Valb1),
            subsets1(Opp, [X2,X3,X4, X6, X7, X8,  X10,X11,X12], Valb2),
            subsets1(Opp, [X5,X6,X7, X9, X10,X11, X13,X14,X15], Valb3),
            subsets1(Opp, [X6,X7,X8, X10,X11,X12, X14,X15,X16], Valb4),
            Valb12 is Valb1 + Valb2,
            Valb34 is Valb3 + Valb4,
            Valb is Valb12 + Valb34,
            Val is Vala - Valb, !.

subsets1(P, Board, Val) :-
    winPos(P, Board, _, _),
    Val is 0.25.

subsets1(_, _, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test with:
% possibilities([x, o, play, [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0], 4, 4], Val).
% Val = 0.
%
% possibilities([x, o, play, [x,x,x,0, 0,0,0,0, o,o,o,0, 0,0,0,0], 4, 4], Val).
% Val = -0.1.
%
% possibilities([x, o, play, [x,x,x,0, 0,0,0,0, 0,0,0,0, 0,0,0,o], 4, 4], Val).
% Val = 0.2.

possibilities([P, _, play, Pos, _, _], Val) :-
    setof(TypeA, all_possibilities(P, Pos, TypeA), LA),
    length(LA, NA),
    ValA is NA / 10,
    opposite(P, Opp),
    setof(TypeB, all_possibilities(Opp, Pos, TypeB), LB),
    length(LB, NB),
    ValB is NB / 10,
    Val is ValA - ValB, !.


all_possibilities(P, 
    [X1,X2,X3,X4, X5,X6,X7,X8, X9,X10,X11,X12, X13,X14,X15,X16], Type) :-
        possible(X1, X2, X3, X4, P), Type is 1 ;        % 1st line
        possible(X5, X6, X7, X8, P), Type is 2 ;        % 2nd line
        possible(X9, X10, X11, X12, P), Type is 3 ;     % 3rd line
        possible(X13, X14, X15, X16, P), Type is 4 ;    % 4th line
        possible(X1, X5, X9, X13, P), Type is 5 ;       % 1st col
        possible(X2, X6, X10, X14, P), Type is 6 ;      % 2nd col
        possible(X3, X7, X11, X15, P), Type is 7 ;      % 3rd col
        possible(X4, X8, X12, X16, P), Type is 8 ;      % 4th col
        possible(X1, X6, X11, X16, P), Type is 9 ;      % 1st diag
        possible(X4, X7, X10, X13, P), Type is 10.      % 2nd diag

possible(X1, X2, X3, X4, P) :-
    (X1 = P ; X1 = 0),
    (X2 = P ; X2 = 0),
    (X3 = P ; X3 = 0),
    (X4 = P ; X4 = 0).

opposite(x, o).
opposite(o, x).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% winPos(+Player, +Board, +Dim, +InARow)
% True if Player wins in Board.
% Special case for 3x3 board.
winPos(P, [X1, X2, X3, X4, X5, X6, X7, X8, X9], _, _) :-
    !, (
    equal(X1, X2, X3, P), ! ;    % 1st line
    equal(X4, X5, X6, P), ! ;    % 2nd line
    equal(X7, X8, X9, P), ! ;    % 3rd line
    equal(X1, X4, X7, P), ! ;    % 1st col
    equal(X2, X5, X8, P), ! ;    % 2nd col
    equal(X3, X6, X9, P), ! ;    % 3rd col
    equal(X1, X5, X9, P), ! ;    % 1st diag
    equal(X3, X5, X7, P)).    % 2nd diag

% Special case for 4x4 board.
winPos(P, [X1,X2,X3,X4, X5,X6,X7,X8, X9,X10,X11,X12, X13,X14,X15,X16], _, _) :-
    !, (
    equal(X1, X2, X3, X4, P), ! ;        % 1st line
    equal(X5, X6, X7, X8, P), ! ;        % 2nd line
    equal(X9, X10, X11, X12, P), ! ;     % 3rd line
    equal(X13, X14, X15, X16, P), ! ;    % 4th line
    equal(X1, X5, X9, X13, P), ! ;       % 1st col
    equal(X2, X6, X10, X14, P), ! ;      % 2nd col
    equal(X3, X7, X11, X15, P), ! ;      % 3rd col
    equal(X4, X8, X12, X16, P), ! ;      % 4th col
    equal(X1, X6, X11, X16, P), ! ;      % 1st diag
    equal(X4, X7, X10, X13, P)).         % 2nd diag

winPos(Player, Board, Dim, N) :-
    winPosGen(Player, Board, Dim, N).

% drawPos(+Board, +Dim, +InARow)
% True if the game is a draw.
drawPos(Board, Dim, N) :-
    \+ member(0, Board),
    \+ winPos(x, Board, Dim, N),
    \+ winPos(o, Board, Dim, N).

% equal(+W, +X, +Y, +Z).
% True if W = X = Y = Z.
equal(X, X, X, X).

% As above for 5.
equal(X, X, X, X, X).
