:- module(stoch_tictactoe, [move/2,utility/2,winPos/4,drawPos/3,strat_at/2,
                            chance_to_move/1,chance_of/3,eval/2]).

:- use_module(winpos).
:- use_module(library(clpfd), [transpose/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Explain here what the assumptions being made in tictactoe.pl (which
% won't carry over) are, as explained in the handout.
% 
% a) The cut after winPos assumes that the player making the move is the only
% one that can win from the move, but with the stochastic version it is possible
% that one player can make a move with the other player's piece and win for them
%
% b) The utility predicate only considers the 3-item game state, not considering
% the token being used by the current player
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

move([_, _, win |_], _) :- fail.

move(Pos, NextPos) :-
    chance_to_move(Pos),
    move_chance(Pos, NextPos).

% Player can make move that wins for that player
move([Player, PlayingAs, play, Board,     Dim, N], 
     [NextP,  PlayingAs, win,  NextBoard, Dim, N]) :-
        nextPlayer(Player, NextP),
        move_aux(PlayingAs, Board, NextBoard),
        winPos(PlayingAs, NextBoard,_,_), !.

% Player can cause a draw
move([Player, PlayingAs, play, Board,     Dim, N], 
     [NextP,  PlayingAs, draw, NextBoard, Dim, N]) :-
        nextPlayer(Player, NextP),
        move_aux(PlayingAs, Board, NextBoard),
        drawPos(NextBoard,_,_), !.

% Player can make move that continues play, requiring a chance event
move([Player, PlayingAs, play, Board,     Dim, N],
     [NextP,  NextP,     play, NextBoard, Dim, N]) :-
        \+ chance_to_move([Player,   PlayingAs, play, Board,  Dim, N]),
        nextPlayer(Player, NextP),
        move_aux(PlayingAs, Board, NextBoard).

move_chance([chance(P), chance(P),  play, Board, Dim, N],
            [P,         PlayingAs,  play, Board, Dim, N]) :-
                PlayingAs = x.

move_chance([chance(P), chance(P),  play, Board, Dim, N],
            [P,         PlayingAs,  play, Board, Dim, N]) :-
                PlayingAs = o.


% move_aux(+Player, +Board, -NextBoard)
% True if NextBoard is Board whith an empty case replaced by a player mark.
move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement strat_at.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% strat_at(+Pos. -Strat)
strat_at([x|_], max).
strat_at([o|_], min).
%strat_at(Pos, random) :- chance_to_move(Pos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement chance_to_move. chance_to_move is true if in the given position
% the coin is to be flipped i.e. chance is to "move".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chance_to_move(+Pos)
chance_to_move([chance(_) |_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement chance_of.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chance_of(+Pos, +NextPos, -Prob)
% Probability of moving from Pos to NextPos at a chance node.
chance_of([chance(P)|_], [P|_], 0.5).

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
utility([chance(o), x, win |_],  1).
utility([chance(o), o, win |_], -1).
utility([chance(x), x, win |_],  1).
utility([chance(x), o, win |_], -1).
utility([_, _, draw |_],  0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3: Describe your evaluation predicates here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3: Compare your evaluation predicates here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part 3: Implement eval, your utility predicate for partially completed games.
% You should implement two of these, but only one should be called eval when
% you run the game. To do this you can name one eval1, the other eval2,
% and write a separate eval which you can change to delegate to one or the other
% during testing.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eval(_, 0).

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
