:- module(expectiminimax_tree, [move/2,utility/2,strat_at/2,
                               chance_to_move/1,chance_of/3]).

:- use_module(expectiminimax).
:- use_module(library(random)).

% Try testing with the tree:
% node(
%       node(
%             node(
%                   leaf(-1,), leaf(0), max
%                 ),
%             node(
%                   leaf(4), leaf(0), max
%                 ),
%             chance(0.6)
%           ),
%       node(
%             node(
%                   leaf(-2), leaf(2), max
%                 ),
%             node(
%                   leaf(3), leaf(-3), max
%                 ),
%             chance(0.5)
%           ),
%       min
%     )
%
% Should return 1.6 (=0.6*0+0.4*4) as the expectiminimax value of the left
% branch (first subtree).
% Command (without the line breaks):
% expectiminimax(node(node(node(leaf(-1),leaf(0),max),node(leaf(4),leaf(0),max),
% chance(0.6)),node(node(leaf(-2),leaf(2),max),node(leaf(3),leaf(-3),max),chance(0.5)),min),
% BestPos, Val).

% Generate binary nat trees of depth N.
% Also stores the player who is moving.
% P is the probability of going left, in chance(P).
gen_tree(0, leaf(V)) :- random_between(-9, 9, V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement the node cases for gen_tree.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% At odd-numbered depths we have chance nodes
gen_tree(N, node(TL, TR, chance(P))) :- 
    0 is N mod 2,
    !,
    random(0.0, 1.0, P),
    succ(M, N),
    gen_tree(M, TL),
    gen_tree(M, TR).
% At odd-numbered depths we have either min or max
% max is at level 1, and then every 4 levels
gen_tree(N, node(TL, TR, max)) :-
    1 is N mod 4,
    !,
    succ(M, N),
    gen_tree(M, TL),
    gen_tree(M, TR).
% min is at level 3, and then every 4 levels
gen_tree(N, node(TL, TR, min)) :-
    3 is N mod 4,
    !,
    succ(M, N),
    gen_tree(M, TL),
    gen_tree(M, TR).

strat_at(node(_, _, min), min).
strat_at(node(_, _, max), max).

chance_to_move(node(_, _, chance(_))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement chance_of/3 here.
% Pos is a 6 element list defining a game state
% (see stoch_ttt_engine.pl for details)
% Is this what they want us to use for this function?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chanceof(+StartPos, +EndPos, -Prob)
% chanceof([Player |_], [Player|_], P) :- P is 0.
% chanceof([Player |_], [NextPlayer |_], P) :-
%     Player \= NextPlayer,
%     P is 0.5.

chance_of(node(TL, _, chance(P)), TL, Q) :-
    Q is P.
chance_of(node(_, TR, chance(P)), TR, Q) :-
    Q is 1-P.


% Choosing either the left or right branch are legal moves.
% There is no legal move from a leaf.
move(node(TL, _, _), TL).
move(node(_, TR, _), TR).

% The leaf stores its utility.
utility(leaf(L), L).

% Generates and solves (in the sense of finding the best next position)
% a tree of depth N.
% solved_tree(+N, -Tree, -BestNext, -Val)
% You can uncomment this when you have implemented expectiminimax.
solved_tree(N, T, Best, Val) :- gen_tree(N, T), expectiminimax(T, Best, Val).

% T = 
% node(
%     node(
%         node(
%             leaf(5), leaf(-1), max
%         ), 
%         node(
%             leaf(-8), leaf(-7), max
%         ), 
%         chance(0.7263581176852368)
%     ),
%     node(
%         node(
%             leaf(-9), leaf(1), max
%         ), 
%         node(
%             leaf(-6), leaf(7), max
%         ), 
%         chance(0.5265403763133746)
%     ), 
%     min
% ),
% B = node(node(leaf(5), leaf(-1), max), node(leaf(-8), leaf(-7), max), chance(0.7263581176852368)),
% V = 1.716297412222842