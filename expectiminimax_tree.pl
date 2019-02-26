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
% gen_tree(N, node(TL, TR, chance(P))) :- ...
% gen_tree(N, node(TL, TR, min)) :- ...
% gen_tree(N, node(TL, TR, max)) :- ...

strat_at(node(_, _, min), min).
strat_at(node(_, _, max), max).

chance_to_move(node(_, _, chance(_))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement chance_of/3 here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
%solved_tree(N, T, Best, Val) :- gen_tree(N, T), expectiminimax(T, Best, Val).
