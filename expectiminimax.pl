:- module(expectiminimax, [expectiminimax/3]).
:- use_module(library(random)).

% expectiminimax(+Pos, -BestNextPos, -Val)
% Pos is a position, Val is its expectiminimax value.
% Best move from Pos leads to position BestNextPos.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement expectiminimax. You do NOT have to modify any of the other
% functions. This goes for every predicate we ask you to modify. You may
% add new predicates as needed, however.
% FOR PART 3:
% Please keep around an expectiminimax/3:
% Add the cutoff argument then add an expectiminimax/3 which delegaes to it
% like so:
% expectiminimax(Pos, BestNextPos, Val) :-
%     expectiminimax(Pos, -1, BestNextPos, Val).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expectiminimax(Pos, BestNextPos, Val) :- ...

% One choice to move to automatically means it is the best next position.
best([Pos], _, Pos, Val) :-
    expectiminimax(Pos, _, Val), !.

% To pick between a number of positions, calculate the minimax of the first one,
% find the best of the rest, then pick whichever was best out of the first or
% the one which was best of the rest.
best([Pos1 | PosList], Strat, BestPos, BestVal) :-
    expectiminimax(Pos1, _, Val1),
    best(PosList, Strat, Pos2, Val2),
    betterOf(Pos1, Val1, Pos2, Val2, Strat, BestPos, BestVal).

% Pos0 better than Pos1.
betterOf(Pos0, Val0, _, Val1, Strat, Pos0, Val0) :-
    Strat = max,    % MAX prefers the greater value.
    Val0 > Val1, !
    ;
    Strat = min,    % MIN prefers the lesser value.
    Val0 < Val1, !.

% Otherwise Pos1 better than Pos0.
betterOf(_, _, Pos1, Val1, _, Pos1, Val1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implement expectedVal.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expectedVal(Pos, Val) :- ...