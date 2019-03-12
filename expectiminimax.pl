:- module(expectiminimax, [expectiminimax/4, expectiminimax/3]).
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

expectiminimax(Pos, BestNextPos, Val) :- 
    expectiminimax(Pos, -1, BestNextPos, Val).

expectiminimax(Pos, Cutoff, _, Val) :-
    chance_to_move(Pos),
    expectedVal(Pos, Cutoff, Val), !.

expectiminimax(Pos, Cutoff, BestNextPos, Val) :-
    Cutoff > 0,
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    strat_at(Pos, Strat),
    best(NextPosList, Cutoff, Strat, BestNextPos, Val), !.

% Hit the cutoff so evaluate this state directly
expectiminimax(Pos, Cutoff, _, Val) :-
    Cutoff = 0,
    eval(Pos, Val), !.

% If we get this far, Cutoff < 0 so we are doing full search
expectiminimax(Pos, Cutoff, BestNextPos, Val) :-
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    strat_at(Pos, Strat),
    best(NextPosList, Cutoff, Strat, BestNextPos, Val), !.

% If we get this far, there were no moves available from Pos, so it is terminal
expectiminimax(Pos, _,  _, Val) :-
    utility(Pos, Val).


% One choice to move to automatically means it is the best next position.
best([Pos], Cutoff, _, Pos, Val) :-
    expectiminimax(Pos, Cutoff, _, Val), !.

% To pick between a number of positions, calculate the minimax of the first one,
% find the best of the rest, then pick whichever was best out of the first or
% the one which was best of the rest.
best([Pos1 | PosList], Cutoff, Strat, BestPos, BestVal) :-
    NewCutoff is Cutoff - 1,
    expectiminimax(Pos1, NewCutoff, _, Val1),
    best(PosList, Cutoff, Strat, Pos2, Val2), % still use old cutoff for rest
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
% expectedVal(+Pos, -ExpVal)
% expectedVal/2 does not require any notion of the best next position because
% each outcome is assumed to have a non-zero probability and so their values
% must both be considered; The chance nodes do not have a 'best move'.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expectedVal(Pos, Cutoff, Val) :-
    bagof(NextPos, move(Pos, NextPos), NextPosList),
    [TL, TR] = NextPosList,
    expectiminimax(TL, Cutoff, _, LVal),
    expectiminimax(TR, Cutoff, _, RVal),
    chance_of(Pos, TL, LProb),
    chance_of(Pos, TR, RProb),
    Val is (LVal * LProb) + (RVal * RProb), !.
