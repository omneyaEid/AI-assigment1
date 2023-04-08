goalState([#,0,1,
      2,3,4,
      5,6,7]).

% # in goal state indicates to blank space

% all possible direction for # (blank space) according to its position
% 2RD  3LRD  2LD
% 3RTD 4LRTD 3LTD
% 2RT  3LRT  2LT

% move left in the top row
move([X1,#,X3, X4,X5,X6, X7,X8,X9],
     [#,X1,X3, X4,X5,X6, X7,X8,X9]).
move([X1,X2,#, X4,X5,X6, X7,X8,X9],
     [X1,#,X2, X4,X5,X6, X7,X8,X9]).

% move left in the middle row
move([X1,X2,X3, X4,#,X6,X7,X8,X9],
     [X1,X2,X3, #,X4,X6,X7,X8,X9]).
move([X1,X2,X3, X4,X5,#,X7,X8,X9],
     [X1,X2,X3, X4,#,X5,X7,X8,X9]).


% move left in the bottom row
move([X1,X2,X3, X4,X5,X6, X7,#,X9],
     [X1,X2,X3, X4,X5,X6, #,X7,X9]).
move([X1,X2,X3, X4,X5,X6, X7,X8,#],
     [X1,X2,X3, X4,X5,X6, X7,#,X8]).


% move right in the top row 
move([#,X2,X3, X4,X5,X6, X7,X8,X9],
     [X2,#,X3, X4,X5,X6, X7,X8,X9]).
move([X1,#,X3, X4,X5,X6, X7,X8,X9],
     [X1,X3,#, X4,X5,X6, X7,X8,X9]).

% move right in the middle row 
move([X1,X2,X3, #,X5,X6, X7,X8,X9],
     [X1,X2,X3, X5,#,X6, X7,X8,X9]).
move([X1,X2,X3, X4,#,X6, X7,X8,X9],
     [X1,X2,X3, X4,X6,#, X7,X8,X9]).


% move right in the bottom row
move([X1,X2,X3, X4,X5,X6,#,X8,X9],
     [X1,X2,X3, X4,X5,X6,X8,#,X9]).
move([X1,X2,X3, X4,X5,X6,X7,#,X9],
     [X1,X2,X3, X4,X5,X6,X7,X9,#]).


% move up from the middle row
move([X1,X2,X3, #,X5,X6, X7,X8,X9],
     [#,X2,X3, X1,X5,X6, X7,X8,X9]).
move([X1,X2,X3, X4,#,X6, X7,X8,X9],
     [X1,#,X3, X4,X2,X6, X7,X8,X9]).
move([X1,X2,X3, X4,X5,#, X7,X8,X9],
     [X1,X2,#, X4,X5,X3, X7,X8,X9]).


% move up from the bottom row
move([X1,X2,X3, X4,X5,X6, X7,#,X9],
     [X1,X2,X3, X4,#,X6, X7,X5,X9]).
move([X1,X2,X3, X4,X5,X6, X7,X8,#],
     [X1,X2,X3, X4,X5,#, X7,X8,X6]).
move([X1,X2,X3, X4,X5,X6, #,X8,X9],
     [X1,X2,X3, #,X5,X6, X4,X8,X9]).


% move down from the top row
move([#,X2,X3, X4,X5,X6, X7,X8,X9],
     [X4,X2,X3, #,X5,X6, X7,X8,X9]).
move([X1,#,X3, X4,X5,X6, X7,X8,X9],
     [X1,X5,X3, X4,#,X6, X7,X8,X9]).
move([X1,X2,#, X4,X5,X6, X7,X8,X9],
     [X1,X2,X6, X4,X5,#, X7,X8,X9]).


% move down from the middle row
move([X1,X2,X3, #,X5,X6, X7,X8,X9],
     [X1,X2,X3, X7,X5,X6, #,X8,X9]).
move([X1,X2,X3, X4,#,X6, X7,X8,X9],
     [X1,X2,X3, X4,X8,X6, X7,#,X9]).
move([X1,X2,X3, X4,X5,#, X7,X8,X9],
     [X1,X2,X3, X4,X5,X9, X7,X8,#]).


crossPuzzle(S, Path, Path) :- goalState(S).
crossPuzzle(S, Visited, Path) :-
    % Move to new state
    move(S, S2),
    % ensure that new state not visited
    \+member(S2, Visited),
    % call CrossPuzzle for new state
    crossPuzzle(S2, [S2|Visited], Path) , !.
    
