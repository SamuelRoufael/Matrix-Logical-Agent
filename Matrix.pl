:-include("KB.pl").

% ---------------------------------------------------------------------------

isBorder([X,Y]) :- 
	grid(_,Y); 
	grid(X,_).

% ---------------------------------------------------------------------------

move(up,[X,Y],[X1,Y]) :- X1 is X - 1.
move(down,[X,Y],[X1,Y]) :- X1 is X + 1.
move(left,[X,Y],[X,Y1]) :- Y1 is Y - 1.
move(right[X,Y],[X,Y1]) :- Y1 is Y + 1.

% ---------------------------------------------------------------------------

sameLocation([X,Y], [X,Y]).

canCarryHostage(N, C, [H|_], H) :-
	C > 0,
	sameLocation(N, H).

canCarryHostage(N, C, [H|T], H1) :-
	C > 0,
	\+ sameLocation(N, H),
	canCarryHostage(N, C, T, H1).

% ---------------------------------------------------------------------------

atBooth([X,Y]) :-
	booth(X,Y).

canDrop(N, [_|_]) :-
	atBooth(N).

% --------------------- isGoal(Neo, Capacity, Hostages) ---------------------
% True Condition: 1 - Neo is at the same location as telephone booth.
%				  2 - Neo's Capacity is equal to that of KB (He is not carrying any hostages atm).
				  3 - There are no other Hostages in the grid.

isGoal(N, C, []) :-
	atBooth(N),
	capacity(C).

