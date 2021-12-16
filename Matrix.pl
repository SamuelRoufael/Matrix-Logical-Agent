:-include("KB.pl").

% --------------------- isBorder(NeoLoc) -----------------------------------------
% True only if the passed location is outside the borders of the grids.

isBorder([X,Y]) :- 
	grid(_,Y); 
	grid(X,_);
	X < 0;
	Y < 0.

% ---------------------- move(Action, PrevLoc, NewLoc) ----------------------
% Takes the direction which neo will move to, the current location, and the 
% new location after applying the move action on the current location.

move(up,[X,Y],[X1,Y]) :- X is X1 + 1.
move(down,[X,Y],[X1,Y]) :- X is X1 - 1.
move(left,[X,Y],[X,Y1]) :- Y is Y1 + 1.
move(right,[X,Y],[X,Y1]) :- Y is Y1 - 1.

% -------------------------- atBooth(NeoLoc) --------------------------------
% true only if the passed current neo's location is equal to that of the telephone
% booth in the knowledge base.

atBooth([NeoX, NeoY]) :-
	booth(NeoX,NeoY).

% ---------------------------------------------------------------------------	
% -------------------------- Successor State Axioms -------------------------
% ---------------------------------------------------------------------------

% ------------------------ Initial State & Base Case ------------------------

solve([NeoX, NeoY], Capacity, Hostages, s0) :-
	capacity(Capacity),
	neo_loc(NeoX, NeoY),
	hostages_loc(Hostages).

% ------------------------ Intermediate States ------------------------------

solve(Neo, Capacity, Hostages, result(Action, State)) :-
	
	% ------ Change Position ------
	(
		(Action = up; Action = down; Action = right; Action = left),
		\+ isBorder(Neo),
		move(Action, NeoPrev, Neo),
		solve(NeoPrev, Capacity, Hostages, State)
	);
			
	% ------ Carry Hostage ------
	(
		Action = carry,
		\+ capacity(Capacity),
		hostages_loc(AllHostages),
		member(Neo, AllHostages),
		append([Neo], Hostages, HostagesPrev),
		CapacityPrev is Capacity + 1,
		solve(Neo, CapacityPrev, HostagesPrev, State)
	);
			
	% ------ Drop Hostages ------
	(
		Action = drop,
		atBooth(Neo),
		capacity(Capacity),
		(
			% Neo Carried Hostages, as he reached his maximum capacity
			solve(Neo, 0, Hostages, State);
			
			% Neo Carried all Hostages in the grid but still has room for more Hostages to carry.
			(
				hostages_loc(AllHostages),
				length(AllHostages, N),
				C1 is Capacity - N,
				C1 > 0,
				solve(Neo, C1, Hostages, State)
			)
		)
	).

% -----------------------------------------------------------------------------
	
goalHelper1(_, _, R):- R \= depth_limit_exceeded.

goalHelper1(Limit, S, depth_limit_exceeded):-
	call_with_depth_limit(once(goalHelper2(S)),Limit,R),
	New_Limit is Limit + 1,
	goalHelper1(New_Limit,S,R).
  
goalHelper2(State):-
	capacity(Capacity),
	atBooth(Neo),
	solve(Neo, Capacity, [], State).
 
goal(State):-
	goalHelper1(1,State,_).