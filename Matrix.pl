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

move(up,[X,Y],[X1,Y]) :- X1 is X - 1.
move(down,[X,Y],[X1,Y]) :- X1 is X + 1.
move(left,[X,Y],[X,Y1]) :- Y1 is Y - 1.
move(right,[X,Y],[X,Y1]) :- Y1 is Y + 1.

% -------------------------- atBooth(NeoLoc) --------------------------------
% true only if the passed current neo's location is equal to that of the telephone
% booth in the knowledge base.

atBooth([NeoX, NeoY]) :-
	booth(NeoX,NeoY).

% --------------------- isGoal(Neo, Capacity, Hostages) ---------------------
% True Condition: 1 - Neo is at the same location as telephone booth.
%				  2 - Neo's Capacity is equal to that of KB (He is not carrying any hostages atm).
%				  3 - There are no other Hostages in the grid.

isGoal(N, C, []) :-
	atBooth(N),
	capacity(C).

% ---------------------------------------------------------------------------	
% -------------------------- Successor State Axioms -------------------------
% ---------------------------------------------------------------------------

% ------------------------------ Initial State ------------------------------
solve([NeoX, NeoY], Capacity, Hostages, s0) :-
	capacity(Capacity),
	neo_loc(NeoX, NeoY),
	hostages_loc(Hostages).

solve(Neo, Capacity, Hostages, result(Action, State)) :-
	
	% ------ Change State ------
	(
		% ------ Get previous State ------
		solve(NeoPrev, CapacityPrev, HostagesPrev, State),
		(
			% ------ Change Position ------
			(
				(Action = up; Action = down; Action = right; Action = left),
				move(Action, NeoPrev, Neo),
				\+ isBorder(Neo),
				Capacity = CapacityPrev,
				Hostages = HostagesPrev
			);
			
			% ------ Carry Hostage ------
			(
				Action = carry,
				CapacityPrev > 0, 
				select(NeoPrev, HostagesPrev, Hostages),
				Capacity is CapacityPrev - 1,
				Neo = NeoPrev
			);
			
			% ------ Drop Hostages ------
			(
				Action = drop,
				\+ capacity(CapacityPrev),
				atBooth(NeoPrev),
				capacity(Capacity),
				Neo = NeoPrev,
				Hostages = HostagesPrev
			)
		)
	);
	
	% Stay in current State
	(
		solve(Neo, Capacity, Hostages, State),
		
		% --------------- Action is carry, and neo is not allowed to carry any hostages ---------------
		(Action = carry -> (Capacity = 0; \+ member(Neo, Hostages))),
		
		% ---------- Action is drop, and neo is not carrying any hostages at the given state ----------
		(Action = drop -> capacity(Capacity)),
		
		% -------- Action is a move action, and the new location is outside the grid's borders --------
		((Action \= drop, Action \= carry) -> move(Action, Neo, NewNeo), isBorder(NewNeo))
	).

% -----------------------------------------------------------------------------
	
goalHelper1(_, _, R):- R \= depth_limit_exceeded.

goalHelper1(Limit, S, depth_limit_exceeded):-
	call_with_depth_limit(once(goalHelper2(S)),Limit,R),
	New_Limit is Limit + 1,
	goalHelper1(New_Limit,S,R).
  
goalHelper2(State):-
	solve(Neo, Capacity, Hostages, State),
	isGoal(Neo, Capacity, Hostages).
 
goal(State):-
	goalHelper1(1,State,_).