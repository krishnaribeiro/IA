% A means-ends planner with goal regression
% plan( State, Goals, Plan)
plan( State, Goals, [ ]) :-
    satisfied( State, Goals).                % Goals true in State

plan( State, Goals, Plan) :-
    conc( PrePlan, [ Action ], Plan),         % Divide plan achieving breadth-first effect
    select( State, Goals, Goal),              % Select a goal
    achieves( Action, Goal),
    can( Action, Condition),                  % Ensure Action contains no variables
    preserves( Action, Goals),
    regress( Goals, Action, RegressedGoals),  % Regress Goals through Action
    plan( State, RegressedGoals, PrePlan).    % Protect Goals

satisfied( State, Goals) :-
    delete_all( Goals, State, [ ]).           % All Goals in State

select( State, Goals, Goal) :-
    member( Goal, Goals).                     % Select Goal from Goals

achieves( Action, Goal) :-
    adds( Action, Goal),
    member( Goal, Goals).                     % A simple selection principle

preserves( Action, Goals) :-
    deletes( Action, Relations),
    \+ ( member( Goal, Relations),            % Action does not destroy Goals
         member( Goal, Goals)).

regress( Goals, Action, RegressedGoals) :-
    adds( Action, NewRelations),
    delete_all( Goals, NewRelations, RestGoals),   % Regress Goals through Action
    can( Action, Condition),
    addnew( Condition, RestGoals, RegressedGoals). % Add precond., check imposs.

% addnew( NewGoals, OldGoals, AllGoals) :-
%    conc( NewGoals, OldGoals, AllGoals).      % AllGoals is the union of NewGoals and OldGoals

addnew( [ ], L, L).

addnew( [ Goal | _ ], Goals, _ ) :-
    impossible( Goal, Goals), !, fail.        % Goal incompatible with Goals

addnew( [ X | L1 ], L2, L3) :-
    member( X, L2), !,
    addnew( L1, L2, L3).                      % Ignore duplicate

addnew( [ X | L1], L2, [ X | L3] ) :-
    addnew( L1, L2, L3).

% delete_all( L1, L2, Diff): Diff is set-difference of lists L1 and L2

delete_all( [ ], _, [ ]).

delete_all( [ X | L1 ], L2, Diff) :-
    member( X, L2), !,
    delete_all( L1, L2, Diff).

delete_all( [ X | L1 ], L2, [ X | Diff]) :-
    delete_all( L1, L2, Diff).

% New predicates to handle variables in goals and actions
achieves( Action, Goal) :-
    adds( Action, GoalTemplate),
    unify_with_occurs_check(Goal, GoalTemplate).

can( Action, Condition) :-
    preconditions( Action, ConditionTemplate),
    unify_with_occurs_check(Condition, ConditionTemplate).

% Regressing goals with variable handling
regress( Goals, Action, RegressedGoals) :-
    adds( Action, NewRelations),
    delete_all( Goals, NewRelations, RestGoals),   % Regress Goals through Action
    can( Action, Condition),
    addnew( Condition, RestGoals, TempGoals),      % Add precond., check imposs.
    subst_vars(TempGoals, RegressedGoals).         % Substitute variables

% Substitute variables to handle variable propagation
subst_vars([], []).

subst_vars([Var/Val|Rest], [Var/Val|SubstRest]) :-
    subst_vars(Rest, SubstRest).

subst_vars([Goal|Rest], [SubstGoal|SubstRest]) :-
    subst(Goal, SubstGoal),
    subst_vars(Rest, SubstRest).

subst(Goal, SubstGoal) :-
    % Substitute variables in Goal with their bindings
    copy_term(Goal, SubstGoal).

