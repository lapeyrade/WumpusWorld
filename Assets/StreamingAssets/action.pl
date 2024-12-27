%% Action Ontology - Defines the possible actions that can be performed in the game
%% This file implements the action hierarchy using Prolog rules

%% Dynamic and incremental tabling declarations for action predicates
%% Ensures efficient querying and updates of the action knowledge base
:- table (action/1, interact/1, attack/1, shoot/1, move/1, moveback/1,
     bumpwall/1, pickup/1, discard/1, shootarrow/1) as (incremental, dynamic).

%% Action Hierarchy
%% Defines all possible actions in the game world:
%% - interact: actions involving object manipulation
%% - attack: offensive actions
%% - move: movement actions
%% - moveback: retreat actions
%% - bumpwall: collision actions
action(X) :- interact(X).
action(X) :- attack(X).
action(X) :- move(X).
action(X) :- moveback(X).
action(X) :- bumpwall(X).

%% Interaction Actions
%% Actions that involve manipulating objects in the environment
interact(X) :- pickup(X).    % Picking up items
interact(X) :- discard(X).   % Dropping items

%% Combat Actions
%% Actions related to combat and offensive capabilities
attack(X) :- shoot(X).       % Generic shooting action
shoot(X) :- shootarrow(X).   % Specific bow and arrow attack