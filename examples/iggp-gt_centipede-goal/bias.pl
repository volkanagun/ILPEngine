%% taken from the paper:
%% Andrew Cropper, Richard Evans, Mark Law: Inductive general game playing. Mach. Learn. 109(7): 1393-1434 (2020)
%% https://arxiv.org/pdf/1906.09627.pdf

%% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%% ;;;
%% ;;;  Game Theory: The Centipede Game
%% ;;;
%% ;;;  A two player game with alternating play in which, in each round,
%% ;;;  each player has the option to either continue or finish. If a player
%% ;;;  finishes, the game ends immediately. Otherwise, the game continues.
%% ;;;  The payoffs are constructed so that the decisions are as follows:
%% ;;;
%% ;;;  Let [x,y] denote a payoff of 'x' to White and 'y' to Black.
%% ;;;
%% ;;;  1. White decides whether to finish at [5,0] or continue.
%% ;;;  2. Black decides whether to finish at [0,15] or continue.
%% ;;;  3. White decides whether to finish at [15,10] or continue.
%% ;;;  4. Black decides whether to finish at [10,25] or continue.
%% ;;;     (etc)
%% ;;;  17. White decides whether to finish at [85,80] or continue.
%% ;;;  18. Black decides whether to finish at [80,95] or continue.
%% ;;;  19. The game finishes at [95,90].
%% ;;;
%% ;;;  Conventional game theory suggests that rational players will finish
%% ;;;  the game immediately. To see why this is the case, consider Black's
%% ;;;  final move: a decision between 95 points and 90 points. A rational
%% ;;;  player will finish rather than continue. Assuming that Black will
%% ;;;  finish on move 18, then by similar logic White will finish on move 17,
%% ;;;  to get 85 points rather than 80, and so on, until White must logically
%% ;;;  choose to finish on move 1.
%% ;;;
%% ;;;  Background: http://en.wikipedia.org/wiki/Centipede_game
%% ;;;
%% ;;;  GDL BY: Sam Schreiber (schreib@cs.stanford.edu)
%% ;;;
%% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


max_body(7).
max_vars(7).
type(agent_white, (agent)).
type(agent_black, (agent)).
type(int_0, (int)).
type(int_5, (int)).
type(int_10, (int)).
type(int_15, (int)).
type(int_20, (int)).
type(int_25, (int)).
type(int_30, (int)).
type(int_35, (int)).
type(int_40, (int)).
type(int_45, (int)).
type(int_50, (int)).
type(int_55, (int)).
type(int_60, (int)).
type(int_65, (int)).
type(int_70, (int)).
type(int_75, (int)).
type(int_80, (int)).
type(int_85, (int)).
type(int_90, (int)).
type(int_95, (int)).
type(int_100, (int)).
type(action_finish, (action)).
type(action_continue, (action)).
type(action_noop, (action)).
type(prop_gameOver, (prop)).
head_pred(goal,3).
body_pred(true_whitePayoff,2).
body_pred(true_blackPayoff,2).
body_pred(true_control,2).
body_pred(role,1).
body_pred(succ,2).
type(true_whitePayoff, (ex, int)).
type(true_blackPayoff, (ex, int)).
type(true_control, (ex, agent)).
type(goal, (ex, agent, int)).
type(role, (agent)).
type(succ, (int, int)).

