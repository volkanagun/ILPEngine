%% taken from the paper:
%% Andrew Cropper, Richard Evans, Mark Law: Inductive general game playing. Mach. Learn. 109(7): 1393-1434 (2020)
%% https://arxiv.org/pdf/1906.09627.pdf

max_vars(7).
max_body(10).
type(agent_black, (agent)).
type(action_left, (action)).
type(action_right, (action)).
type(action_up, (action)).
type(action_down, (action)).
type(action_noop, (action)).
type(int_1, (int)).
type(int_2, (int)).
type(int_3, (int)).
type(int_4, (int)).
type(int_5, (int)).
type(score_0, (score)).
type(score_100, (score)).
type(obj_x, (obj)).
type(obj_obj1, (obj)).
type(obj_obj2, (obj)).
type(obj_wall, (obj)).
head_pred(goal,3).
body_pred(true_at,4).
body_pred(true_target,3).
body_pred(input,2).
body_pred(player_obj,1).
body_pred(is_box,1).
body_pred(controls,2).
body_pred(object,1).
body_pred(role,1).
body_pred(is_left,1).
body_pred(is_right,1).
body_pred(is_up,1).
body_pred(is_down,1).
body_pred(is_noop,1).
body_pred(succ,2).
body_pred(bounds,1).
body_pred(dir,1).
type(true_at, (ex, int, int, obj)).
type(true_target, (ex, int, int)).
type(input, (agent, action)).
type(goal, (ex, agent, score)).
type(player_obj, (obj)).
type(is_box, (obj)).
type(controls,(agent, obj)).
type(object, (obj)).
type(role, (agent)).
type(is_left, (action)).
type(is_right, (action)).
type(is_up, (action)).
type(is_down, (action)).
type(is_noop, (action)).
type(succ, (int, int)).
type(bounds, (int)).
type(dir, (action)).
