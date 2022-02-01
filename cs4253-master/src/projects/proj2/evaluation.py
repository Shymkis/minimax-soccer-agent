#!/usr/bin/env python3

import math, random, time
from ...lib.game import discrete_soccer, connect_four

def soccer(state, player_id):
    # TODO: Implement this function!
    #
    # The soccer evaluation function *must* look into the game state
    # when running. It will then return a number, where the larger the
    # number, the better the expected reward (or lower bound reward)
    # will be.
    #
    # For a good evaluation function, you will need to
    # SoccerState-specific information. The file
    # `src/lib/game/discrete_soccer.py` provides a description of all
    # useful SoccerState properties.
    if not isinstance(state, discrete_soccer.SoccerState):
        raise ValueError("Evaluation function incompatible with game type.")

    # If state is terminal,
    # add or deduct large magnitude according to reward.
    # If player is winner, end game ASAP,
    # else keep trying. Maybe human opponent will mess up.
    reward = 0
    player = state.players[player_id]
    if state.is_terminal:
        reward = state.reward(player_id)*9999
        if state.winner == player.team:
            return reward

    # If player has ball,
    # return positive value according to distance to goal
    player_location = (player.x, player.y)
    max_dist = math.sqrt((state.pitch.width)**2 + (state.pitch.height)**2)
    if player.has_ball:
        goal_location = state.goal_pos(player.team)
        distance_to_goal = math.dist(player_location, goal_location)
        return reward + max_dist - distance_to_goal

    # If other player has ball,
    # return negative value according to distance to midpoint
    other = state.players[(player_id + 1)%state.num_players]
    other_location = (other.x, other.y)
    if other.has_ball:
        goal_location = state.goal_pos(other.team)
        Mx = (other.x + goal_location[0])/2
        My = (other.y + goal_location[1])/2
        midpoint = (Mx, My)
        distance_to_midpoint = math.dist(player_location, midpoint)
        return reward - distance_to_midpoint

    # If no player has ball,
    # return positive value according to inverse distance to ball
    ball_location = (state.ball.x, state.ball.y)
    distance_to_ball = math.dist(player_location, ball_location)
    return reward + 1/distance_to_ball

def connect_four(state, player_id):
    if not isinstance(state, connect_four.Connect4State):
        raise ValueError("Evaluation function incompatible with game type.")
    return 0
