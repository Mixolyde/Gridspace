%%% Some Cellular Automata map generaters

-module(cellular_map_gen).
-author("Brian E. Williams").
-compile([debug_info, export_all]).

cellular_cave(Width, Height) ->
    StartCave = random_maze(Width, Height, 0.40),
    FirstRuleCave = applyCellularRule(StartCave, 4,
    fun(RuleX, RuleY, RuleCave) ->
        Range1Walls = [X || X <- map_util:list_of_neighbors_in_range(RuleCave, {RuleX, RuleY}, wall, 1), X == wall],
        Range2Walls = [X || X <- map_util:list_of_neighbors_in_range(RuleCave, {RuleX, RuleY}, wall, 2), X == wall],
        if length(Range1Walls) >= 5; length(Range2Walls) =< 2 ->
            wall;
        true ->
            open
        end
    end),
    FinalCave = applyCellularRule(FirstRuleCave, 3,
    fun(RuleX, RuleY, RuleCave) ->
        Range1Walls = [X || X <- map_util:list_of_neighbors_in_range(RuleCave, {RuleX, RuleY}, wall, 1), X == wall],
        if length(Range1Walls) >= 5 ->
            wall;
        true ->
            open
        end
    end),
    map_util:print_maze(FinalCave).


random_maze(_Width, 0, _Percent) ->
    [];
random_maze(Width, Height, Percent) ->
    [random_row(Width, Percent) | random_maze(Width, Height - 1, Percent)].

random_row(0, _Percent) ->
    [];
random_row(Width, Percent) ->
    RandFloat = random:uniform(),
    [ if RandFloat < Percent -> wall; true -> open end | random_row(Width - 1, Percent)].

applyCellularRule(Cave, 0, _Rule) -> Cave;
applyCellularRule(Cave, Count, Rule) ->
    AppliedCave = applyCellularRule(Cave, Count - 1, Rule),
    NewCave = lists:map(
    fun(MapY) ->
        lists:map(
            fun(MapX) -> Rule(MapX, MapY, AppliedCave) end,
            lists:seq(1, map_util:max_width(AppliedCave)) )
        end,
    lists:seq(1, length(AppliedCave)) ),
    % map_util:print_maze(NewCave),
    NewCave.
