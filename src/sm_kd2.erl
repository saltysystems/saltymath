% sm_kd2 -
% 2-dimensional K-D Tree
-module(sm_kd2).
-export([build/1, nearest/2, naive_nearest/2, range_query/2]).

-record(node, {
    pos :: {float(), float()},
    data :: any(),
    left :: #node{} | undefined,
    right :: #node{} | undefined,
    axis :: 0 | 1
}).
-opaque tree() :: #node{}.
-export_type([tree/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Build a K-D Tree of Dimension 2. Must supply a list of objects and a
%      function that will retrieve position in {X,Y} format from the object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec build([{{float(),float()}, any()}]) -> #node{} | undefined.
build(Objects) ->
    build(Objects, 0).

-spec build(list(), integer()) -> #node{} | undefined.
build([], _Depth) ->
    undefined;
build(Objects, Depth) ->
    % Splitting axis
    SplitAxis = Depth rem 2,

    % Sort the list of objects by the axis
    F = fun
        ({{X1,_},_Data1}, {{X2,_},_Data2}) when SplitAxis =:= 0 ->
            % X Axis
            X1 =< X2;
        ({{_,Y1},_Data1}, {{_,Y2},_Data2}) when SplitAxis =:= 1 ->
            Y1 =< Y2
    end,

    Sorted = lists:sort(F, Objects),
    Index = (length(Sorted) + 1) div 2,
    % Ensure the median ends up on the right by splitting on Index-1
    {LeftObjects, [{Pos, Data}| RightObjects]} = lists:split(Index - 1, Sorted),

    % Create a new node that represents of the median of the objects along the
    % split axis, with smaller positions on the left and larger on the right
    N = #node{
        pos = Pos,
        data = Data,
        left = build(LeftObjects, Depth + 1),
        right = build(RightObjects, Depth + 1),
        axis = SplitAxis
    },
    N.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Find the nearest neighbour to a target position given by {X,Y} or 
%      #{ x := X, y := Y }
%      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec nearest(#{ x := float(), y := float() } | {float(), float()}, #node{}) -> {{float(), float()}, any()}.
nearest(#{ x := X, y := Y}, Node) ->
    nearest({X,Y}, Node);
nearest(TargetPos, Node) ->
    {NearestNode, _Distance} = nearest(TargetPos, Node, undefined, undefined),
    {NearestNode#node.pos, NearestNode#node.data}.

nearest(_TargetPos, undefined, Best, BestDistance) ->
    % Leaf
    {Best, BestDistance};
nearest(TargetPos, Node = #node{pos = NodePos}, undefined, undefined) ->
    % No best / best distance defined, at the entrypoint
    BestDistance = distance(NodePos, TargetPos),
    nearest(TargetPos, Node, Node, BestDistance);
nearest(TargetPos, Node, Best, BestDistance) ->
    #node{
        pos = NodePos,
        left = LN,
        right = RN,
        axis = Axis
    } = Node,
    NodeDistance = distance(TargetPos, NodePos),
    {NewBest, NewBestDistance} =
        case NodeDistance < BestDistance of
            true ->
                % Current node is closer than the best so far
                {Node, NodeDistance};
            false ->
                % Old node is still best
                {Best, BestDistance}
        end,
    {Next, Opposite} =
        case cmp(TargetPos, NodePos, Axis) of
            true ->
                % Go down the left branch first
                {LN, RN};
            false ->
                % Go down the right branch fist
                {RN, LN}
        end,
    {NewBest1, NewBestDistance1} = nearest(TargetPos, Next, NewBest, NewBestDistance),
    % Check if the opposite branch can still have something useful. We can prune this.
    case Opposite of
        undefined ->
            % not worth checking but pruned automatically
            nearest(TargetPos, Opposite, NewBest1, NewBestDistance1);
        _Node ->
            OppPos = Opposite#node.pos,
            D =
                case Axis of
                    0 ->
                        {Tx, _} = TargetPos,
                        {Ox, _} = OppPos,
                        Ox - Tx;
                    1 ->
                        {_, Ty} = TargetPos,
                        {_, Oy} = OppPos,
                        Oy - Ty
                end,
            case abs(D) < NewBestDistance1 of
                true ->
                    nearest(TargetPos, Opposite, NewBest1, NewBestDistance1);
                false ->
                    {NewBest1, NewBestDistance1}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @doc Find points within a bounding box specified by {{X1,Y1},{X2,Y2}}
%      or {#{ x := X1, y := Y1 },#{ x := X2, y := Y2}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec range_query({{number(), number()}, {number(), number()}}, #node{}) ->
    [any()].
range_query({#{ x := X1, y := Y1}, #{ x := X2, y := Y2 }}, Node) ->
    range_query({{X1,Y1},{X2,Y2}}, Node);
range_query(Range, Node) ->
    range_query(Range, Node, []).

range_query(_Range, undefined, Neighbours) ->
    Neighbours;
range_query(Range = {MinPos, MaxPos}, Node, Neighbours) ->
    #node{
        pos = NodePos,
        data = NodeData,
        left = LN,
        right = RN,
        axis = Axis
    } = Node,
    case is_in_range(NodePos, Range) of
        true ->
            Neighbours1 = [NodeData | Neighbours];
        false ->
            Neighbours1 = Neighbours
    end,
    case overlaps_range(MinPos, MaxPos, NodePos, Axis) of
        {true, true} ->
            % Both branches may contain points in the range
            Neighbours2 = range_query(Range, LN, Neighbours1),
            range_query(Range, RN, Neighbours2);
        {true, false} ->
            % Only the left branch may contain points in the range
            range_query(Range, LN, Neighbours1);
        {false, true} ->
            % Only the right branch may contain points in the range
            range_query(Range, RN, Neighbours1);
        {false, false} ->
            % Neither branch can contain points in the range
            Neighbours1
    end.

is_in_range({X, Y}, {{MinX, MinY}, {MaxX, MaxY}}) ->
    X >= MinX andalso X =< MaxX andalso Y >= MinY andalso Y =< MaxY.

overlaps_range({MinX, MinY}, {MaxX, MaxY}, {Px, Py}, Axis) ->
    case Axis of
        0 ->
            {Px >= MinX, Px =< MaxX};
        1 ->
            {Py >= MinY, Py =< MaxY}
    end.

naive_nearest({Xp, Yp}, List) ->
    F = fun
        ({X, Y}, inf) ->
            D = math:pow(Xp - X, 2) + math:pow(Yp - Y, 2),
            {{X, Y}, D};
        ({X, Y}, {AccPoint, AccDist}) ->
            D = math:pow(Xp - X, 2) + math:pow(Yp - Y, 2),
            case D =< AccDist of
                true ->
                    {{X, Y}, D};
                false ->
                    {AccPoint, AccDist}
            end
    end,
    {Point, _Distance} = lists:foldl(F, inf, List),
    Point.

cmp({Xt, _}, {Xc, _}, 0) ->
    Xt < Xc;
cmp({_, Yt}, {_, Yc}, 1) ->
    Yt < Yc.

distance({X1, Y1}, {X2, Y2}) ->
    (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2).
