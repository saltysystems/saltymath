% sm_vec2t
% Vector operations for 2-tuples
-module(sm_vec2t).

-export([zero/0, splat/1, add/2, sub/2, mul/2, divide/2, dot/2, cross/2, abs/1, len/1,
         len_sq/1, distance/2, distance_sq/2, rotate/2, rotate/3, normalize/1]).

-type vector2() :: {float(), float()}.

-export_type([vector2/0]).

-compile(inline).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns a zero vector (0,0)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec zero() -> vector2().
zero() ->
    {0.0, 0.0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a vector with both components set to the given value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec splat(float()) -> vector2().
splat(N) when is_float(N) ->
    {N, N}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Adds two vectors component-wise
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add(vector2(), vector2()) -> vector2().
add({V10, V11}, {V20, V21}) when is_float(V10), is_float(V11) ->
    {V10 + V20, V11 + V21}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Subtracts the second vector from the first component-wise
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec sub(vector2(), vector2()) -> vector2().
sub({V10, V11}, {V20, V21}) when is_float(V10), is_float(V11) ->
    {V10 - V20, V11 - V21}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Multiplies a vector by a scalar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec mul(vector2(), float()) -> vector2().
mul({V10, V11}, S) when is_float(S) ->
    {V10 * S, V11 * S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Divides a vector by a scalar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec divide(vector2(), float()) -> vector2().
divide({V10, V11}, S) when is_float(S) ->
    {V10 / S, V11 / S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Calculates the dot product of two vectors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec dot(vector2(), vector2()) -> float().
dot({V10, V11}, {V20, V21}) when is_float(V10), is_float(V11) ->
    V10 * V20 + V11 * V21.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Calculates the cross product of two 2D vectors (returns a scalar)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec cross(vector2(), vector2()) -> float().
cross({V10, V11}, {V20, V21}) when is_float(V10), is_float(V11) ->
    V10 * V21 - V20 * V11.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns a vector with the absolute values of the components
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec abs(vector2()) -> vector2().
abs({V10, V11}) when is_float(V10), is_float(V11) ->
    {erlang:abs(V10), erlang:abs(V11)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Calculates the length (magnitude) of a vector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec len(vector2()) -> float().
len({V10, V11}) when is_float(V10), is_float(V11) ->
    math:sqrt(V10 * V10 + V11 * V11).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Calculates the squared length of a vector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec len_sq(vector2()) -> float().
len_sq({V10, V11}) when is_float(V10), is_float(V11) ->
    V10 * V10 + V11 * V11.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Calculates the Euclidean distance between two points
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec distance(vector2(), vector2()) -> float().
distance({V10, V11}, {V20, V21})
    when is_float(V10), is_float(V11), is_float(V20), is_float(V21) ->
    X = V10 - V20,
    Y = V11 - V21,
    math:sqrt(X * X + Y * Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Calculates the squared Euclidean distance between two points
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec distance_sq(vector2(), vector2()) -> float().
distance_sq({V10, V11}, {V20, V21}) when is_float(V10), is_float(V11) ->
    X = V10 - V20,
    Y = V11 - V21,
    X * X + Y * Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Rotates a vector around the origin by the given angle in radians
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec rotate(vector2(), float()) -> vector2().
rotate(Vector, Rad) when is_float(Rad) ->
    rotate(Vector, Rad, {0.0, 0.0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Rotates a vector around a given point by the given angle in radians
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec rotate(vector2(), float(), vector2()) -> vector2().
rotate({V10, V11}, Rad, {V20, V21})
    when is_float(V10), is_float(V11), is_float(V20), is_float(V21) ->
    X = V20 + (V10 - V20) * math:cos(Rad) - (V11 - V21) * math:sin(Rad),
    Y = V21 + (V10 - V20) * math:sin(Rad) + (V11 - V21) * math:cos(Rad),
    {X, Y}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Normalizes a vector
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec normalize(vector2()) -> vector2().
normalize({V10, V11}) ->
    N = math:sqrt(V10 * V10 + V11 * V11),
    {V10 / N, V11 / N}.
