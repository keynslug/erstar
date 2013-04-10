%%
%% Utilities.

-module(erstar_utils).

%%

-export([
    render/2,
    render_to_file/3
]).

%%

-spec render(svg, erstar:tree()) -> iolist().

render(To, RStar) ->
    Renderables = erstar:foldwide(fun render_entry/5, [], RStar),
    render_to(To, Renderables).

-spec render_to_file(string(), svg, erstar:tree()) -> ok | {error, atom()}.

render_to_file(Filename, To, RStar) ->
    file:write_file(Filename, render(To, RStar)).

%%

render_to(svg, What) ->
    erstar_svg:render(What).

render_entry(node, Bound, _, Level, Acc) ->
    [{bound, Bound, Level} | Acc];

render_entry(leaf, Bound, _, _, Acc) ->
    [{point, erstar_bound:lowerleft(Bound)} | Acc].
