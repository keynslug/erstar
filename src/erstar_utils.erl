%% Copyright (c) 2013 Andrew Majorov <encube.ul@gmail.com>
%%
%% All Rights Reserved.
%%
%% Redistribution and use in source and binary forms, with or without modification,
%% are permitted provided that the following conditions are met:
%%
%% 1. Redistributions of source code must retain the above copyright notice, this
%%    list of conditions and the following disclaimer.
%%
%% 2. Redistributions in binary form must reproduce the above copyright notice,
%%    this list of conditions and the following disclaimer in the documentation
%%    and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%
%% Utilities

-module(erstar_utils).

%%

-export([
    render/2,
    render_to_file/3
]).

%%

-spec render(svg, erstar:rtree()) -> iolist().

render(To, RStar) ->
    Renderables = erstar:foldwide(fun render_entry/5, [], RStar),
    render_to(To, Renderables).

-spec render_to_file(string(), svg, erstar:rtree()) -> ok | {error, atom()}.

render_to_file(Filename, To, RStar) ->
    file:write_file(Filename, render(To, RStar)).

%%

render_to(svg, What) ->
    erstar_svg:render(What).

render_entry(_, empty, _, _, Acc) ->
    Acc;

render_entry(node, Bound, _, Level, Acc) ->
    [{bound, Bound, Level} | Acc];

render_entry(leaf, Bound, _, _, Acc) ->
    case erstar_bound:dimensions(Bound) of
        {W, H} when W == 0, H == 0 ->
            [{point, erstar_bound:lowerleft(Bound)} | Acc];
        _ ->
            [{rect, Bound} | Acc]
    end.
