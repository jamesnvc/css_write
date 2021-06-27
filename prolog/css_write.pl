:- module(css_write, [css//1,
                      write_css/2]).

:- use_module(library(apply), [foldl/4, partition/4]).
:- use_module(library(lists), [append/3, last/2]).
:- use_module(library(list_util), [take/3]).
:- use_module(library(yall)).
:- use_module(library(debug)).

ensure_list(X, X) :- is_list(X), !.
ensure_list(X, [X]).

butlast(Ls, Lss) :-
    length(Ls, L),
    ButL is L - 1,
    take(ButL, Ls, Lss).

% Copied from list_util:split/3, but addressing
% https://github.com/mndrix/list_util/issues/30
% which makes joining strings that already have spaces in them fail
split([], _, [[]]) :-
    !.  % optimization
split([Div|T], Div, [[]|Rest]) :-
    split(T, Div, Rest),  % implies: dif(Rest, [])
    !.  % optimization
split([H|T], Div, [[H|First]|Rest]) :-
    %% dif(H, Div),
    split(T, Div, [First|Rest]).

%!  css(+Content)// is det
%
%  Generate CSS string from Content.
css(Content) -->
    css_children(Content), { ! }.

css_children([]) --> [], { ! }.
css_children([Thing|Things]) -->
    css_child(Thing), css_children(Things).

css_child(\(Reference)) -->
   call(Reference).
css_child('@import'(Arg)) --> !, ['@import'(Arg)].
css_child('@media'(Query, Children)) -->
    !,
    [begin_media(Query)],
    { ensure_list(Children, Children_) },
    css_children(Children_),
    [end_media].
css_child('@keyframes'(Name, Frames)) -->
    !,
    { ensure_list(Frames, Frames_),
      text_to_string(Name, NameStr),
      string_codes(NameStr, NameCodes) },
    [begin_animation(NameCodes)],
    keyframes(Frames_),
    [end_animation].
css_child(Thing) -->
    { Thing =.. [Sel,StyleOrStyles],
      ensure_list(StyleOrStyles, Styles),
      text_to_string(Sel, SelStr),
      string_codes(SelStr, SelStrCodes) },
    [begin_styles(SelStrCodes)],
    css_styles(Styles),
    [end_styles(SelStrCodes)].
css_child(Thing) -->
    { Thing =.. [Sel,Styles,Children],
      ThingStyles =.. [Sel,Styles] },
    css_child(ThingStyles),
    { text_to_string(Sel, SelStr),
      string_codes(SelStr, SelStrCodes),
      ensure_list(Children, Children_) },
    [begin_ctx(SelStrCodes)],
    css_children(Children_),
    [end_ctx(SelStrCodes)].

css_styles([]) --> [], { ! }.
css_styles([Style|Styles]) -->
    css_style(Style), css_styles(Styles).

css_style(Style) -->
    { Style =.. [Attr, Value],
      atom_codes(Attr, AttrCodes),
      atom_codes(Value, ValueCodes) },
    [style(AttrCodes, ValueCodes)].

:- meta_predicate write_css(//, -).
%!  write_css(+Css, -String) is semidet.
%
%   True when String is the Css DCG written out as a string.
write_css(Css, String) :-
    phrase(Css, Elements0),
    partition(['@import'(_)]>>true,
              Elements0, ImportRules, Elements),
    phrase(import_rules(ImportRules), Codes, Codes1),
    phrase(css_tokens([], Elements), Codes1), !,
    string_codes(String, Codes).

import_rules(['@import'(Arg)|Rest]) -->
    "@import ",
    { ensure_list(Arg, ArgL) }, import_args(ArgL),
    ";\n",
    import_rules(Rest).
import_rules([]) --> [].

import_args([url(URL)|Rest]) -->
    !,
    "url(\"",
    { text_to_string(URL, URLStr),
      string_codes(URLStr, URLCodes) },
    URLCodes, "\")",
    import_args(Rest).
import_args([X|Rest]) -->
    " ",
    { text_to_string(X, Str),
      string_codes(Str, Codes) },
    Codes,
    import_args(Rest).
import_args([]) --> [].

css_tokens(_, []) --> [].
css_tokens(Ctx, [begin_styles(S),end_styles(S)|Next]) -->
    css_tokens(Ctx, Next), { ! }.
css_tokens(Ctx, [begin_styles(SelCodes)|Next]) -->
    { append(Ctx, [SelCodes], CombinedCtx),
      collapse_ampersands(CombinedCtx, DerivedSels),
      split(DerivedSel, 0' , DerivedSels) },
    DerivedSel, " {\n", css_tokens(Ctx, Next).
css_tokens(Ctx, [end_styles(_)|Next]) -->
    "}\n", css_tokens(Ctx, Next).
css_tokens(Ctx, [style(Prop, Val)|Next]) -->
    "  ", Prop, ": ", Val, ";\n",
    css_tokens(Ctx, Next).
css_tokens(Ctx, [begin_ctx(AddCtx)|Next]) -->
    { append(Ctx, [AddCtx], NewCtx) },
    css_tokens(NewCtx, Next).
css_tokens(Ctx, [end_ctx(_)|Next]) -->
    { butlast(Ctx, NewCtx) },
    css_tokens(NewCtx, Next).
css_tokens(Ctx, [begin_media(Query)|Next]) -->
    "@media ", media_query(Query), " {\n",
    css_tokens(Ctx, Next).
css_tokens(Ctx, [end_media|Next]) -->
    "}\n",
    css_tokens(Ctx, Next).
css_tokens(Ctx, [begin_animation(Name)|Next]) -->
    "@keyframes ", Name, " {\n",
    css_tokens(Ctx, Next).
css_tokens(Ctx, [begin_keyframe(Pos)|Next]) -->
    "  ", Pos, " {\n",
    css_tokens(Ctx, Next).
css_tokens(Ctx, [end_keyframe|Next]) -->
    "  }\n",
    css_tokens(Ctx, Next).
css_tokens(Ctx, [end_animation|Next]) -->
    "}\n",
    css_tokens(Ctx, Next).

media_query(and(Qs)) -->
    !, media_query_ands(Qs).
media_query(Elt) -->
    media_query_elt(Elt).

media_query_ands([A,B|Rest]) -->
    media_query(A),
    " and ",
    media_query_ands([B|Rest]).
media_query_ands([E]) -->
    media_query(E).
media_query_ands([]) --> [].

media_query_elt(max_width(W)) -->
    !,
    { text_to_string(W, S),
      string_codes(S, Cs) },
    "(max-width: ",  Cs, ")".
media_query_elt(min_width(W)) -->
    !,
    { text_to_string(W, S),
      string_codes(S, Cs) },
    "(min-width: ",  Cs, ")".
media_query_elt(color_scheme(Theme)) -->
    !,
    { text_to_string(Theme, S),
      string_codes(S, Cs) },
    "(prefers-color-scheme: ",  Cs, ")".
media_query_elt(motion(Type)) -->
    !,
    { text_to_string(Type, S),
      string_codes(S, Cs) },
    "(prefers-reduced-motion: ",  Cs, ")".
media_query_elt(X) -->
    { text_to_string(X, S),
      string_codes(S, Cs) },
    Cs.

collapse_ampersands(Sels, CollapsedSels) :-
    foldl(add_selector, Sels, [], CollapsedSels).

add_selector([0'&|SubSel], Ctx, NewCtx) :-
    last(Ctx, Parent),
    append(Parent, SubSel, NewSel),
    butlast(Ctx, CtxHead),
    append(CtxHead, [NewSel], NewCtx), !.
add_selector(SubSel, Ctx, NewCtx) :-
    append(Ctx, [SubSel], NewCtx).

keyframes([]) --> [].
keyframes([Frame|Frames]) -->
    { Frame =.. [FramePos|Styles],
      debug(xxx, "Frame ~w: ~w ~w", [Frame, FramePos, Styles]),
      text_to_string(FramePos, FramePosString),
      string_codes(FramePosString, FramePosCodes) },
    [begin_keyframe(FramePosCodes)],
    css_styles(Styles),
    [end_keyframe],
    keyframes(Frames).
