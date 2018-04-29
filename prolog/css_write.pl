:- module(css_write, [css//1,
                      write_css/2]).

:- use_module(library(apply), [foldl/4]).
:- use_module(library(list_util), [take/3]).

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
css_child(Thing) -->
    { Thing =.. [Sel,StyleOrStyles],
      ( is_list(StyleOrStyles)
      -> Styles = StyleOrStyles
      ;  Styles = [StyleOrStyles] ),
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
    phrase(Css, Elements),
    phrase(css_tokens([], Elements), Codes), !,
    string_codes(String, Codes).

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

collapse_ampersands(Sels, CollapsedSels) :-
    foldl(add_selector, Sels, [], CollapsedSels).

add_selector([0'&|SubSel], Ctx, NewCtx) :-
    last(Ctx, Parent),
    append(Parent, SubSel, NewSel),
    butlast(Ctx, CtxHead),
    append(CtxHead, [NewSel], NewCtx), !.
add_selector(SubSel, Ctx, NewCtx) :-
    append(Ctx, [SubSel], NewCtx).
