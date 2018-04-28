:- module(css_write, [css//1,
                      write_css/2]).

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

:- dynamic initial_context/1.

%!  css(+Content)// is det
%
%  Generate CSS string from Content.
css(Content) -->
    { ensure_list(Content, Content_),
      (initial_context(Ctx) ; Ctx = []) },
    css_children(Ctx, Content_),
    { ! }.

css_children(_, []) --> [], { ! }.
css_children(Ctx, [Thing|Things]) -->
    css_child(Ctx, Thing), css_children(Ctx, Things).

add_selector(Ctx, [0'&|SubSel], NewCtx) :-
    last(Ctx, Parent),
    append(Parent, SubSel, NewSel),
    butlast(Ctx, CtxHead),
    append(CtxHead, [NewSel], NewCtx), !.
add_selector(Ctx, SubSel, NewCtx) :-
    append(Ctx, [SubSel], NewCtx).

selector_styles(_, []) --> [], { ! }.
selector_styles(Selector, Styles) -->
    Selector, " {\n", css_styles(Styles), "}\n".

css_child(Ctx, \(Reference), In, Out) :-
   setup_call_cleanup(
       assert(initial_context(Ctx)),
       call(Reference, In, Out),
       retract(initial_context(Ctx))).
css_child(Ctx, Thing) -->
    { Thing =.. [Sel,StyleOrStyles],
      ( is_list(StyleOrStyles)
      -> Styles = StyleOrStyles
      ;  Styles = [StyleOrStyles] ),
      text_to_string(Sel, SelStr),
      string_codes(SelStr, SelStrCodes_),
      add_selector(Ctx, SelStrCodes_, CurrentCtx),
      split(Selector, 0' , CurrentCtx) },
    selector_styles(Selector, Styles).
css_child(Ctx, Thing) -->
    { Thing =.. [Sel,Styles,Children],
      ThingStyles =.. [Sel,Styles] },
    css_child(Ctx, ThingStyles),
    { text_to_string(Sel, SelStr),
      string_codes(SelStr, SelStrCodes),
      add_selector(Ctx, SelStrCodes, SubCtx),
      ensure_list(Children, Children_)
    },
    css_children(SubCtx, Children_).

css_styles([]) --> [], { ! }.
css_styles([Style|Styles]) -->
    css_style(Style), css_styles(Styles).

css_style(Style) -->
    { Style =.. [Attr, Value],
      atom_codes(Attr, AttrCodes),
      atom_codes(Value, ValueCodes) },
    "  ", AttrCodes, ": ", ValueCodes, ";\n".

:- meta_predicate write_css(//, -).
%!  write_css(+Css, -String) is semidet.
%
%   True when String is the Css DCG written out as a string.
write_css(Css, String) :-
    phrase(Css, Codes),
    string_codes(String, Codes).
