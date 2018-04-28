# write_css

This Prolog library provides a DCG, `css//1` for generating CSS in the style of [`html//`](http://www.swi-prolog.org/pldoc/doc_for?object=html//1).

You can use this to write CSS rules as nested functors with arity one or two; see below for examples.

One-arity functors are interpreted as `selector(styles)`, so for example `p(margin('3em'))` will generate the CSS rule `p { margin: 3em; }`.
The `styles` can also be a list, to provide multiple styles (e.g. `p([margin('3em'), 'font-size'(small)])` generates `p { margin: 3em; font-size: small; }`).

For two-arity functors, the first argument is the styles, as above, and the second argument will be nested child selectors.
For example, `p(margin('3em'), [img(width('300px'), strong('font-size'(large)))])` generates:

```css
p { margin: 3em; }
p img { width: 300px; }
p strong { font-size: large; }
```

This can of course themselves be nested, so you could write:

```prolog
p([],
  strong([color(blue)],
         emph([color(red)]))).
```

to generate

```css
p strong { color: blue; }
p strong emph { color: red; }
```

## Examples

Using with `html_write`

```prolog
:- use_module(library(css_write), [css//1, write_css/2]).
:- use_module(library(http/html_write), [reply_html_page/2,
                                         html//1,
                                         html_post//2,
                                         html_receive//1]).

main_css -->
    css([body(margin('3em')),
         p([color(red), 'font-size'(small)],
           ['.thing'([margin('0 auto'),
                      'font-family'(monospace)])])])

home_page(_Request) :-
   reply_html_page([title('CSS Demo'),
                    \html_receive(css)],
                    \home_page_body).

include_css(CssDcg) -->
  { write_css(CssDcg, CssTxt) },
  html_post(css, style([], CssTxt)).

home_page_body -->
  html([\include_css(main_css),
       div(id(main),
           [p([],
              ["Hello world",
               strong(class(thing), "Some stuff")])])]).
```
