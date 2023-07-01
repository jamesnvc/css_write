:- module(css_write_t, []).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(css_write).
:- begin_tests(css_write).

user:extra_css -->
    css([code('font-family'('"PragmataPro Mono"'))]).

user:nested -->
    css(['*'(margin(0))]).

user:bloop -->
    css([span(color(red),
              [\(user:nested)])]).

user:main_css -->
    css([body(margin('3em')),
         \(user:extra_css),
         ul([],
            '&.bloop'(color(blue),
                      [li('font-weight'(bold),
                         '&:hover'(color(red)))])),
         p([color(red), 'font-size'(small)],
           ['>.thing'([margin('0 auto'),
                      'font-family'(monospace)]),
           \(user:bloop)])]).

test(css_basics) :-
    write_css(main_css, Txt),
    ExpectTxt = "body {
  margin: 3em;
}
code {
  font-family: \"PragmataPro Mono\";
}
ul.bloop {
  color: blue;
}
ul.bloop li {
  font-weight: bold;
}
ul.bloop li:hover {
  color: red;
}
p {
  color: red;
  font-size: small;
}
p >.thing {
  margin: 0 auto;
  font-family: monospace;
}
p span {
  color: red;
}
p span * {
  margin: 0;
}
", Txt = ExpectTxt.

test(import_statement,
    true( Txt == "@import url(\"https://example.com/font?family=123\");
body p {
  font-family: 123;
}
")) :-
    write_css(css(['@import'(url('https://example.com/font?family=123')),
                   body([], p('font-family'(123)))]),
              Txt).

test(media_query_1,
    true( Txt == "body {
  color: red;
}
@media screen and (min-width: 500px) and (max-width: 999px) {
body {
  color: blue;
}
body p {
  font-size: 2em;
}
}
body p {
  font-size: 1em;
}
")) :-
    write_css(
        css([body([color(red)],
                  ['@media'(and([screen, min_width('500px'), max_width('999px')]),
                           &([color(blue)],
                             p('font-size'("2em")))),
                   p('font-size'('1em'))])]),
        Txt).

test(media_query_2,
    true( Txt == "body {
  color: red;
}
body p {
  font-size: 1em;
}
@media screen and (min-width: 500px) and (max-width: 999px) {
body {
  color: blue;
}
body p {
  font-size: 2em;
}
}
")) :-
    write_css(
        css([body([color(red)], [p('font-size'('1em'))]),
             '@media'(and([screen, min_width('500px'), max_width('999px')]),
                      body([color(blue)],
                           p('font-size'("2em"))))]),
        Txt).
test(media_query_3,
    true( Txt == "body {
  color: red;
}
body p {
  font-size: 1em;
}
@media (prefers-color-scheme: dark) and (prefers-reduced-motion: reduced) {
body {
  color: blue;
}
body p {
  font-size: 2em;
}
}
")) :-
    write_css(
        css([body([color(red)], [p('font-size'('1em'))]),
             '@media'(and([color_scheme(dark), motion("reduced")]),
                      body([color(blue)],
                           p('font-size'("2em"))))]),
        Txt).

test(anim_keyframes_1,
    true( Txt ==  "@keyframes slidein {
  from {
  transform: translateX(0%);
  }
  to {
  transform: translateX(100%);
  }
}
")) :-
    write_css(
        css(['@keyframes'(
                 slidein,
                 [from(transform("translateX(0%)")),
                  to(transform("translateX(100%)"))])]),
        Txt).
test(anim_keyframes_2,
    true( Txt ==  ".foobar {
  color: blue;
  background-color: yellow;
}
@keyframes ping {
  0%, 100% {
  transform: translateY(-25%);
  animation-timing-function: cubic-bezier(0.8,0,1,1);
  }
  50% {
  transform: translateY(0);
  animation-timing-function: cubic-bezier(0,0,0.2,1);
  }
}
")) :-
    write_css(
        css(['.foobar'([color(blue), 'background-color'(yellow)]),
             '@keyframes'(
                 ping,
                 ['0%, 100%'(transform("translateY(-25%)"),
                             'animation-timing-function'("cubic-bezier(0.8,0,1,1)")),
                  '50%'(transform("translateY(0)"),
                        'animation-timing-function'("cubic-bezier(0,0,0.2,1)"))])]),
        Txt).

test(tailwind_1,
    true( Txt == ".bg-red-50 {
  background-color: rgba(254, 242, 242, var(--pl-bg-opacity, 1));
}
@media (min-width: 768px) {
.md\\:mt-1 {
  margin-top: 0.25rem;
}
}
.hidden {
  display: none;
}
.hover\\:bg-pink-100:hover {
  background-color: rgba(252, 231, 243, var(--pl-bg-opacity, 1));
}
.text-red-500 {
  color: rgba(239, 68, 68, var(--pl-text-opacity,1));
}
")) :-
    write_css(
        css(['.bg-red-50'('background-color'("rgba(254, 242, 242, var(--pl-bg-opacity, 1))")),
             '@media'(and([min_width("768px")]),
                      '.md\\:mt-1'(['margin-top'("0.25rem")])),
             '.hidden'(display("none")),
             '.hover\\:bg-pink-100:hover'('background-color'("rgba(252, 231, 243, var(--pl-bg-opacity, 1))")),
             '.text-red-500'(color("rgba(239, 68, 68, var(--pl-text-opacity,1))"))

            ]),
        Txt).

test(supports_query,
    true( Txt == "@supports (color: oklch(0% 0 0)) {
html {
  background-color: oklch(50% 0.163 56.65);
}
}
")) :-
    write_css(
        css(['@supports'(color('oklch(0% 0 0)'),
                       html('background-color'("oklch(50% 0.163 56.65)")))]),
        Txt
    ).

test(supports_query_complex,
    true( Txt == "@supports ((color: oklch(0% 0 0)) and (selector(:has(a, b))) and not ((text-stroke: 10px) or (-webkit-text-stroke: 10px))) {
html {
  background-color: oklch(50% 0.163 56.65);
}
}
")) :-
    write_css(
        css(['@supports'(and([color('oklch(0% 0 0)'), "selector(:has(a, b))",
                              not(or(["text-stroke: 10px", "-webkit-text-stroke: 10px"]))]),
                         html('background-color'("oklch(50% 0.163 56.65)")))]),
        Txt
    ).

:- end_tests(css_write).
