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
           ['.thing'([margin('0 auto'),
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
p .thing {
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

:- end_tests(css_write).
