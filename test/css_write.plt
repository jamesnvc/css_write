:- module(css_write_t, []).
:- use_module(library(plunit)).
:- use_module(css_write).
:- begin_tests(css_write).

user:extra_css -->
    css([code('font-family'('"PragmataPro Mono"'))]).

user:bloop -->
    css([span(color(red))]).

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
", Txt = ExpectTxt.

:- end_tests(css_write).
