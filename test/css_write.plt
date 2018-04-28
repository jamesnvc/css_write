:- use_module(library(plunit)).
:- use_module(css_write).
:- begin_tests(css_write).

main_css -->
    css([body(margin('3em')),
         ul([],
            '&.bloop'(color(blue),
                      [li('font-weight'(bold),
                         '&:hover'(color(red)))])),
         p([color(red), 'font-size'(small)],
           ['.thing'([margin('0 auto'),
                      'font-family'(monospace)])])]).

test(css_basics) :-
    write_css(main_css, Txt),
    ExpectTxt = "body {
  margin: 3em;
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
", Txt = ExpectTxt.

:- end_tests(css_write).
