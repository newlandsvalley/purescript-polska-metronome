polska-metronome
================

Try it [here](http://www.tradtunedb.org.uk/metronome).

Scandi polskas are usually notated in 3/4.  However, they exist in a variety of rhythms that are rarely, if ever, waltz-like. The characteristic rhythm has an emphasis on the first and third beat of the bar with the second beat understated.  Another way of looking at it is that the first and third beats are strictly in tempo, but the second may come earlier than expected.

This project is therefore a metronome for 3/4 rhythms, but one where the second beat can be shortened or lengthened (and where also the overall tempo can be changed).

To build
--------

    bower install
    npm run build

and then navigate in your browser to /dist.  The browser must (of course) support web-audio.

Credits
-------

This is a re-implementation (using PureScript, Halogen, Behaviors and Web-Audio) of the JavaScript metronome written (I think) by Ben Potton of the band [Jigfoot](http://www.jigfoot.com/) from whom the idea was shamelessly stolen.
