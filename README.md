purescript-polska-metronome
===========================

Try it [here](http://www.tradtunedb.org.uk/#/metronome).

Scandi polskas are usually notated in 3/4.  However, they exist in a variety of rhythms that are rarely, if ever, truly waltz-like. The characteristic rhythm has an emphasis on the first and third beat of the bar with the second beat understated.  For example, in the __short first beat__ polska, the first and third beats are strictly in tempo, but the second comes earlier than expected.

This project is a metronome for 3/4 rhythms with three 'beat markers' drawn in a line, but where the position of the middle marker can be shifted, thus skewing the beat. Three polska variants are supported: 

*  Short first beat 
*  Long first beat
*  Finnskogpols (short final beat)

Additionally, the __Even__ variant, with each beat in tempo, can be represented by one of the other forms if you choose the skew of the middle beat marker to be zero.

To build as a library module
----------------------------

    npm run build

or

    bower install
    pulp build


To build the example
--------------------

    npm run example

and then navigate in your browser to /example/dist.  The browser must (of course) support web-audio.

Credits
-------

This is an adaptation (using PureScript, Halogen, Behaviors and Web-Audio) of the JavaScript metronome written (I think) by Ben Potton of the band [Jigfoot](http://www.jigfoot.com/) from whom the idea was shamelessly stolen.
