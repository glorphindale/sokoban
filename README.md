# Sokoban of the Dead

You wake up at the cemetery. It's getting dark, and all of the tombstones are moved.
Zombies start to rise from their graves. You must put tombstones back, or... Brainzzzzzz...

Clojure app for playing sokoban.
Inspired by the [RubyQuiz](http://www.rubyquiz.com/quiz5.html) and based on the [The Caves of Clojure](http://stevelosh.com/blog/2012/07/caves-of-clojure-01/).

## Usage

Place your own levels into the "levels" directory. Both single level files and collections (.slc) files are accepted.

j/k/pageup/pagedown to select level

h/j/k/l to move

r to restart, q to return to the level selection, u to undo moves, escape to exit

## Resources

Level collection was taken from [Sourcecode](http://www.sourcecode.se/sokoban/levtext.php?file=Original.slc).

## Future work

* Add redo?
* Add backstory
* Add handling of big levels - scrolling, centering viewport on the player
* Add proper font selection (looks ugly in linux swing)
* Add general-purpose draw-text-centered routine
* Add tutorial

## License

Copyright © 2013 glorphindale

Distributed under the Eclipse Public License, the same as Clojure.
