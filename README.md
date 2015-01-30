# Josh's terrible sawfish configuration #

Hey there!

This repository contains a number of configurations and scripts I've
either used or written for my
[Sawfish Window Manager](http://sawfish.wikia.com/) setup.

**A disclaimer:** This repository isn't all that well organized (see
 title). If you clone it and put it in your `~/.sawfish`, a *lot* of
 things won't work, just because there are a lot of references to
 absolute paths that probably won't exist on your system
 (e.g. `/home/YOURUSERNAMEHERE/scripts/blah`). Also, it's poorly
 documented. If at all.

*But* there are things you might find useful.

## Things you *might* find useful ##

* "ceftoolbar" is a tool I made using the
  [Chromium Embedded Framework](https://code.google.com/p/chromiumembedded/),
  [conky](http://conky.sourceforge.net/), [d3.js](http://d3js.org/),
  and a bunch of other tools to display the current state of my
  system. It displays information like the CPU usage, RAM,
  temperature, current music playing, etc.

* `lisp/subdivide.jl` has a couple cool tools for binary window tiling
  with padding in Sawfish. I know Sawfish comes with some tools for
  tiling, but those weirded me out because they bind to desktops and
  automatically tile new windows.
