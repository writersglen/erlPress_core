```
            _ _____
           | |  __ \
   ___ _ __| | |__) | __ ___  ___ ___
  / _ \ '__| |  ___/ '__/ _ \/ __/ __|
 |  __/ |  | | |   | | |  __/\__ \__ \
  \___|_|  |_|_|   |_|  \___||___/___/
```

# erlPress_core

**erlPress_core** modifies and extends the _Hugh Watkins_ fork of Erlang [Erlguten](https://github.com/hwatkins/erlguten) originally developed by _Joe Armstrong_.

**erlPress_core** is the base library for many as-yet-to_be-developed Erlang applications for creative print media publishing tools.

To review current erlPpress_core functionality, display [ep_show_n_tell](pdf/galleys/ep_show_n_tell.pdf).

See [Programmer Guide](src/guide/programmer_guide.txt) for API details.

**erlPress_core**, is a work-in-progress. Should you find deficiencies or rough-edges, please let us know. The **MIT license** encourages evolution toward world-class print-media design and development applications ranging from business cards to books.

Imagine:

* Easy-to-use documentation tools
* An application for development of marketing collateral along the lines Scribus, but web-based
* An Erlang-based book-publishing pipeline

See features: ep_show_n_tell.pdf in pdf directory ([source](src/tests/ep_show_n_tell.erl)).

**erlPress_core** so far offers three type styles. But you can create your own. In the src/typespec directory, see:

* ep_report_sty.erl
* ep_report_hv_sty.erl
* ep_cdoc_sty.erl


# Programmer Guide

Notes toward a programmer's guide can be found in the guide [directory](src/guide/programmer_guide.txt).


# Directories

* src/
  * eg prefixed modules -- from Erlguten
  * ep prefixed modules -- from Erlguten with minor modifications 

* src/
  * tests     -- erlPress_core feature tests
  * guide     -- Notes toward a programmer's guide -- WARNING: under constuction
  * content   -- Markdown input -- WARNING: under construction
  * fonts     -- Font modules; mostly from erlguten
  * typespecs -- Type styling a specifications
  * layout    -- layut functions
  * copyfit   -- Copyfitting routines and primatives
  * lib       -- Useful libraries

# Helper/Utility Directories

* src/
  * grid       -- page design functions -- WARNING: under construction
  * image      -- image functions
  * job        -- resource management functions
  * line       -- line functions
  * media      -- display media functions
  * metrics    -- measurement conversion functions 
  * page       -- page numbering functions
  * shape      -- geometric shape functions
  * text       -- text display functions

# 3rd Party Libs

* [CMARK](https://github.com/skaee/cmark): CommonMark parsing and rendering library and program in C to convert cmark_nodes to ERLANG terms strings
