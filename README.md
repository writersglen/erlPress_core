            _ _____
           | |  __ \
   ___ _ __| | |__) | __ ___  ___ ___
  / _ \ '__| |  ___/ '__/ _ \/ __/ __|
 |  __/ |  | | |   | | |  __/\__ \__ \
  \___|_|  |_|_|   |_|  \___||___/___/


=============
erlPress_core
=============

erlPress_core modifies and extends the Hugh Watkins fork of the Erlang Erlguten library originally developed by Joe Armstrong.

https://github.com/hwatkins/erlguten

erlPress_core is the base library for many as-yet-to_be-developed Erlang applications for creative print media and publishing tools.

To review current erlPpress_core functionality, display ../pdf/galleys/ep_show_n_tell.pdf.

To see ep_show_n_tell.pdf source, see ../tests/ep_show_n_tell.erl.

See ../src/guides/prograpmmer_guide.txt for programming details. 

ErlPress_core, is a work-in-progress. Should you find deficiencies and rough-edges, we'd love to hear abouat them. The MIT license encourages evolution toward world-class print-media design and development applications ranging from business cards to books.

Imagine:

. Easy-to-use documentation tools

. An application for development of marketing collateral along the lines Scribus, but web-based

. An Erlang-based book-publishing pipeline

See features: ep_show_n_tell.pdf in pdf directory

See ~/tests/ep_show_n_tell.erl for source

erlPress_core so far offers three type styles. But you can create your own. In the ~/typespec directory, see:

. ep_report_sty.erl

. ep_report_hv_sty.erl

. ep_cdoc_sty.erl


==========
Programmer Guide
==========

Notes toward a programmer's guide can be found in the guide directory.


===========
Directories
===========

. tests     -- erlPress_core feature tests
. guide     -- Notes toward a programmer's guide -- WARNING: under constuction
. content   -- Markdown input -- WARNING: under construction
. fonts     -- Font modules; mostly from erlguten
. typespecs -- Type styling a specifications
. copyfit   -- Copyfitting routines and primatives
. lib       -- Useful libraries 


