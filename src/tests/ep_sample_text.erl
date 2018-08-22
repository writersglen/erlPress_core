%%% ==========================================================================
%%% ep_sample_text.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_sample_text.erl
%%%   Description:  Sample text blocks 
%%% @end

%%% ==========================================================================

-module (ep_sample_text).

-export ([times_14/0, helvetica_10/0, the_road_not_taken/0]).
-export ([code_listing/0, ul/0, cl/0, article/0]).
-export([erlpress/0]).

%% @doc  Display Times-Roman 14pt text
%% NOTE: Must apply ep_block:default_14/0 for this to 
%%       show advertised font size 


times_14() ->
"<p>This is normal text set in 14/21 Times Roman.
It includes <em>emphasized terms,</em> set in Times-Italic. The TeX
hyphenation algorithm is implemented.  The term <code>{person, 
\"Joe\"}</code> is an Erlang term.
The variable <code>X</code>, was immediately followed by
a comma. The justification algorithm does proper <em>kerning</em>.
AWAY is correctly kerned! Erlang terms <code>{like, this}</code>
are typeset in <em>courier.</em></p>".


%% @doc  Display Helvetica 10pt text
%% NOTE: Must apply ep_typespec:default_helvetics(10) for this to 
%%       show advertised font size 

helvetica_10() ->
    "<p>This is normal text set in 10/15 Helvetica.
It includes <em>emphasized terms,</em> set in Helvetica-Oblique. The TeX
hyphenation algorithm is implemented.  The term <code>{person, 
\"Joe\"}</code> is an Erlang term.
The variable <code>X</code>, was immediately followed by
a comma. The justification algorithm does proper <em>kerning</em>.
 AWAY is correctly kerned! Erlang terms <code>{like, this}</code>
are typeset in <em>courier.</em></p>".


the_road_not_taken() ->
"<p>  </p>
<p>Two roads diverged in a yellow wood,
And sorry I could not travel both
And be one traveler, long I stood
And looked down one as far as I could
To where it bent in the undergrowth;</p>
     <p> - <em>Robert Frost</em></p>".


code_listing() ->
"<h3><b>Listing: cdoc test</b></h3>
<br>  </br>
<p>run()->
    OutFile = \"Test\",
    PDF = eg_pdf:new(),
     PanelNameF                    = {1, 3, code},
     CopyF                         = ep_sample_text:code_listing(),
     PositionF                     = {72, 530},
     SizeF                         = {210, 150},
     PanelMapF1                    = ep_panel:create(PanelNameF, PositionF, SizeF),
     PanelMapF2                    = ep_panel:reveal(PanelMapF1),
     PanelMapF3                    = ep_panel:update_typestyle(cdoc, PanelMapF2),
     {PasteF, _Spill, _PanelMapF4} = ep_text_block:fit_copy(CopyF, PanelMapF3),
     ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMapF3),
     ok                            = ep_paste_lib:paste(PDF, PasteF, [], PanelMapF3),
     ep_job:save_job(PDF, OutFile).</p>".


ul() ->
"<p>This is a test of an <b>unordered list</b> with a bold phrase.</p>
<ul>
<li>Display Adobe Type I fonts</li>
<li>Position and copyfit text</li>
<li>Justify, kern, and rotate text</li>
<li>Specify and position images and graphic elements</li>
<li>Support checklists</li>
</ul>
<p>More copy</p>".


cl() ->
"<h3>Roadmap:</h3>
<br> </br>
<ci>Extend and debug Markdown parsing</ci>
<ci>Ordered lists</ci>
<ci>Footnotes</ci>
<ci>Tables</ci>
<ci>Articles and beads</ci>
<ci>Test! Test! Test!</ci>
<ci>TrueType and OpenType fonts</ci>
<ci>Impostion and layout functions</ci>
<ci>PDF-to-Erlang functions</ci>
<ci>Applications for diverse print formats</ci>
<br> </br>
<p><em>erlPress_core.01</em> provides a foundation for development of Erlang applications that support creative generation of print formats ranging from business cards to technical books. We immodestly imagine the evolution of a comprehensive Human Communication Platform, HCP, parallel to OTP.</p>".



article() ->
"<h1>This is a headline</h1>
<h2>This is a deck</h2>
<p>    </p>
<p>This is normal text set in 14/21 Times Roman.
It includes <em>emphasized terms,</em> set in Times-Italic. The TeX
hyphenation algorithm is implemented.  The term <code>{person, 
\"Joe\"}</code> is an Erlang term.</p>
<h3>This is a subhead</h3>
<p>The variable <code>X</code>, was immediately followed by
a comma. The justification algorithm does proper <em>kerning</em>,
which is more than <em>Microsoft Word</em> can do. AWAY is
correctly kerned! Erlang terms <code>{like, this}</code>
are typeset in <em>courier.</em></p>".



erlpress() ->
"<h1>erlPress_core</h1>
<br> </br>
<p>Generate PDFs from your Erlang applications!</p>
<p>User applications based on the Adobe Portable Document Format, PDF, compose text, images, 2-D geometric objects, and presentation instructions for distribution across diverse output devices. The goal is faithful and consistent display of human-readable content. Presentation instructions are encoded in the page description language Adobe PostScript.</p>
<p><em>erlPress_core.01</em> functions transform human-readable content into Erlang data structures, generate Postscript object code, and format the code into properly structured PDF files. Based on <em>PDF Reference: third edition, Version 1.4</em>, <em>erlPress_lib.01</em> provides functions to:</p>
<ul>
<li>Display Adobe Type I fonts</li>
<li>Position and copyfit text</li>
<li>Justify, kern, and rotate text</li>
<li>Specify and position images and graphic elements</li>
<li>Support checklists</li>
</ul>
<p><em>erlPress_lib.01</em> revises and extends the Hugh Watkins fork of the Erlang Erlguten library originally developed by Joe Armstrong. See: <em>https://github.com/hwatkins/erlguten</em></p>
<h3>Roadmap:</h3>
<br> </br>
<ci>Extend and debug Markdown parsing</ci>
<ci>Ordered lists</ci>
<ci>Footnotes</ci>
<ci>Tables</ci>
<ci>Articles and beads</ci>
<ci>Test! Test! Test!</ci>
<ci>TrueType and OpenType fonts</ci>
<ci>Impostion and layout functions</ci>
<ci>PDF-to-Erlang functions</ci>
<ci>Applications for diverse print formats</ci>
<br> </br>
<p><em>erlPress_core.01</em> provides a foundation for development of Erlang applications that support creative generation of print formats ranging from business cards to technical books. We immodestly imagine the evolution of a comprehensive Human Communication Platform, HCP, parallel to OTP.</p>

<p> Join in!</p>

<p> Help make it so.</p>".
