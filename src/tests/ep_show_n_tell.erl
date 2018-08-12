%%% ==========================================================================
%%% ep_show_n_tell.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_show_n_tell.erl
%%%   Description:  Test page construction 
%%% @end

%%% ==========================================================================


-module(ep_show_n_tell).

-export([run/0]).


run()->
    Job = ep_job:create("erlPress: Highlights", "LRP"),
    OutFile = "./pdf/galleys/" ++ ?MODULE_STRING ++ ".pdf", 

    PDF = eg_pdf:new(),
    ep_pagination:first_page(PDF),  % ****** Page1

    %% Page 1 Make up

   Copy                        = ep_sample_text:erlpress(),
   PanelMap                    = ep_panel:create({1, 1, test}, {72, 20}, {460, 705}),
   PanelMap1                   = ep_panel:reveal(PanelMap),
   {Paste, _Spill, _PanelMap2} = ep_text_block:fit_copy(Copy, PanelMap1),
   ok                          = ep_paste_lib:paste_panel(PDF, Job, PanelMap1),
   ok                          = ep_paste_lib:paste(PDF, Paste, [], PanelMap1),

    ep_pagination:next_page(PDF),    % ****** Page 2
   
    TextA         = "Best of erlPress_core.01",
    FontA         = "Helvetica",
    PositionA     = {72, 72},
    FontSizeA     = 36,
    TextMapA      = ep_text:create(FontA, PositionA, FontSizeA), 
    ep_text:one_line(PDF, TextA, Job, TextMapA),

    TextB          = "Justified Text",
    FontB          = "Helvetica",
    PositionB      = {72, 125},
    FontSizeB      = 24,
    TextMapB       = ep_text:create(FontB, PositionB, FontSizeB), 
    ep_text:one_line(PDF, TextB, Job, TextMapB),


    PanelNameC                   = {1, 1, justify}, 
    CopyC                        = ep_sample_text:times_14(),
    PositionC                    = {72, 140},
    SizeC                        = {450, 150},
    PanelMapC                    = ep_panel:create(PanelNameC, PositionC, SizeC),
    {PasteC, _Spill, _PanelMapC} = ep_text_block:fit_copy(CopyC, PanelMapC),
    ep_paste_lib:paste(PDF, PasteC, [], PanelMapC),


    TextD          = "Boxed Text",
    FontD          = "Helvetica",
    TextPositionD  = {72, 280},
    FontSizeD      = 24,
    TextMapD       = ep_text:create(FontD, TextPositionD, FontSizeD), 
    ep_text:one_line(PDF, TextD, Job, TextMapD),

 
     PanelNameE                    = {1, 2, boxed_text}, 
     CopyE                         = ep_sample_text:helvetica_10(),
     PositionE                     = {72, 320},
     SizeE                         = {450, 110},
     PanelMapE1                    = ep_panel:create(PanelNameE, PositionE, SizeE),
     PanelMapE2                    = ep_panel:update_typestyle(justify_report_hv, PanelMapE1),
     PanelMapE3                    = ep_panel:reveal(PanelMapE2),
     PanelMapE4                    = ep_panel:update_background_color(gainsboro, PanelMapE3),
     {PasteE, _Spill, _PanelMapE5} = ep_text_block:fit_copy(CopyE, PanelMapE4),
     ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMapE4),
     ok                            = ep_paste_lib:paste(PDF, PasteE, [], PanelMapE4),


    Text1S         = "Poetry (Preformatted Text)",
    Font1S         = "Helvetica",
    TextPosition1S = {72, 500},
    FontSize1S     = 24,
    TextMap1S      = ep_text:create(Font1S, TextPosition1S, FontSize1S), 
    ep_text:one_line(PDF, Text1S, Job, TextMap1S),


     PanelNameF                    = {1, 3, poetry}, 
     CopyF                         = ep_sample_text:the_road_not_taken(),
     PositionF                     = {72, 530},
     SizeF                         = {210, 150},
     PanelMapF1                    = ep_panel:create(PanelNameF, PositionF, SizeF),
     PanelMapF2                    = ep_panel:reveal(PanelMapF1),
     PanelMapF3                    = ep_panel:update_typestyle(preformatted_report, PanelMapF2),
     {PasteF, _Spill, _PanelMapF4} = ep_text_block:fit_copy(CopyF, PanelMapF3),
     ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMapF3),
     ok                            = ep_paste_lib:paste(PDF, PasteF, [], PanelMapF3),


    ep_pagination:next_page(PDF),    % ****** Page 2

    Text2N         = "Centered Text",
    Font2N         = "Helvetica",
    TextPosition2N = {72, 72},
    FontSize2N     = 24,
    TextMap2N      = ep_text:create(Font2N, TextPosition2N, FontSize2N), 
    ep_text:one_line(PDF, Text2N, Job, TextMap2N),



     PanelNameG                    = {2, 1, centered}, 
     CopyG                         = ep_sample_text:times_14(),
     PositionG                     = {72, 120},
     SizeG                         = {450, 170},
     PanelMapG1                    = ep_panel:create(PanelNameG, PositionG, SizeG),
     PanelMapG2                    = ep_panel:reveal(PanelMapG1),
     PanelMapG3                    = ep_panel:update_background_color(gainsboro, PanelMapG2),
     PanelMapG4                    = ep_panel:update_typestyle(centered_report, PanelMapG3),
     {PasteG, _Spill, _PanelMapG5} = ep_text_block:fit_copy(CopyG, PanelMapG4),
     ok                            = ep_paste_lib:paste_panel(PDF, Job, PanelMapG4),
     ok                            = ep_paste_lib:paste(PDF, PasteG, [], PanelMapG4),

 

    Text2M         = "Right Justified Text",
    Font2M         = "Helvetica",
    TextPosition2M = {72, 362},
    FontSize2M     = 24,
    TextMap2M      = ep_text:create(Font2M, TextPosition2M, FontSize2M), 
    ep_text:one_line(PDF, Text2M, Job, TextMap2M),


     PanelNameH                      = {2, 2, ragged_left},
     CopyH                           = ep_sample_text:times_14(),
     PositionH                       = {72, 380},
     SizeH                           = {450, 110},
     PanelMapH1                      = ep_panel:create(PanelNameH, PositionH, SizeH),
     PanelMapH2                      = ep_panel:reveal(PanelMapH1),
     PanelMapH3                      = ep_panel:reveal(PanelMapH2),
     PanelMapH4                      = ep_panel:update_radius(0, PanelMapH3),
     PanelMapH5                      = ep_panel:update_typestyle(ragged_left_report, PanelMapH4),
     {PasteH, _Spill, _PanelMapH6}   = ep_text_block:fit_copy(CopyH, PanelMapH5),
     ok                              = ep_paste_lib:paste_panel(PDF, Job, PanelMapH5),
     ok                              = ep_paste_lib:paste(PDF, PasteH, [], PanelMapH5),

%% NOTE: The following example demonstrates how to position and display text
%%       Note that Y coordinate of text sample is not inverted; e.g. text will
%%       assume that Y coordinate is at the bottom of the page. 
%%       ErlPress functions invert Y coordiante so that Y = 0 is at the 
%%       top of the page-- bit of a hassle, but is more consistent with 
%%       human scanning of pages from top to bottom. 

    Text2O         = "Kerning",
    Font2O         = "Helvetica",
    TextPosition2O = {72, 570},
    FontSize2O     = 24,
    TextMap2O      = ep_text:create(Font2O, TextPosition2O, FontSize2O), 
    ep_text:one_line(PDF, Text2O, Job, TextMap2O),


    eg_pdf:begin_text(PDF),
    eg_pdf:set_text_pos(PDF, 72, 190),
    eg_pdf:set_text_leading(PDF,14),
    eg_pdf:set_font(PDF,"Blahonga", 12),
    eg_pdf:textbr(PDF, "The blahonga font will fall back to Times-Roman"),
    eg_pdf:textbr(PDF, "This is a check of ( ) \\ escape chars"),
    eg_pdf:kernedtext(PDF,
                   [ "This is a test of Kerning: A", 120, "W",
                     120, "A", 95, "Y again" ]),
    eg_pdf:break_text(PDF),
    eg_pdf:textbr(PDF, "This is a text without Kerning: AWAY again"),
    eg_pdf:break_text(PDF),
    eg_pdf:end_text(PDF),


    ep_pagination:next_page(PDF),    % ****** Page 3
    
    % ********** Page 3 makeup


    Text3N         = "Fonts",
    Font3N         = "Helvetica",
    TextPosition3N = {72, 72},
    FontSize3N     = 24,
    TextMap3N      = ep_text:create(Font3N, TextPosition3N, FontSize3N), 
    ep_text:one_line(PDF, Text3N, Job, TextMap3N),


    ep_fonts:catalog(PDF, {72, 670}),

    ep_pagination:next_page(PDF),    % ****** Page 4

    % ********** Page 4 makeup


    Text4N         = "Colors",
    Font4N         = "Helvetica",
    TextPosition4N = {72, 72},
    FontSize4N     = 24,
    TextMap4N      = ep_text:create(Font4N, TextPosition4N, FontSize4N), 
    ep_text:one_line(PDF, Text4N, Job, TextMap4N),

    ep_colors:all_colors(PDF),

    ep_pagination:next_page(PDF),    % ****** Page 5

    % ********** Page 5 makeup

    Text5A         = "Images",
    Font5A         = "Helvetica",
    TextPosition5A = {72, 72},
    FontSize5A     = 24,
    TextMap5A      = ep_text:create(Font5A, TextPosition5A, FontSize5A),
    ep_text:one_line(PDF, Text5A, Job, TextMap5A),


    ImageFileName = "freein_pancho.jpg",
    ImagePosition = {100, 400},
    Size          = {width, 200},
    ImageMap      = ep_image:create(ImageFileName, ImagePosition, Size),
    ep_image:image(PDF, Job, ImageMap),

    ImageFileName4B = "GospelOfAshesCover_300dpi.jpg",
    ImagePosition4B = {320, 400},
    Size4B          = {width, 200},
    ImageMap4B      = ep_image:create(ImageFileName4B, ImagePosition4B, Size4B),
    ep_image:image(PDF, Job, ImageMap4B),

    ImageFileName5D = "AyaCover.jpg",
    ImagePosition5D = {100, 720},
    Size5D          = {200, 300},
    ImageMap5D      = ep_image:create(ImageFileName5D, ImagePosition5D, Size5D),
    ep_image:image(PDF, Job, ImageMap5D),

    ImageFileName4C = "joenew.jpg",
    ImagePosition4C = {320, 570},
    Size4C          = {width, 200},
    ImageMap4C      = ep_image:create(ImageFileName4C, ImagePosition4C, Size4C),
    ep_image:image(PDF, Job, ImageMap4C),


    ep_pagination:next_page(PDF),    % ****** Page 6

    % ********** Page 6 makeup 


    Text6A         = "Lines and Shapes",
    Font6A         = "Helvetica",
    TextPosition6A = {72, 72},
    FontSize6A     = 24,
    TextMap6A      = ep_text:create(Font6A, TextPosition6A, FontSize6A), 
    ep_text:one_line(PDF, Text6A, Job, TextMap6A),

%    Text1B      = ep_sample_text:times_14(),


    CropmarkMap = ep_cropmark:create({72, 190}),
    ep_cropmark:cropmark(PDF, Job, CropmarkMap),

    CropmarkMap1 = ep_cropmark:create({72, 230}),
    ep_cropmark:cropmark(PDF, Job, CropmarkMap1),

    CropmarkMap2 = ep_cropmark:create({122, 190}),
    ep_cropmark:cropmark(PDF, Job, CropmarkMap2),

    CropmarkMap3 = ep_cropmark:create({122, 230}),
    ep_cropmark:cropmark(PDF, Job, CropmarkMap3),

    LineMap5 = ep_line:create({100, 100}, {200, 200}),
    ep_line:line(PDF, Job, LineMap5),

    LineMap5A = ep_line:create({110, 100}, {210, 200}),
    LineMap5B = maps:put(color, red, LineMap5A),
    ep_line:line(PDF, Job, LineMap5B),

    LineMap5C = ep_line:create({120, 100}, {220, 200}),
    LineMap5D = maps:put(color, green, LineMap5C),
    ep_line:line(PDF, Job, LineMap5D),

    LineMap5E = ep_line:create({130, 100}, {230, 200}),
    LineMap5F = maps:put(color, blue, LineMap5E),
    ep_line:line(PDF, Job, LineMap5F),

    CircleMap = ep_circle:create({300, 150}, 50),
    ep_circle:circle(PDF, Job, CircleMap),

    EllipseMap = ep_ellipse:create({450, 250}, {100, 50}),
    ep_ellipse:ellipse(PDF, Job, EllipseMap),

    EllipseMap1 = ep_ellipse:create({450, 150}, {100, 50}),
    EllipseMap2 = maps:put(fill_color, yellow, EllipseMap1),
    ep_ellipse:ellipse(PDF, Job, EllipseMap2),

    CircleMap1 = ep_circle:create({450, 250}, 30),
    CircleMap2 = maps:put(fill_color, yellow, CircleMap1),
    CircleMap3 = maps:put(border, 20, CircleMap2),
    CircleMap4 = maps:put(border_style, dash, CircleMap3),
    ep_circle:circle(PDF, Job, CircleMap4),

    Pt1  = {110, 325},
    Pt2  = {260, 300},
    Pt3  = {410, 340},
    Pt4  = {550, 325},
    BezierMap  = ep_bezier:create(Pt1, Pt2, Pt3, Pt4),
    BezierMap1 = maps:put(width, 1, BezierMap),
    BezierMap2 = maps:put(color,red, BezierMap1),
    ep_bezier:bezier(PDF, Job, BezierMap2),

    LineMap5G = ep_line:create({0, 400}, {550, 400}),
    LineMap5H = maps:put(dash, dash, LineMap5G),
    ep_line:line(PDF, Job, LineMap5H),

    Position5A = {72, 370},
    LineHeight5A = 24,

    ep_text:text_lines(PDF, Position5A, LineHeight5A),
    ep_text:next_text_line(PDF, "$", "ZapfDingbats", 20),
    ep_text:end_text_lines(PDF),

    LineList = [ {{200, 450}, {200, 550}}
               , {{220, 450}, {220, 550}}
               , {{240, 450}, {240, 550}}
               , {{260, 450}, {260, 550}}
               ],
    LinesMap = ep_lines:create(LineList),
    ep_lines:lines(PDF, Job, LinesMap),



    LineList1 = [ {{280, 450}, {340, 450}}
                , {{280, 470}, {340, 470}}
                , {{280, 490}, {340, 490}}
                , {{280, 510}, {340, 510}}
                , {{280, 530}, {340, 530}}
                , {{280, 550}, {340, 550}}
                ],
   LinesMap1 = ep_lines:create(LineList1),
   ep_lines:lines(PDF, Job, LinesMap1),


   Vertices6A = [{300, 600}, {250, 650}, {350, 650}],
   PolygonMap6A    = ep_poly:create(Vertices6A),
   ep_poly:polygon(PDF, Job, PolygonMap6A), 


   Vertices6B = [{200, 600}, {150, 650}, {250, 650}],
   PolygonMap6B    = ep_poly:create(Vertices6B),
   PolygonMap6C    = maps:put(fill_color, green, PolygonMap6B),
   ep_poly:polygon(PDF, Job, PolygonMap6C), 


    ep_pagination:next_page(PDF),    % ****** Page 7

    % ********** Page 7 makeup 

    Text7A         = "Dots, Boxes, and Grids",
    Font7A         = "Helvetica",
    TextPosition7A = {72, 72},
    FontSize7A     = 24,
    TextMap7A      = ep_text:create(Font7A, TextPosition7A, FontSize7A), 
    ep_text:one_line(PDF, Text7A, Job, TextMap7A),

    DotCenter = {72, 122},
    DotMap    = ep_dot:create(DotCenter),
    ep_dot:dot(PDF, Job, DotMap),

    DotCenter1  = {92, 122},
    DotMap1      = ep_dot:create(DotCenter1),
    DotMap2     = maps:put(color, red, DotMap1),
    DotMap3     = maps:put(border_color, red, DotMap2),
    ep_dot:dot(PDF, Job, DotMap3),

    DotCenter2 = {112, 122},
    DotMap4    = ep_dot:create(DotCenter2),
    DotMap5    = maps:put(color, green, DotMap4),
    ep_dot:dot(PDF, Job, DotMap5),

    DotCenter3  = {132, 122},
    DotMap6     = ep_dot:create(DotCenter3),
    DotMap7     = maps:put(color, blue, DotMap6),
    ep_dot:dot(PDF, Job, DotMap7),

    CheckboxPosition = {72, 170},
    CheckboxMap    = ep_checkbox:create(CheckboxPosition),
    ep_checkbox:checkbox(PDF, Job, CheckboxMap),

    CheckedboxPosition = {72, 190},
    CheckedboxMap    = ep_checkbox:create(CheckedboxPosition),
    ep_checkbox:checked_box(PDF, Job, CheckedboxMap),


   XList = [72, 92, 112, 132],
   YList = [220, 240, 260, 280],
   GridMap = ep_grid:create(XList, YList),
   ep_grid:grid(PDF, Job, GridMap),

   BoxPosition = {200, 280},
   BoxSize     = {60, 40},
   BoxMap      = ep_rectangle:create(BoxPosition, BoxSize),
   ep_rectangle:rectangle(PDF, Job, BoxMap),

   BoxPosition1 = {300, 280},
   BoxSize1     = {60, 40},
   BoxMap1      = ep_rectangle:create(BoxPosition1, BoxSize1),
   BoxMap2      = maps:put(outline, 4, BoxMap1),
   BoxMap3      = maps:put(outline_color, blue, BoxMap2),
   BoxMap4      = maps:put(fill_color, red, BoxMap3),
   ep_rectangle:rectangle(PDF, Job, BoxMap4),

   BoxPosition2 = {100, 350},
   BoxSize1     = {60, 40},
   Radius       = 10,
   BoxMap5      = ep_round_rect:create(BoxPosition2, BoxSize1, Radius),
   ep_round_rect:round_rect(PDF, Job, BoxMap5),

   BoxPosition6A = {200, 350},
   BoxSize6A     = {60, 40},
   Radius6A      = 10,
   BoxMap6A      = ep_round_rect:create(BoxPosition6A, BoxSize6A, Radius6A),
   BoxMap6AA     = maps:put(outline, 7, BoxMap6A),
   BoxMap6AB     = maps:put(outline_color, red, BoxMap6AA),
   BoxMap6AC     = maps:put(fill_color, blue, BoxMap6AB),
   ep_round_rect:round_rect(PDF, Job, BoxMap6AC),


   ep_pagination:next_page(PDF),    % ****** Page 8

    % ********** Page 8 makeup 

   ep_show_grid:show_grid(PDF, letter),


    Text8         = "Layout Grid",
    Font8         = "Helvetica",
    TextPosition8 = {72, 72},
    FontSize8     = 24,
    TextMap8      = ep_text:create(Font8, TextPosition8, FontSize8), 
    ep_text:one_line(PDF, Text8, Job, TextMap8),


    Text8C         = "Paper Stock: letter",
    Font8C         = "Helvetica",
    TextPosition8C = {72, 102},
    FontSize8C     = 18,
    TextMap8C      = ep_text:create(Font8C, TextPosition8C, FontSize8C), 
    ep_text:one_line(PDF, Text8C, Job, TextMap8C),

    Text8A         = "Th-th-th-that's all folks!",
    Font8A         = "Helvetica",
    TextPosition8A = {450, 550},
    FontSize8A     = 36,
    TextMap8A      = ep_text:create(Font8A, TextPosition8A, FontSize8A), 
    TextMap8B      = maps:put(rot, 90, TextMap8A),
    ep_text:one_line(PDF, Text8A, Job, TextMap8B),



ep_job:save_job(PDF, OutFile).

