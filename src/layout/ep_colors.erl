%%% ==========================================================================
%%%  ep_colors.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:      MIT
%%%   File:         ep_colors.erl
%%%   Description:  Color catalogue
%%%                 colortest/1 is excerpted from:
%%%                 Hugh Hawkins
%%%                 eg1_test.erl
%%%                 https://github.com/hwatkins/erlguten/blob/master/test/eg1_test.erl 
%%% @end

%%% ==========================================================================


-module (ep_colors).

-export ([
    all_colors/1,
    color/1
]).

-include("ep.hrl").
-include("ep_erltypes.hrl").

%% ********************************************************* 
%% Color name to RGB
%% ********************************************************* 

%% @doc Give color name, return RGB value
-spec color(Name :: atom()) -> rgb_color().
color(Name) ->
   eg_pdf_op:color(Name).


%% ********************************************************* 
%% Color Test 
%% ********************************************************* 

all_colors(PDF) ->
    D = 50, S = 630, M = 60,
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S),
    colortest1(PDF, 0, [white, silver, gray, black, maroon, red, fuchsia, purple,
                        lime, green, olive, yellow, navy, blue, teal, aqua]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - D),
    colortest1(PDF, 0, [blue2, blue3, blue4, blueviolet, cornflowerblue, darkorchid,
                        darkslateblue, dodgerblue, lightskyblue, mediumblue,
                        mediumpurple, mediumslateblue, midnightblue, purpleblue,
                        royalblue, skyblue2, slateblue]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 2 * D),
    colortest1(PDF, 0, [aquamarine4, cadetblue, darkturquoise, lightblue,
                        lightseagreen, lightslategray, lightsteelblue,
                        mediumturquoise, paleturquoise, powderblue, skyblue,
                        steelblue, turquoise]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 3 * D),
    colortest1(PDF, 0, [antiquewhite3, antiquewhite4, azure3, beige, darkslategray,
                        gainsboro, honeydew, slategray, thistle
    ]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 4 * D),
    colortest1(PDF, 0, [aquamarine, chartreuse, darkgreen, darkseagreen, forestgreen,
                        green2, green3, green4, greenyellow, lawngreen, limegreen,
                        mediumaquamarine, mediumseagreen, mediumspringgreen,
                        olivedrab, palegreen]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 5 * D),
    colortest1(PDF, 0, [seagreen, springgreen, yellowgreen]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 6 * D),
    colortest1(PDF, 0, [magenta, magenta2, magenta3, magenta4, mediumorchid, orchid,
                        plum, violet]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 7 * D),
    colortest1(PDF, 0, [brown, burlywood, chocolate, coral, darkgoldenrod, darkorange,
                        darksalmon, deeppink, firebrick, gold, goldenrod, hotpink,
                        indianred, lightcoral, lightpink, lightsalmon]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 8 * D),
    colortest1(PDF, 0, [maroon0, orange, orangered, palevioletred, peachpuff, peru,
                        pink, red2, red3, red4, rosybrown, salmon, sandybrown, sienna,
                        tomato, violetred]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 9 * D),
    colortest1(PDF, 0, [aliceblue, azure, floralwhite, ghostwhite, ivory, lavender,
                        lavenderblush, lightcyan, lightyellow, linen, mintcream,
                        mistyrose, oldlace, seashell, snow, whitesmoke]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 10 * D),
    colortest1(PDF, 0, [antiquewhite, bisque, blancedalmond, comsilk, darkkhaki,
                        darkolivegreen, khaki, lemonchiffon, lightgoldenrod,
                        lightgoldenrodyellow]),
    eg_pdf:restore_state(PDF),
    eg_pdf:save_state(PDF),
    eg_pdf:translate(PDF, M, S - 11 * D),
    colortest1(PDF, 0, [moccasin, palegoldenrod, papayawhip,
                        tan, wheat, yellow2, yellow3, yellow4]),

    eg_pdf:restore_state(PDF).


colortest1(_PDF, _N, []) ->
    [];

colortest1(PDF, N, [H | T]) ->
    eg_pdf:set_fill_color(PDF, H),
    eg_pdf:rectangle(PDF, {0, 20}, {20, 20}),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:set_fill_color(PDF, black),
    eg_pdf:begin_text(PDF),
    eg_pdf:set_font(PDF, "Times-Roman", 8),
    eg_pdf:set_text_pos(PDF, 0, (N rem 2) * 10),
    eg_pdf:text(PDF, atom_to_list(H)),
    eg_pdf:end_text(PDF),
    eg_pdf:translate(PDF, 30, 0),
    colortest1(PDF, N + 1, T).
