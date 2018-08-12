%%% ==========================================================================
%%% ep_fonts.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_fonts.erl
%%%   Description:  Font catalong 
%%% @end

%%% ==========================================================================



-module (ep_fonts).

-export ([catalog/2]).


%% *********************************************************************
%% Font_catalog
%% *********************************************************************


catalog(PDF, Position) ->

    LineHeight = 24, 

    ep_text:text_lines(PDF, Position, LineHeight),

    ep_text:next_text_line(PDF, "Times-Roman 18 pt", "Times-Roman", 18),
    ep_text:next_text_line(PDF, "Times-Italic 18 pt", "Times-Italic", 18),
    ep_text:next_text_line(PDF, "Times-Bold 18 pt", "Times-Bold", 18),
    ep_text:next_text_line(PDF, "Times-BoldItalic 18 pt", "Times-BoldItalic", 18),

    eg_pdf:textbr(PDF, " "),

    ep_text:next_text_line(PDF, "Courier 18 pt", "Courier", 18),
    ep_text:next_text_line(PDF, "Courier-Oblique 18 pt", "Courier-Oblique", 18),
    ep_text:next_text_line(PDF, "Courier-Bold 18 pt", "Courier-Bold", 18),
    ep_text:next_text_line(PDF, "Courier-BoldOblique 18 pt", "Courier-BoldOblique", 18),

    eg_pdf:textbr(PDF, " "),

    ep_text:next_text_line(PDF, "Helvetica 18 pt", "Helvetica", 18),
    ep_text:next_text_line(PDF, "Helvetica-Oblique 18 pt", "Helvetica-Oblique", 18),
    ep_text:next_text_line(PDF, "Helvetica-Bold 18 pt", "Helvetica-Bold", 18),
    ep_text:next_text_line(PDF, "Helvetica-BoldOblique 18 pt", "Helvetica-BoldOblique", 18),

%    ep_text:next_text_line(PDF, "Victorias-Secret 24pt", "Victorias-Secret", 24), 
%    NOTE: Be fun to know why this breaks

    eg_pdf:textbr(PDF, " "),

    ep_text:next_text_line(PDF, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "Symbol", 18),
    ep_text:next_text_line(PDF, "abcdefghijklmnopqrstuvwxyz", "Symbol", 18),
    ep_text:next_text_line(PDF, "0123456789", "Symbol", 18),
    ep_text:next_text_line(PDF, "`~!@#$%^&*()-_=+[{]}\|;:',<.>/?", "Symbol", 18),
    

    eg_pdf:textbr(PDF, " "),

    ep_text:next_text_line(PDF, "ABCDEFGHIJKLMNOPQRSTUVWXYX", "ZapfDingbats", 18),
    ep_text:next_text_line(PDF, "abcdefghijklmnopqrstuvwxyz", "ZapfDingbats", 18),
    ep_text:next_text_line(PDF, "0123456789", "ZapfDingbats", 18),
    ep_text:next_text_line(PDF, "`~!@#$%^&*()-_=+[{]}\|;:',<.>/?", "ZapfDingbats", 18),

    ep_text:end_text_lines(PDF).


%    eg_pdf:set_font(PDF, "OCR-A-Digits", 10),
%    eg_pdf_lib:moveAndShow(PDF, 280, 15, "09588   234102 01 7         0       013135").

%    ep_text:next_text_line(PDF, "1 2 3 4", "OCR-B-Digits", 24),
