%%==========================================================================
%% Copyright (C) 2005 Johan Bevemyr
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%
%% Author: Johan Bevemyr <jb@son.bevemyr.com>
%%==========================================================================

-module(eg_convert).

-export([mac2pdf/1,pdf2mac/1]).

mac2pdf(List) when is_list(List) ->
    [mac2pdf(X) || X <- List];
mac2pdf(8#101)->8#101;
mac2pdf(8#256)->8#306;
mac2pdf(8#347)->8#301;
mac2pdf(8#345)->8#302;
mac2pdf(8#200)->8#304;
mac2pdf(8#313)->8#300;
mac2pdf(8#201)->8#305;
mac2pdf(8#314)->8#303;
mac2pdf(8#102)->8#102;
mac2pdf(8#103)->8#103;
mac2pdf(8#202)->8#307;
mac2pdf(8#104)->8#104;
mac2pdf(8#105)->8#105;
mac2pdf(8#203)->8#311;
mac2pdf(8#346)->8#312;
mac2pdf(8#350)->8#313;
mac2pdf(8#351)->8#310;
mac2pdf(8#106)->8#106;
mac2pdf(8#107)->8#107;
mac2pdf(8#110)->8#110;
mac2pdf(8#111)->8#111;
mac2pdf(8#352)->8#315;
mac2pdf(8#353)->8#316;
mac2pdf(8#354)->8#317;
mac2pdf(8#355)->8#314;
mac2pdf(8#112)->8#112;
mac2pdf(8#113)->8#113;
mac2pdf(8#114)->8#114;
mac2pdf(8#115)->8#115;
mac2pdf(8#116)->8#116;
mac2pdf(8#204)->8#321;
mac2pdf(8#117)->8#117;
mac2pdf(8#316)->8#226;
mac2pdf(8#356)->8#323;
mac2pdf(8#357)->8#324;
mac2pdf(8#205)->8#326;
mac2pdf(8#361)->8#322;
mac2pdf(8#257)->8#330;
mac2pdf(8#315)->8#325;
mac2pdf(8#120)->8#120;
mac2pdf(8#121)->8#121;
mac2pdf(8#122)->8#122;
mac2pdf(8#123)->8#123;
mac2pdf(8#124)->8#124;
mac2pdf(8#125)->8#125;
mac2pdf(8#362)->8#332;
mac2pdf(8#363)->8#333;
mac2pdf(8#206)->8#334;
mac2pdf(8#364)->8#331;
mac2pdf(8#126)->8#126;
mac2pdf(8#127)->8#127;
mac2pdf(8#130)->8#130;
mac2pdf(8#131)->8#131;
mac2pdf(8#331)->8#230;
mac2pdf(8#132)->8#132;
mac2pdf(8#141)->8#141;
mac2pdf(8#207)->8#341;
mac2pdf(8#211)->8#342;
mac2pdf(8#253)->8#264;
mac2pdf(8#212)->8#344;
mac2pdf(8#276)->8#346;
mac2pdf(8#210)->8#340;
mac2pdf(8#046)->8#046;
mac2pdf(8#214)->8#345;
mac2pdf(8#136)->8#136;
mac2pdf(8#176)->8#176;
mac2pdf(8#052)->8#052;
mac2pdf(8#100)->8#100;
mac2pdf(8#213)->8#343;
mac2pdf(8#142)->8#142;
mac2pdf(8#134)->8#134;
mac2pdf(8#174)->8#174;
mac2pdf(8#173)->8#173;
mac2pdf(8#175)->8#175;
mac2pdf(8#133)->8#133;
mac2pdf(8#135)->8#135;
mac2pdf(8#371)->8#030;
mac2pdf(8#245)->8#200;
mac2pdf(8#143)->8#143;
mac2pdf(8#377)->8#031;
mac2pdf(8#215)->8#347;
mac2pdf(8#374)->8#270;
mac2pdf(8#242)->8#242;
mac2pdf(8#366)->8#032;
mac2pdf(8#072)->8#072;
mac2pdf(8#054)->8#054;
mac2pdf(8#251)->8#251;
mac2pdf(8#333)->8#244;
mac2pdf(8#144)->8#144;
mac2pdf(8#240)->8#201;
mac2pdf(8#340)->8#202;
mac2pdf(8#241)->8#260;
mac2pdf(8#254)->8#250;
mac2pdf(8#326)->8#367;
mac2pdf(8#044)->8#044;
mac2pdf(8#372)->8#033;
mac2pdf(8#365)->8#232;
mac2pdf(8#145)->8#145;
mac2pdf(8#216)->8#351;
mac2pdf(8#220)->8#352;
mac2pdf(8#221)->8#353;
mac2pdf(8#217)->8#350;
mac2pdf(8#070)->8#070;
mac2pdf(8#311)->8#203;
mac2pdf(8#321)->8#204;
mac2pdf(8#320)->8#205;
mac2pdf(8#075)->8#075;
mac2pdf(8#041)->8#041;
mac2pdf(8#301)->8#241;
mac2pdf(8#146)->8#146;
mac2pdf(8#336)->8#223;
mac2pdf(8#065)->8#065;
mac2pdf(8#337)->8#224;
mac2pdf(8#304)->8#206;
mac2pdf(8#064)->8#064;
mac2pdf(8#332)->8#207;
mac2pdf(8#147)->8#147;
mac2pdf(8#247)->8#337;
mac2pdf(8#140)->8#140;
mac2pdf(8#076)->8#076;
mac2pdf(8#307)->8#253;
mac2pdf(8#310)->8#273;
mac2pdf(8#334)->8#210;
mac2pdf(8#335)->8#211;
mac2pdf(8#150)->8#150;
mac2pdf(8#375)->8#034;
mac2pdf(8#055)->8#055;
mac2pdf(8#151)->8#151;
mac2pdf(8#222)->8#355;
mac2pdf(8#224)->8#356;
mac2pdf(8#225)->8#357;
mac2pdf(8#223)->8#354;
mac2pdf(8#152)->8#152;
mac2pdf(8#153)->8#153;
mac2pdf(8#154)->8#154;
mac2pdf(8#074)->8#074;
mac2pdf(8#302)->8#254;
mac2pdf(8#155)->8#155;
mac2pdf(8#370)->8#257;
mac2pdf(8#265)->8#265;
mac2pdf(8#156)->8#156;
mac2pdf(8#071)->8#071;
mac2pdf(8#226)->8#361;
mac2pdf(8#043)->8#043;
mac2pdf(8#157)->8#157;
mac2pdf(8#227)->8#363;
mac2pdf(8#231)->8#364;
mac2pdf(8#232)->8#366;
mac2pdf(8#317)->8#234;
mac2pdf(8#376)->8#035;
mac2pdf(8#230)->8#362;
mac2pdf(8#061)->8#061;
mac2pdf(8#273)->8#252;
mac2pdf(8#274)->8#272;
mac2pdf(8#277)->8#370;
mac2pdf(8#233)->8#365;
mac2pdf(8#160)->8#160;
mac2pdf(8#246)->8#266;
mac2pdf(8#050)->8#050;
mac2pdf(8#051)->8#051;
mac2pdf(8#045)->8#045;
mac2pdf(8#056)->8#056;
mac2pdf(8#341)->8#267;
mac2pdf(8#344)->8#213;
mac2pdf(8#053)->8#053;
mac2pdf(8#261)->8#261;
mac2pdf(8#161)->8#161;
mac2pdf(8#077)->8#077;
mac2pdf(8#300)->8#277;
mac2pdf(8#042)->8#042;
mac2pdf(8#343)->8#214;
mac2pdf(8#322)->8#215;
mac2pdf(8#323)->8#216;
mac2pdf(8#324)->8#217;
mac2pdf(8#325)->8#220;
mac2pdf(8#342)->8#221;
mac2pdf(8#047)->8#047;
mac2pdf(8#162)->8#162;
mac2pdf(8#250)->8#256;
mac2pdf(8#373)->8#036;
mac2pdf(8#163)->8#163;
mac2pdf(8#244)->8#247;
mac2pdf(8#073)->8#073;
mac2pdf(8#067)->8#067;
mac2pdf(8#066)->8#066;
mac2pdf(8#057)->8#057;
mac2pdf(8#040)->8#040;
mac2pdf(8#243)->8#243;
mac2pdf(8#164)->8#164;
mac2pdf(8#063)->8#063;
mac2pdf(8#367)->8#037;
mac2pdf(8#252)->8#222;
mac2pdf(8#062)->8#062;
mac2pdf(8#165)->8#165;
mac2pdf(8#234)->8#372;
mac2pdf(8#236)->8#373;
mac2pdf(8#237)->8#374;
mac2pdf(8#235)->8#371;
mac2pdf(8#137)->8#137;
mac2pdf(8#166)->8#166;
mac2pdf(8#167)->8#167;
mac2pdf(8#170)->8#170;
mac2pdf(8#171)->8#171;
mac2pdf(8#330)->8#377;
mac2pdf(8#264)->8#245;
mac2pdf(8#172)->8#172;
mac2pdf(8#060)->8#060;
mac2pdf(X)    ->X.

pdf2mac(List) when is_list(List) ->
    [pdf2mac(X) || X <- List];
pdf2mac(8#101)->8#101;
pdf2mac(8#306)->8#256;
pdf2mac(8#301)->8#347;
pdf2mac(8#302)->8#345;
pdf2mac(8#304)->8#200;
pdf2mac(8#300)->8#313;
pdf2mac(8#305)->8#201;
pdf2mac(8#303)->8#314;
pdf2mac(8#102)->8#102;
pdf2mac(8#103)->8#103;
pdf2mac(8#307)->8#202;
pdf2mac(8#104)->8#104;
pdf2mac(8#105)->8#105;
pdf2mac(8#311)->8#203;
pdf2mac(8#312)->8#346;
pdf2mac(8#313)->8#350;
pdf2mac(8#310)->8#351;
pdf2mac(8#106)->8#106;
pdf2mac(8#107)->8#107;
pdf2mac(8#110)->8#110;
pdf2mac(8#111)->8#111;
pdf2mac(8#315)->8#352;
pdf2mac(8#316)->8#353;
pdf2mac(8#317)->8#354;
pdf2mac(8#314)->8#355;
pdf2mac(8#112)->8#112;
pdf2mac(8#113)->8#113;
pdf2mac(8#114)->8#114;
pdf2mac(8#115)->8#115;
pdf2mac(8#116)->8#116;
pdf2mac(8#321)->8#204;
pdf2mac(8#117)->8#117;
pdf2mac(8#226)->8#316;
pdf2mac(8#323)->8#356;
pdf2mac(8#324)->8#357;
pdf2mac(8#326)->8#205;
pdf2mac(8#322)->8#361;
pdf2mac(8#330)->8#257;
pdf2mac(8#325)->8#315;
pdf2mac(8#120)->8#120;
pdf2mac(8#121)->8#121;
pdf2mac(8#122)->8#122;
pdf2mac(8#123)->8#123;
pdf2mac(8#124)->8#124;
pdf2mac(8#125)->8#125;
pdf2mac(8#332)->8#362;
pdf2mac(8#333)->8#363;
pdf2mac(8#334)->8#206;
pdf2mac(8#331)->8#364;
pdf2mac(8#126)->8#126;
pdf2mac(8#127)->8#127;
pdf2mac(8#130)->8#130;
pdf2mac(8#131)->8#131;
pdf2mac(8#230)->8#331;
pdf2mac(8#132)->8#132;
pdf2mac(8#141)->8#141;
pdf2mac(8#341)->8#207;
pdf2mac(8#342)->8#211;
pdf2mac(8#264)->8#253;
pdf2mac(8#344)->8#212;
pdf2mac(8#346)->8#276;
pdf2mac(8#340)->8#210;
pdf2mac(8#046)->8#046;
pdf2mac(8#345)->8#214;
pdf2mac(8#136)->8#136;
pdf2mac(8#176)->8#176;
pdf2mac(8#052)->8#052;
pdf2mac(8#100)->8#100;
pdf2mac(8#343)->8#213;
pdf2mac(8#142)->8#142;
pdf2mac(8#134)->8#134;
pdf2mac(8#174)->8#174;
pdf2mac(8#173)->8#173;
pdf2mac(8#175)->8#175;
pdf2mac(8#133)->8#133;
pdf2mac(8#135)->8#135;
pdf2mac(8#030)->8#371;
pdf2mac(8#200)->8#245;
pdf2mac(8#143)->8#143;
pdf2mac(8#031)->8#377;
pdf2mac(8#347)->8#215;
pdf2mac(8#270)->8#374;
pdf2mac(8#242)->8#242;
pdf2mac(8#032)->8#366;
pdf2mac(8#072)->8#072;
pdf2mac(8#054)->8#054;
pdf2mac(8#251)->8#251;
pdf2mac(8#244)->8#333;
pdf2mac(8#144)->8#144;
pdf2mac(8#201)->8#240;
pdf2mac(8#202)->8#340;
pdf2mac(8#260)->8#241;
pdf2mac(8#250)->8#254;
pdf2mac(8#367)->8#326;
pdf2mac(8#044)->8#044;
pdf2mac(8#033)->8#372;
pdf2mac(8#232)->8#365;
pdf2mac(8#145)->8#145;
pdf2mac(8#351)->8#216;
pdf2mac(8#352)->8#220;
pdf2mac(8#353)->8#221;
pdf2mac(8#350)->8#217;
pdf2mac(8#070)->8#070;
pdf2mac(8#203)->8#311;
pdf2mac(8#204)->8#321;
pdf2mac(8#205)->8#320;
pdf2mac(8#075)->8#075;
pdf2mac(8#041)->8#041;
pdf2mac(8#241)->8#301;
pdf2mac(8#146)->8#146;
pdf2mac(8#223)->8#336;
pdf2mac(8#065)->8#065;
pdf2mac(8#224)->8#337;
pdf2mac(8#206)->8#304;
pdf2mac(8#064)->8#064;
pdf2mac(8#207)->8#332;
pdf2mac(8#147)->8#147;
pdf2mac(8#337)->8#247;
pdf2mac(8#140)->8#140;
pdf2mac(8#076)->8#076;
pdf2mac(8#253)->8#307;
pdf2mac(8#273)->8#310;
pdf2mac(8#210)->8#334;
pdf2mac(8#211)->8#335;
pdf2mac(8#150)->8#150;
pdf2mac(8#034)->8#375;
pdf2mac(8#055)->8#055;
pdf2mac(8#151)->8#151;
pdf2mac(8#355)->8#222;
pdf2mac(8#356)->8#224;
pdf2mac(8#357)->8#225;
pdf2mac(8#354)->8#223;
pdf2mac(8#152)->8#152;
pdf2mac(8#153)->8#153;
pdf2mac(8#154)->8#154;
pdf2mac(8#074)->8#074;
pdf2mac(8#254)->8#302;
pdf2mac(8#155)->8#155;
pdf2mac(8#257)->8#370;
pdf2mac(8#265)->8#265;
pdf2mac(8#156)->8#156;
pdf2mac(8#071)->8#071;
pdf2mac(8#361)->8#226;
pdf2mac(8#043)->8#043;
pdf2mac(8#157)->8#157;
pdf2mac(8#363)->8#227;
pdf2mac(8#364)->8#231;
pdf2mac(8#366)->8#232;
pdf2mac(8#234)->8#317;
pdf2mac(8#035)->8#376;
pdf2mac(8#362)->8#230;
pdf2mac(8#061)->8#061;
pdf2mac(8#252)->8#273;
pdf2mac(8#272)->8#274;
pdf2mac(8#370)->8#277;
pdf2mac(8#365)->8#233;
pdf2mac(8#160)->8#160;
pdf2mac(8#266)->8#246;
pdf2mac(8#050)->8#050;
pdf2mac(8#051)->8#051;
pdf2mac(8#045)->8#045;
pdf2mac(8#056)->8#056;
pdf2mac(8#267)->8#341;
pdf2mac(8#213)->8#344;
pdf2mac(8#053)->8#053;
pdf2mac(8#261)->8#261;
pdf2mac(8#161)->8#161;
pdf2mac(8#077)->8#077;
pdf2mac(8#277)->8#300;
pdf2mac(8#042)->8#042;
pdf2mac(8#214)->8#343;
pdf2mac(8#215)->8#322;
pdf2mac(8#216)->8#323;
pdf2mac(8#217)->8#324;
pdf2mac(8#220)->8#325;
pdf2mac(8#221)->8#342;
pdf2mac(8#047)->8#047;
pdf2mac(8#162)->8#162;
pdf2mac(8#256)->8#250;
pdf2mac(8#036)->8#373;
pdf2mac(8#163)->8#163;
pdf2mac(8#247)->8#244;
pdf2mac(8#073)->8#073;
pdf2mac(8#067)->8#067;
pdf2mac(8#066)->8#066;
pdf2mac(8#057)->8#057;
pdf2mac(8#040)->8#040;
pdf2mac(8#243)->8#243;
pdf2mac(8#164)->8#164;
pdf2mac(8#063)->8#063;
pdf2mac(8#037)->8#367;
pdf2mac(8#222)->8#252;
pdf2mac(8#062)->8#062;
pdf2mac(8#165)->8#165;
pdf2mac(8#372)->8#234;
pdf2mac(8#373)->8#236;
pdf2mac(8#374)->8#237;
pdf2mac(8#371)->8#235;
pdf2mac(8#137)->8#137;
pdf2mac(8#166)->8#166;
pdf2mac(8#167)->8#167;
pdf2mac(8#170)->8#170;
pdf2mac(8#171)->8#171;
pdf2mac(8#377)->8#330;
pdf2mac(8#245)->8#264;
pdf2mac(8#172)->8#172;
pdf2mac(8#060)->8#060;
pdf2mac(8#240)->8#040;
pdf2mac(X)    ->X.

