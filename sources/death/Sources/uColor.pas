unit uColor;

interface

const
{0}cBlack   = $00000000; {0}cWhite    = $00ffffff; {0}cWhiteYel = $008efbfb; {0}cWhiteGre = $0088ff88;
{1}cGray    = $00808080; {1}cLtGray   = $00c0c0c0; {1}cDkGray   = $00303030; {1}cRdGray   = $00706060;
{2}cBlue    = $00880000; {2}cLtBlue   = $00ffaaaa; {2}cDkBlue   = $00550000; {2}cRdBlue   = $00ff8200; //$00F4B65B
{3}cRed     = $00000080; {3}cLtRed    = $000000ff; {3}cDkRed    = $00000040; {3}cRdRed    = $004040ff;
{4}cPurple  = $00800080; {4}cLtPurple = $00ff00ff; {4}cDkPurple = $00440044; {4}cRdPurple = $00d7ebfa;
{5}cGreen   = $00008000; {5}cLtGreen  = $0000ff00; {5}cDkGreen  = $00003300; {5}cRdGreen  = $00187d42;
{6}cYellow  = $00008080; {6}cLtYellow = $0000ffff; {6}cDkYellow = $005ab6ce; {6}cRdYellow = $0063c3c6;
{7}cBrown   = $0000346b; {7}cLtBrown  = $00006699; {7}cDkBrown  = $00112947; {7}cRdBrown  = $00cdd9dd;
{8}cSkyBlue = $00eed9ba; {8}cLtNavy   = $00d38f4b; {8}cDkNavy   = $00330000; {8}
{9}                      {9}cLtTeal   = $0072AEA5; {9}cDkTeal   = $004d622d; {9}cRdTeal   = $00243226;

function DarkColor(Color: Integer; Percent: Byte): Integer;
function LightColor(Color: Integer; Percent: Byte): Integer;

implementation

uses Windows, SysUtils, Graphics;

function DarkColor(Color: Integer; Percent: Byte): Integer;
var
  R, G, B: Byte;
  C: Integer;
begin
  C := ColorToRGB(Color);
  R := GetRValue(C);
  G := GetGValue(C);
  B := GetBValue(C);
  R := R - MulDiv(R, Percent, 100);
  G := G - MulDiv(G, Percent, 100);
  B := B - MulDiv(B, Percent, 100);
  Result := RGB(R, G, B);
end;

function LightColor(Color: Integer; Percent: Byte): Integer;
var
  R, G, B: Byte;
  C: Integer;
begin
  C := ColorToRGB(Color);
  R := GetRValue(C);
  G := GetGValue(C);
  B := GetBValue(C);
	R := R + MulDiv(255 - R, Percent, 100);
	G := G + MulDiv(255 - G, Percent, 100);
	B := B + MulDiv(255 - B, Percent, 100);
  Result := RGB(R, G, B);
end;

end.
