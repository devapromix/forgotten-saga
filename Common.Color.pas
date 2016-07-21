unit Common.Color;

interface

uses Graphics;

// Цвета
const
{0}cBlack   = $00000000; {0}cWhite    = $00ffffff; {0}cWhiteYel = $008efbfb; {0}cWhiteGre = $0088ff88;
{1}cGray    = $00808080; {1}cLtGray   = $00c0c0c0; {1}cDkGray   = $00303030; {1}cRdGray   = $00706060;
{2}cBlue    = $00880000; {2}cLtBlue   = $00ffaaaa; {2}cDkBlue   = $00550000; {2}cRdBlue   = $00ff8200; //$00F4B65B
{3}cRed     = $00000080; {3}cLtRed    = $000000ff; {3}cDkRed    = $00000040; {3}cRdRed    = $004040ff;
{4}cPurple  = $00800080; {4}cLtPurple = $00ff00ff; {4}cDkPurple = $00440044; {4}cRdPurple = $00d7ebfa;
{5}cGreen   = $00008000; {5}cLtGreen  = $0000ff00; {5}cDkGreen  = $00003300; {5}cRdGreen  = $00187d42;
{6}cYellow  = $00008080; {6}cLtYellow = $0000ffff; {6}cDkYellow = $005ab6ce; {6}cRdYellow = $0063c3c6; //$00aaffff
{7}cBrown   = $0000346b; {7}cLtBrown  = $00006699; {7}cDkBrown  = $00112947; {7}cRdBrown  = $00cdd9dd;
{8}cSkyBlue = $00eed9ba; {8}cLtNavy   = $00d38f4b; {8}cDkNavy   = $00330000; {8}
{9}                      {9}cLtTeal   = $0072AEA5; {9}cDkTeal   = $004d622d; {9}cRdTeal   = $00243226;

  clClear     = -1;        // Прозрачный цвет

  clTitle     = clYellow;  // Цвет заголовка
  clButton    = clGreen;   // Цвет кнопки
  clHotKey    = clSkyBlue; // Цвет активной кнопки
  clSplText   = clSilver;  // Цвет простого текста
  clMenuAct   = 50650;     // Цвет акт. пункта меню
  clNotification = cLtYellow;
  clMenuDef   = $00567390; // Цвет деф. пункта меню
  clAlertText = clRed;     // Цвет сигн. сообщ.
  clEPMText   = clMaroon;  //
  clGoldText  = $0068c9c8; //
  clTileEnt   = cLtBrown;  // Вход и выход из подземелья

  clDirt      = $00234567; // Грязь
  clStone     = $00234123; // Камень #1
  clStone2    = $00444444; // Камень #2
  clStone3    = $00343044; // Камень #3
  clStone4    = $00233228; // Камень #4
  clGrass     = $00005500; // Трава
  clPlant     = $00006633; // Растение #1
  clLtPlant   = $00007800; // Растение #2
  clDkPlant   = clGreen;   // Растение #3

  clDkGray    = clGray;    //

  clCreature  = $00FFFF00;
  clItem      = $000FFFF0;

implementation

end.
