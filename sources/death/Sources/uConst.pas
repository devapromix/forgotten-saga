unit uConst;

interface

uses uTypes;

const
  // Версия
  DeathVersion = '0.2.0';
  // Символ курсора
  CursorChar = '_';
  // Мод. шкалы опыта
  ExpMod = 45;
  // Кол. предметов
  ItemsCount = 255;
  //
  DungeonHeight = 39;
  DungeonWidth = 69;
  DungeonMin = -1;
  DungeonsCount = 25;
  DungeonGold = 10;
  DungeonCreatures = 10;
  //
  RacesCount = 12;
  ClassesCount = 12;

var
  // Отл. режим
  DebugMode: Boolean = False;
  // Фрейм
  GameFrameTemp: TGameFrame = gfNone;
  GameFrameScreen: TGameFrame = gfNone;
  GameFrame: TGameFrame = gfSplash;
  // Флаг начала игры
  IsGame: Boolean = False;
  // Поз. пунк. меню
  MenuPos: Byte = 0;
  LLPos: Byte = 0;
  // Символ
  CharWidth: Integer = 0;
  CharHeight: Integer = 0;
  HalfCharWidth: Integer = 0;
  HalfCharHeight: Integer = 0;

const
  // Экран
  ScreenWidth = 110;
  ScreenHeight = 40;
  // Шир. колонки
  ColWidth = ScreenWidth div 4;

var
  //
  Cursor: TPoint;
  // Имя
  PCName: string = '';
  LastEnemyName: string = '';
  // Миг. курсора
  ShowCursor: Boolean = False;
  // Выбр. предмет в инв.
  ItemSelInvID: Integer = 0;
  ItemSelInvCount: Integer = 0;
  // Look, shoot, spell, talk
  LMode: Boolean = False;
  SMode: Boolean = False;
  TMode: Boolean = False;
  // Animations
  AnimPC: Boolean = False;
  AnimEnemyChar: Char;
  AnimEnemy: Boolean = False;
  AnimEnemyPoint: TPoint;
  IsVictory: Boolean = False;
  NoSpell: Boolean = False;
  ShowCharAllSkills: Byte = 1;
  PCPanelWidth: Integer = 40;
  TrapPit: Boolean = False;
  AutoPickUpGold: Boolean = False;

const
  Zip2ArchivePassword = 'supernatural';

implementation

end.
