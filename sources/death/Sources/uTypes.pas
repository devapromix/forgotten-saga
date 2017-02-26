unit uTypes;

interface

type
  // Свойства предмета
  TItemPropertiesResult = record
    Armor, MinDamage, MaxDamage: Integer;
  end;
  //
  TExplodeResult  = array of string;
  //
  Array4 = array [0..3] of Integer;
  //
  TPoint = record
    X, Y: Integer;
  end;
  //
  TSize = record
    Width, Height: Integer;
  end;
  // Инф. о тайле
  TTileInfo = record
    Color : Integer;
    Char  : Char;
  end;
  // Skill
  TSkillItem = record
    Value: Integer;
    Exp: Integer;
  end;
  // Skills
  TSkills = array[1..36] of TSkillItem;
  // Свойства расы
  THeroProp = record
    Str, Dex, Sta, Int: Integer;
    Skills: TSkills;
  end;
  // Фреймы
  TGameFrame = (gfNone, gfSplash, gfMenu, gfName, gfRace, gfClass, gfGame, gfHelp,
    gfChar, gfInv, gfMsg, gfDrink, gfCellItems, gfItem, gfDefeat, gfKills,
    gfRecords, gfBook, gfVictory, gfAlchemy, gfCraft, gfHelpAbout,
    gfHelpKeys, gfJournal, gfConfig, gfDialog);

function SetTileInfo(AChar: Char; AColor: Integer): TTileInfo;
function SetPoint(X, Y: Integer): TPoint;
function SetSize(AWidth, AHeight: Integer): TSize;

implementation

function SetTileInfo(AChar: Char; AColor: Integer): TTileInfo;
begin
  Result.Color := AColor;
  Result.Char := AChar;
end;

function SetPoint(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function SetSize(AWidth, AHeight: Integer): TSize;
begin
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

end.
