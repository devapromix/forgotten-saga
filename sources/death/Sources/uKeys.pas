unit uKeys;

interface

uses Classes;

procedure Keys(var Key: Word); overload;
procedure Keys(var Key: Char); overload;
procedure Keys(var Key: Word; Shift: TShiftState); overload;

implementation

uses uConst, uMenu, uName, uRace, uClass, uHelp, uSplash, uGame, uInv,
  uChar, uMsg, uCellItems, uItem, uTypes, uKills, uRecords, uBook, uEnd,
  uHelpAbout, uHelpKeys, uCraft, uJournal, uConfig, uDialog;

procedure Keys(var Key: Word);
begin
  // Разное управление в завис. от текущего фрейма
  case GameFrame of
    // Заставка
    gfSplash   : KeysSplash(Key);
    // Меню
    gfMenu     : KeysMenu(Key);
    // Ввод имени
    gfName     : KeysName(Key);
    // Выбр. расу.
    gfRace     : KeysRace(Key);
    // Выбр. класс
    gfClass    : KeysClass(Key);
    // Руководство
    gfHelp     : KeysHelp(Key);
    gfHelpAbout: KeysHelpAbout(Key);
    gfHelpKeys : KeysHelpKeys(Key);
    // Предмет
    gfItem     : KeysItem(Key);
//    // Деревня
//    gfVillage  : KeysVillage(Key);
    // Игра
    gfGame     : KeysGame(Key);
    // Книга
    gfBook     : KeysBook(Key);
    // Персонаж
    gfChar     : KeysChar(Key);
    // Инвентарь
    gfInv      : KeysInv(Key);
    // Сообщения
    gfMsg      : KeysMsg(Key);
    // Объекты на полу
    gfCellItems: KeysCellItems(Key);
    // Поражение
    gfDefeat   : KeysEnd(Key);
    // Победа
    gfVictory  : KeysEnd(Key);
    // Алхимия
    gfAlchemy  : KeysCraft(Key);
    // Крафт
    gfCraft    : KeysCraft(Key);
    // Убитые монстры
    gfKills    : KeysKills(Key);
    // Таблица рекордов
    gfRecords  : KeysRecords(Key);
    // Настройки
    gfConfig   : KeysConfig(Key);
    // Диалог с NPC
    gfDialog   : KeysDialog(Key);
    // Журнал квестов
    gfJournal  : KeysJournal(Key);
  end;
end;

procedure Keys(var Key: Char);
begin
  // Разное управление в завис. от текущего фрейма
  case GameFrame of
    // Ввод имени
    gfName     : KeysName(Key);
    // Игра
    gfGame     : KeysGame(Key);
    // Меню
    gfMenu     : KeysMenu(Key);
  end;
end;

procedure Keys(var Key: Word; Shift: TShiftState);
begin
  // Разное управление в завис. от текущего фрейма
  case GameFrame of
    // Инвентарь
    gfInv      : KeysInv(Key, Shift);
  end;
end;

end.
