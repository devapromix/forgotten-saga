unit uDraw;

interface

procedure Draw();
procedure Refresh();

implementation

uses Types, uMenu, uGraph, uColor, uMain, uConst, uName, uClass, uRace,
  uHelp, uSplash, uGame, uInv, uChar, uMsg, uCellItems, uItem, uCraft,
  uTypes, uKills, uRecords, uBook, uEnd, uHelpAbout, uHelpKeys,
  uJournal, uConfig, uDialog;

procedure Draw();
begin
  // Подготавливаем канву: заливаем черным цветом...
  BG.Canvas.Brush.Color := cBlack;
  BG.Canvas.FillRect(Rect(0, 0, fMain.ClientRect.Right, fMain.ClientRect.Bottom));
  // Отображаем картинку в зависимости от текущего фрейма
  case GameFrame of
    // Заставка
    gfSplash   : DrawSplash();
    // Меню
    gfMenu     : DrawMenu();
    // Ввести имя
    gfName     : DrawName();
    // Выбр. расу
    gfRace     : DrawRace();
    // Выбр. класс
    gfClass    : DrawClass();
    // Руководство
    gfHelp     : DrawHelp();
    gfHelpAbout: DrawHelpAbout();
    gfHelpKeys : DrawHelpKeys();
    // Предмет
    gfItem     : DrawItem();
//    // Деревня
//    gfVillage  : DrawVillage();
    // Игра
    gfGame     : DrawGame();
    // Книга
    gfBook     : DrawBook();
    // Персонаж
    gfChar     : DrawChar();
    // Инвентарь
    gfInv      : DrawInv();
    // Сообщения
    gfMsg      : DrawMsg(False, 0, ScreenHeight - 4);
    // Объекты на полу
    gfCellItems: DrawCellItems();
    // О персонаже после поражения
    gfDefeat   : DrawEnd();
    // Победа
    gfVictory  : DrawEnd();
    // Алхимия
    gfAlchemy  : DrawCraft();
    // Крафт
    gfCraft    : DrawCraft();
    // Список побежденных врагов
    gfKills    : DrawKills();
    // Таблица рекордов
    gfRecords  : DrawRecords();
    // Настройки
    gfConfig   : DrawConfig();
    // Диалог
    gfDialog   : DrawDialog();
    // Журнал квестов
    gfJournal  : DrawJournal();
  end;
  // Отображаем буфер
  fMain.Canvas.Draw(HalfCharWidth, HalfCharHeight, BG);
end;

procedure Refresh();
begin
  fMain.Canvas.Draw(HalfCharWidth, HalfCharHeight, BG);
end;

end.
