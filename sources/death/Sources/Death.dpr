program Death;

uses
  Forms,
  uMain in 'uMain.pas' {fMain},
  uResFont in 'uResFont.pas',
  uInit in 'uInit.pas',
  uConst in 'uConst.pas',
  uGraph in 'uGraph.pas',
  uDraw in 'uDraw.pas',
  uKeys in 'uKeys.pas',
  uMenu in 'uMenu.pas',
  uColor in 'uColor.pas',
  uName in 'uName.pas',
  uPC in 'uPC.pas',
  uRace in 'uRace.pas',
  uClass in 'uClass.pas',
  uHelp in 'uHelp.pas',
  uSplash in 'uSplash.pas',
  uGame in 'uGame.pas',
  uInv in 'uInv.pas',
  uChar in 'uChar.pas',
  uMsg in 'uMsg.pas',
  uBox in 'uBox.pas',
  uDungeon in 'uDungeon.pas',
  uUtils in 'uUtils.pas',
  uLang in 'uLang.pas',
  uKillList in 'uKillList.pas',
  uDropList in 'uDropList.pas',
  uStringUtils in 'uStringUtils.pas',
  uCellItems in 'uCellItems.pas',
  uItem in 'uItem.pas',
  uBattle in 'uBattle.pas',
  uTypes in 'uTypes.pas',
  uSkills in 'uSkills.pas',
  uKills in 'uKills.pas',
  uEnd in 'uEnd.pas',
  uXML in 'uXML.pas',
  uRecords in 'uRecords.pas',
  uScores in 'uScores.pas',
  uBook in 'uBook.pas',
  uSpells in 'uSpells.pas',
  uScreenshot in 'uScreenshot.pas',
  uCreature in 'uCreature.pas',
  uNPC in 'uNPC.pas',
  uHelpAbout in 'uHelpAbout.pas',
  uHelpKeys in 'uHelpKeys.pas',
  uCraft in 'uCraft.pas',
  uMisc in 'uMisc.pas',
  uMagic in 'uMagic.pas',
  uJournal in 'uJournal.pas',
  uConfig in 'uConfig.pas',
  uQuest in 'uQuest.pas',
  uDialog in 'uDialog.pas',
  uEnemy in 'uEnemy.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Death';
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
