unit uInit;

interface

procedure Init();

implementation

uses Graphics, Forms, SysUtils, uMain, uScores, uGraph, uConst, uResFont, uLang,
  uXML, uBox;

procedure Init();
var
  RF: TResFont;
  A: TXML;
  S: string;
  I: Integer;

  procedure CreateSaveArchive();
  begin
    //Zip[2].Password := Zip2ArchivePassword;
    //Zip[2].OpenArchive;
    //Zip[2].AddFromString('scores', '');
    //Zip[2].CloseArchive;
  end;

begin
  Randomize;
  for I := 1 to ParamCount do
  begin
    if (LowerCase(ParamStr(I)) = '-d') then DebugMode := True;
    if (LowerCase(ParamStr(I)) = '-c') then CreateSaveArchive();
  end;
  //
  //if not FileExists(Zip[1].FileName) then ErrorInZip(Zip[1].FileName);
  //if not FileExists(Zip[2].FileName) then CreateSaveArchive();
  Scores.Load;
  //
  try
    A := TXML.Create('Death.Config');
    //
{    if DebugMode then with A do begin
      Write('fontfile', 'CourBD.ttf');
      Write('fontname', 'Courier New');
      Write('fontsize', '11');
      Write('language', 'russian');
    end;                        }
    //

    RF := TResFont.Create;
    try
      S := A.Read('fontfile');
      if (S <> '') then RF.LoadFromFile(S);
      with BG.Canvas.Font do
      begin
        S := A.Read('fontsize');
        if (S = '') then S := '11';
        I := StrToInt(S);
        if (I < 9) then I := 9;
        Size := I;
        S := A.Read('fontname');
        if (S = '') then Name := RF.FontName else Name := S;
        Style := [];
      end;
    finally
      FreeAndNil(RF);    
    end;
    if DebugMode then with fMain do Caption := Application.Title + ' [DEBUG]';
    CharWidth          := BG.Canvas.TextWidth(' ');
    CharHeight         := BG.Canvas.TextHeight(' ');
    HalfCharWidth      := CharWidth div 2;
    HalfCharHeight     := CharHeight div 2;
    BG.Width           := ScreenWidth * CharWidth;
    BG.Height          := ScreenHeight * CharHeight;
    with fMain do
    begin
      ClientWidth  := BG.Width + CharWidth;
      ClientHeight := BG.Height + CharHeight;
      Left := (Screen.Width div 2) - (Width div 2)
    end;
    //
    Lang.Load(A.Read('language'));
  finally
    FreeAndNil(A);
  end;
end;

end.
