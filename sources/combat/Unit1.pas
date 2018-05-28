unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    Button1: TButton;
    Button2: TButton;
    RichEdit2: TRichEdit;
    ListBox1: TListBox;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Math;

type
  TForce = (tfPlayer, tfEnemy);

type
  TDamage = record
    Min, Max: Integer;
  end;

type
  TMob = record
    Name: string;
    Ini: Integer;
    Life: Integer;
    Damage: TDamage;
    Agility: Integer;
    Force: TForce;
  end;

var
  Mob: array [0 .. 3] of TMob;
  PlayerIndex: Integer = 0;
  SelIndex: Integer = 0;
  IsRun: Boolean = False;
  RunFlag: Boolean = False;

function IsDefeat: Boolean;
begin
  Result := Mob[PlayerIndex].Life <= 0;
end;

function IsVictory: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Mob) to Length(Mob) - 1 do
  begin
    if (Mob[I].Force = tfPlayer) then
      Continue;
    if (Mob[I].Life > 0) then
      Exit;
  end;
  Result := True;
end;

procedure MobRefresh;
var
  I: Integer;
begin
  SelIndex := 0;
  Form1.RichEdit2.Clear;
  Form1.ListBox1.Clear;
  for I := Low(Mob) to Length(Mob) - 1 do
    with Mob[I] do
    begin
      Form1.RichEdit2.Lines.Add(Format('%s: %d [%d]', [Name, Life, Ini]));
      if (Life > 0) and (Force <> tfPlayer) then
        Form1.ListBox1.Items.Append(Name);
    end;
  Form1.ListBox1.ItemIndex := SelIndex;

  with Form1 do
    if IsVictory then
      Caption := '+'
    else
      Caption := '-';
end;

function AttackMob(var A, B: TMob): string;
var
  Damage: Integer;
  S: string;
begin
  if (A.Life <= 0) or (B.Life <= 0) then
    Exit;
  if (RandomRange(0, A.Agility) < RandomRange(0, B.Agility)) then
  begin
    Result := Format('%s miss. ', [A.Name]);
    Exit;
  end;
  S := '';
  Damage := RandomRange(A.Damage.Min, A.Damage.Max + 1);
  B.Life := B.Life - Damage;
  if (B.Life < 0) then
    B.Life := 0;
  S := S + Format('%s=>%s (%d). ', [A.Name, B.Name, Damage]);
  if (B.Life <= 0) then
  begin
    S := S + Format('%s die. ', [B.Name]);
    if (B.Force <> tfPlayer) then
      S := S + Format('Exp +%d. ', [1]);
  end;
  Result := S;
end;

procedure StartRound;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := Low(Mob) to Length(Mob) - 1 do
  begin
    if (Mob[I].Life <= 0) or (Mob[PlayerIndex].Life <= 0) then
      Continue;
    begin
      if (Mob[I].Force = tfPlayer) then
        Break;
      S := S + AttackMob(Mob[I], Mob[PlayerIndex]);
    end;
  end;
  if (Trim(S) <> '') then
    Form1.RichEdit1.Lines.Add(Trim(S));
  MobRefresh;
end;

procedure FinishRound;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := PlayerIndex + 1 to Length(Mob) - 1 do
  begin
    if (Mob[I].Life > 0) and (Mob[PlayerIndex].Life > 0) then
      S := S + AttackMob(Mob[I], Mob[PlayerIndex]);
  end;

  if (Trim(S) <> '') then
    Form1.RichEdit1.Lines.Add(Trim(S));
  MobRefresh;
end;

procedure StartCombat;
var
  I, J: Integer;
  M: TMob;
begin
  Form1.RichEdit1.Clear;

  for I := 0 to Length(Mob) - 2 do
  begin
    for J := 0 to Length(Mob) - 2 do
      if (Mob[J + 1].Ini > Mob[J].Ini) then
      begin
        M := Mob[J];
        Mob[J] := Mob[J + 1];
        Mob[J + 1] := M;
      end;
  end;

  PlayerIndex := -1;
  for I := Low(Mob) to High(Mob) do
    if (Mob[I].Force = tfPlayer) then
      PlayerIndex := I;

  StartRound;
end;

procedure Restart();
var
  I, E: Integer;
begin
  E := 0;
  for I := Low(Mob) to Length(Mob) - 1 do
    with Mob[I] do
    begin
      if (I <= 0) then
        Force := tfPlayer
      else
        Force := tfEnemy;
      if (Force = tfPlayer) then
      begin
        Name := 'Player';
        Ini := 20;
        Agility := 10;
        Life := 200;
      end
      else
      begin
        Inc(E);
        Name := 'Enemy[' + IntToStr(E) + ']';
        Ini := RandomRange(10, 30);
        Agility := 10;
        Life := 20;
      end;
      Damage.Min := 5;
      Damage.Max := 10;
    end;
  MobRefresh;
  StartCombat();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  Restart();
end;

// lukas ?: you're to there

function GetSel(Index: Integer): Integer;
var
  I, C: Integer;
begin
  C := -1;
  Result := -1;
  for I := 0 to Length(Mob) - 1 do
  begin
    if (Mob[I].Life > 0) and (Mob[I].Force <> tfPlayer) then
    begin
      Inc(C);
      if (C = Index) then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  S: string;
begin
  if IsRun then
    Exit;
  if (Mob[PlayerIndex].Life <= 0) then
    Exit;
  SelIndex := ListBox1.ItemIndex;
  if (SelIndex < 0) then
    SelIndex := 0;
  if (GetSel(SelIndex) < 0) then
    Exit;
  if (GetSel(SelIndex) = PlayerIndex) then
    Exit;
  if (Mob[GetSel(SelIndex)].Life <= 0) then
    Exit;

  if RunFlag then
    S := S + 'Player try run. '
  else
    S := S + AttackMob(Mob[PlayerIndex], Mob[GetSel(SelIndex)]);

  ListBox1.SetFocus;
  ListBox1.ItemIndex := 0;
  if not RunFlag then
    Form1.RichEdit1.Lines.Add(Trim(S));
  MobRefresh;

  FinishRound;
  Form1.RichEdit1.Lines.Add('-----');
  StartRound;
  RunFlag := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if IsRun or IsDefeat or IsVictory then
    Exit;
  if (RandomRange(0, 100) <= 25) then
  begin
    IsRun := True;
    Form1.RichEdit1.Lines.Add('RUN!');
  end
  else
  begin
    RunFlag := True;
    Button1Click(Sender);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to 99 do
    Button1.Click;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Restart;
end;

end.
