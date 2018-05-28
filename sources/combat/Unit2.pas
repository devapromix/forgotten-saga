unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  CrProp = (cpForceID, cpName, cpLife);

type
  TForm2 = class(TForm)
    ListBox1: TListBox;
    ListBox2: TListBox;
    Button4: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Creatures: TStringList;
    procedure Restart();
    procedure Refresh();
    procedure Attack();
    procedure AddCreature(const S: string);
    function GetCreatureProp(const S: string; const V: CrProp): string;
    procedure ReplaceCreatureProp(var C: string; const S: string;
      const V: CrProp);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Restart();
begin
  Creatures.Clear;
  AddCreature('A|PLAYER|200');
  AddCreature('E|ENEMY|30');
  Refresh;
end;

procedure TForm2.AddCreature(const S: string);
begin
  if Trim(S) = '' then
    Exit;
  Creatures.Append(Trim(S));
end;

procedure TForm2.Attack;
begin

end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Restart();
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  Attack();
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Creatures := TStringList.Create;
  Restart();
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Creatures);
end;

function ExplodeString(const Separator, Source: string): TStringList;
begin
  Result := TStringList.Create();
  Result.Text := StringReplace(Source, Separator, #13, [rfReplaceAll]);
end;

function TForm2.GetCreatureProp(const S: string; const V: CrProp): string;
var
  SL: TStringList;
begin
  Result := '';
  SL := TStringList.Create;
  try
    SL.Assign(ExplodeString('|', S));
    Result := SL[Ord(V)];
  finally
    SL.Free;
  end;
end;

procedure TForm2.Refresh;
var
  I: Integer;
  S, F: string;
begin
  ListBox1.Clear;
  ListBox2.Clear;

  for I := 0 to Creatures.Count - 1 do
  begin
    S := GetCreatureProp(Creatures[I], cpForceID);
    F := Format('%s', [GetCreatureProp(Creatures[I], cpName)]);
    if (S = 'A') then
      ListBox1.Items.Append(F);
    if (S = 'E') then
      ListBox2.Items.Append(F);
  end;
end;

procedure TForm2.ReplaceCreatureProp(var C: string; const S: string;
  const V: CrProp);
var
  D, P: string;
  I: CrProp;
begin
  D := '';
  for I := Low(CrProp) to High(CrProp) do
  begin
    if I <> Low(CrProp) then
      P := '|'
    else
      P := '';
    if I <> V then
      D := D + P + GetCreatureProp(C, I)
    else
      D := D + P + Trim(S);
  end;
  C := D;
end;

end.
