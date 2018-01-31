unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

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
  private
    { Private declarations }
  public
    { Public declarations }
    Creatures: TStringList;
    procedure Restart();
    procedure Refresh();
    procedure AddCreature();
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Restart();
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

procedure TForm2.Refresh;
var
  I: Integer;
begin
  ListBox1.Clear;

  ListBox2.Clear;

end;

procedure TForm2.Restart();
begin
  Creatures.Clear;
  Refresh;
end;

end.
