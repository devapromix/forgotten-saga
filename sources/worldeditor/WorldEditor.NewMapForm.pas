unit WorldEditor.NewMapForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TfNew = class(TForm)
    btnClose: TBitBtn;
    cmdFill: TBitBtn;
    cbxTiles: TComboBox;
    procedure cmdFillClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fNew: TfNew;

implementation

{$R *.dfm}

uses WorldEditor.Classes, Common.Map, WorldEditor.MainForm;

procedure TfNew.cmdFillClick(Sender: TObject);
begin
  fMain.TerListBox.ItemIndex := fNew.cbxTiles.ItemIndex;
  fMain.TerListBoxClick(Self);
  Editor.Map.FillLayer(lrTerrain, Editor.Tile);
  Editor.Modified := True;
  fMain.UpdateCaption();
  fMain.Refresh;
end;

end.
