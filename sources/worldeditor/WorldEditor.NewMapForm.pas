unit WorldEditor.NewMapForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TfNew = class(TForm)
    btnClose: TBitBtn;
    GroupBox1: TGroupBox;
    btTerFill: TBitBtn;
    cbTerrain: TComboBox;
    GroupBox2: TGroupBox;
    btObjFill: TBitBtn;
    cbObjects: TComboBox;
    procedure btTerFillClick(Sender: TObject);
    procedure btObjFillClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fNew: TfNew;

implementation

{$R *.dfm}

uses WorldEditor.Classes, WorldEditor.MainForm, ForgottenSaga.Entities;

procedure TfNew.btObjFillClick(Sender: TObject);
begin
  fMain.ObjListBox.ItemIndex := fNew.cbObjects.ItemIndex;
  fMain.ObjListBoxClick(Self);
  Editor.Map.FillLayer(lrObjects, Editor.Tile);
  Editor.Modified := True;
  fMain.UpdateCaption();
  fMain.Refresh;
end;

procedure TfNew.btTerFillClick(Sender: TObject);
begin
  fMain.TerListBox.ItemIndex := fNew.cbTerrain.ItemIndex;
  fMain.TerListBoxClick(Self);
  Editor.Map.FillLayer(lrTerrain, Editor.Tile);
  Editor.Modified := True;
  fMain.UpdateCaption();
  fMain.Refresh;
end;

end.
