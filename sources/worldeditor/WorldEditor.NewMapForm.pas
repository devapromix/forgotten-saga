unit WorldEditor.NewMapForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls;

type
  TfNew = class(TForm)
    btnClose: TBitBtn;
    GroupBox1: TGroupBox;
    btTerFill: TBitBtn;
    cbTerrain: TComboBox;
    GroupBox2: TGroupBox;
    btObjFill: TBitBtn;
    cbObjects: TComboBox;
    btGen: TBitBtn;
    cbFloor: TComboBox;
    cbWall: TComboBox;
    edNum: TEdit;
    UpDown1: TUpDown;
    GroupBox3: TGroupBox;
    btAddTerSpot: TBitBtn;
    cbTerSpot: TComboBox;
    edTerSpotCount: TEdit;
    UpDown2: TUpDown;
    edTerSpotSize: TEdit;
    UpDown3: TUpDown;
    GroupBox4: TGroupBox;
    btAddObjSpot: TBitBtn;
    cbObjSpot: TComboBox;
    edObjSpotCount: TEdit;
    UpDown4: TUpDown;
    edObjSpotSize: TEdit;
    UpDown5: TUpDown;
    procedure btTerFillClick(Sender: TObject);
    procedure btObjFillClick(Sender: TObject);
    procedure btGenClick(Sender: TObject);
    procedure btAddTerSpotClick(Sender: TObject);
    procedure btAddObjSpotClick(Sender: TObject);
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

procedure TfNew.btAddObjSpotClick(Sender: TObject);
var
  G: TMapGenerator;
  I, C: Integer;
begin
  fMain.ObjListBox.ItemIndex := fNew.cbObjSpot.ItemIndex;
  G := TMapGenerator.Create;
  G.Map := Editor.Map;
  try
    G.Num := StrToIntDef(edObjSpotSize.Text, 25);
    C := StrToIntDef(edObjSpotCount.Text, 10);
    for I := 1 to C do
      G.AddSpot(fMain.GetRealTile(cbObjSpot.ItemIndex, ltObjects), lrObjects);
  finally
    G.Free;
  end;
  Editor.Modified := True;
  fMain.UpdateCaption();
  fMain.Refresh;
end;

procedure TfNew.btAddTerSpotClick(Sender: TObject);
var
  G: TMapGenerator;
  I, C: Integer;
begin
  fMain.TerListBox.ItemIndex := fNew.cbTerSpot.ItemIndex;
  G := TMapGenerator.Create;
  G.Map := Editor.Map;
  try
    G.Num := StrToIntDef(edTerSpotSize.Text, 25);
    C := StrToIntDef(edTerSpotCount.Text, 10);
    for I := 1 to C do
      G.AddSpot(fMain.GetRealTile(cbTerSpot.ItemIndex, ltTerrain), lrTerrain);
  finally
    G.Free;
  end;
  Editor.Modified := True;
  fMain.UpdateCaption();
  fMain.Refresh;
end;

procedure TfNew.btGenClick(Sender: TObject);
var
  G: TMapGenerator;
  I: Integer;
begin
  G := TMapGenerator.Create;
  G.Map := Editor.Map;
  try
    G.MGTiles.Wall := (fMain.GetRealTile(cbWall.ItemIndex, ltTerrain));
    G.MGTiles.Floor := (fMain.GetRealTile(cbFloor.ItemIndex, ltTerrain));
    G.Num := StrToIntDef(edNum.Text, 7);
    G.Map.Clear;
    G.Map.FillLayer(lrTerrain, G.MGTiles.Wall);
    G.Start := Point(TMap.Size.Width div 2, TMap.Size.Height div 2);
    G.GenCave();
  finally
    G.Free;
  end;
  Editor.Modified := True;
  fMain.UpdateCaption();
  fMain.Refresh;
end;

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
