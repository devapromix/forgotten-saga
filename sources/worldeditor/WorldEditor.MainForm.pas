unit WorldEditor.MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, Buttons, ComCtrls, ToolWin, ImgList, StdCtrls,
  ForgottenSaga.Entities;

type
  TfMain = class(TForm)
    ToolsPanel: TPanel;
    ToolBar: TToolBar;
    StatusBar: TStatusBar;
    ImageList: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    btTerrain: TToolButton;
    btObjects: TToolButton;
    btItems: TToolButton;
    btCreatures: TToolButton;
    ToolButton3: TToolButton;
    brTerrain: TToolButton;
    brObjects: TToolButton;
    brItems: TToolButton;
    brCreatures: TToolButton;
    TerListBox: TListBox;
    Label1: TLabel;
    OD: TOpenDialog;
    SD: TSaveDialog;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ObjListBox: TListBox;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure brTerrainClick(Sender: TObject);
    procedure TerListBoxClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ObjListBoxClick(Sender: TObject);
    procedure brObjectsClick(Sender: TObject);
  private
    procedure LoadResources;
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateCaption;
    function GetCurrentLayer(): Byte;
    function GetRealTile(Index: Integer; Layer: TTiles.TLayerTypeEnum)
      : TTiles.TTileEnum;
  end;

var
  fMain: TfMain;

implementation

uses WorldEditor.Classes, WorldEditor.NewMapForm;

{$R *.dfm}

procedure Border(Pos: TPoint; Color: Integer; Symbol: Char = #32);
begin
  Editor.Engine.Surface.Canvas.Pen.Color := Color;
  Editor.Engine.Surface.Canvas.Brush.Style := bsClear;
  Editor.Engine.Surface.Canvas.Rectangle(Pos.X * Editor.Engine.Char.Width - 1,
    Pos.Y * Editor.Engine.Char.Height - 1, Pos.X * Editor.Engine.Char.Width +
    Editor.Engine.Char.Width + 1, Pos.Y * Editor.Engine.Char.Height +
    Editor.Engine.Char.Height + 1);
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  Editor := TEditor.Create;
  Editor.ToolBarHeight := ToolBar.Height;
  ClientWidth := Editor.Engine.Surface.Width + ToolsPanel.Width;
  ClientHeight := Editor.Engine.Surface.Height + Editor.ToolBarHeight +
    StatusBar.Height;
  Left := (Screen.Width div 2) - (Width div 2);
  Top := (Screen.Height div 2) - (Height div 2);
  btTerrain.Down := True;
  btObjects.Down := True;
  btItems.Down := True;
  btCreatures.Down := True;
  brTerrain.Click;
  brTerrain.Down := True;
  UpdateCaption;
  LoadResources;
  Editor.Tile := tNone;
  FormPaint(Sender);
end;

procedure TfMain.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Editor.MouseMove(TMap.TLayerEnum(GetCurrentLayer), X, Y);
  FormPaint(Sender);
  Label1.Caption := Format('%d:%d', [Editor.Pos.X, Editor.Pos.Y]);
end;

procedure TfMain.FormPaint(Sender: TObject);
begin
  Editor.Engine.Clear;
  if btTerrain.Down then
    Editor.RenderTerrain;
  if btObjects.Down then
    Editor.RenderObjects;
  if btItems.Down then
    Editor.RenderItems;
  if btCreatures.Down then
    Editor.RenderCreatures;
  if brTerrain.Down then
    Border(Editor.Pos, clYellow);
  if brObjects.Down then
    Border(Editor.Pos, clWhite);
  if brItems.Down then
    Border(Editor.Pos, clSkyBlue);
  if brCreatures.Down then
    Border(Editor.Pos, clFuchsia);
  Canvas.Draw(0, Editor.ToolBarHeight, Editor.Engine.Surface);
end;

function TfMain.GetCurrentLayer: Byte;
begin
  if brTerrain.Down then
    Result := 0;
  if brObjects.Down then
    Result := 1;
  if brItems.Down then
    Result := 2;
  if brCreatures.Down then
    Result := 3;
end;

function TfMain.GetRealTile(Index: Integer; Layer: TTiles.TLayerTypeEnum)
  : TTiles.TTileEnum;
var
  Tile: TTiles.TTileEnum;
  Counter: Byte;
  Flag: Boolean;
begin
  Counter := 0;
  Result := tNone;
  for Tile := Low(TTiles.TTileEnum) to High(TTiles.TTileEnum) do
  begin
    Flag := (Editor.Tiles.GetTile(Tile).Layer = Layer) or
      (Editor.Tiles.GetTile(Tile).Layer = ltBoth);
    if not Flag then
      Continue;
    if (Counter = Index) then
    begin
      Result := Tile;
      Break;
    end;
    if Flag then
      Inc(Counter);
  end;
end;

procedure TfMain.LoadResources;
var
  I: TTiles.TTileEnum;
  ObjFlag: Boolean;
  TerFlag: Boolean;
begin
  TerListBox.Clear;
  ObjListBox.Clear;
  for I := Low(TTiles.TTileEnum) to High(TTiles.TTileEnum) do
  begin
    TerFlag := (Editor.Tiles.GetTile(I).Layer = ltTerrain) or
      (Editor.Tiles.GetTile(I).Layer = ltBoth);
    ObjFlag := (Editor.Tiles.GetTile(I).Layer = ltObjects) or
      (Editor.Tiles.GetTile(I).Layer = ltBoth);
    if TerFlag then
      TerListBox.Items.Append(Format(TPlayer.KeyFmt,
        [Editor.Tiles.GetTile(I).Symbol, Editor.Tiles.GetTile(I).Name]));
    if ObjFlag then
      ObjListBox.Items.Append(Format(TPlayer.KeyFmt,
        [Editor.Tiles.GetTile(I).Symbol, Editor.Tiles.GetTile(I).Name]));
  end;
  TerListBox.ItemIndex := 0;
  ObjListBox.ItemIndex := 0;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  Editor.Free;
end;

procedure TfMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Editor.MouseDown(TMap.TLayerEnum(GetCurrentLayer), Button, X, Y);
  FormPaint(Sender);
  UpdateCaption;
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Editor.KeyDown(Key);
  FormPaint(Sender);
  UpdateCaption;
end;

procedure TfMain.brObjectsClick(Sender: TObject);
begin
  TerListBox.Visible := False;
  ObjListBox.Visible := True;
  ObjListBoxClick(Sender);
  FormPaint(Sender);
end;

procedure TfMain.brTerrainClick(Sender: TObject);
begin
  ObjListBox.Visible := False;
  TerListBox.Visible := True;
  TerListBoxClick(Sender);
  FormPaint(Sender);
end;

procedure TfMain.TerListBoxClick(Sender: TObject);
begin
  Editor.Tile := GetRealTile(TerListBox.ItemIndex, ltTerrain);
end;

procedure TfMain.ObjListBoxClick(Sender: TObject);
begin
  Editor.Tile := GetRealTile(ObjListBox.ItemIndex, ltObjects);
end;

procedure TfMain.ToolButton1Click(Sender: TObject);
begin
  if OD.Execute then
  begin
    Editor.CurrentMapFile := OD.FileName;
    Editor.Map.LoadFromFile(Editor.CurrentMapFile);
    Editor.Creatures.LoadFromFile(ChangeFileExt(Editor.CurrentMapFile, '.crt'));
    Editor.Items.LoadFromFile(ChangeFileExt(Editor.CurrentMapFile, '.itm'));
    UpdateCaption;
  end;
end;

procedure TfMain.ToolButton4Click(Sender: TObject);
begin
  if (Editor.CurrentMapFile <> '') then
  begin
    Editor.Map.SaveToFile(Editor.CurrentMapFile);
    Editor.Modified := False;
    UpdateCaption;
  end;
end;

procedure TfMain.ToolButton6Click(Sender: TObject);
begin
  fNew.Left := Self.Left + ((Self.Width div 2) - (fNew.Width div 2));
  fNew.Top := Self.Top + ((Self.Height div 2) - (fNew.Height div 2));
  fNew.cbTerrain.Items.Assign(Self.TerListBox.Items);
  fNew.cbTerrain.ItemIndex := Self.TerListBox.ItemIndex;
  fNew.cbObjects.Items.Assign(Self.ObjListBox.Items);
  fNew.cbObjects.ItemIndex := Self.ObjListBox.ItemIndex;
  fNew.ShowModal;
end;

procedure TfMain.UpdateCaption;
var
  S: string;
begin
  if Editor.Modified then
    S := '*'
  else
    S := '';
  if (Editor.CurrentMapFile = '') then
    Caption := Application.Title
  else
    Caption := Format('%s%s - %s', [ExtractFileName(Editor.CurrentMapFile), S,
      Application.Title]);
end;

end.
