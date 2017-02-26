unit uXML;

interface

uses Classes;

type
  TXML = class(TObject)
  private
    FXMLFile: TStringList;
    FFileName: string;
    function ReplaceString(Str, S1, S2: String): String;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    function Read(const Node: string): string;
    procedure Write(const Node, Data: string);
  end;

implementation

uses SysUtils;

{ TXML }

constructor TXML.Create(const FileName: string);
var
  XMLFile: TextFile;
begin
  FXMLFile := TStringList.Create;
  if not FileExists(FileName) then
  try
    AssignFile(XMLFile, FileName);
    Rewrite(XMLFile);
    WriteLn(XMLFile, '<?xml version="1.0" ?>');
    WriteLn(XMLFile, '<root>');
    WriteLn(XMLFile, '</root>');
  finally
    CloseFile(XMLFile);
  end;
  FXMLFile.LoadFromFile(FileName);    
  FFileName := FileName;
end;

destructor TXML.Destroy;
begin
  FreeAndNil(FXMLFile);
  inherited;
end;

function TXML.ReplaceString(Str, S1, S2: String): String;
var
  I: Integer;
  S, T: String;
begin
  S := '';
  T := Str;
  repeat
    I := Pos(LowerCase(S1), LowerCase(T));
    if (I > 0) then
    begin
      S := S + Copy(T, 1, I - 1) + S2;
      T := Copy(T, I + Length(S1), MaxInt);
    end else S := S + T;
  until (I <= 0);
  Result := S;
end;

function TXML.Read(const Node: string): string;
var
  A: Integer;
begin
  A := Pos(LowerCase('<' + Node + '>'), LowerCase(FXMLFile.Text)) + Length('<' + Node + '>');
  Result := Copy(FXMLFile.Text, A, Pos(LowerCase('</' + Node + '>'), LowerCase(FXMLFile.Text)) - A);
end;

procedure TXML.Write(const Node, Data: string);
var
  FData: string;
begin
  FData := Trim(Self.Read(Node));
  if (FData <> '') then
    FXMLFile.Text := Self.ReplaceString(FXMLFile.Text,
    LowerCase('<' + Node + '>') + FData + LowerCase('</' + Node + '>'),
    LowerCase('<' + Node + '>') + Data + LowerCase('</' + Node + '>'))
  else begin    
    FData := Trim(FXMLFile.Text);
    Insert(#9 + LowerCase('<' + Node + '>') + Data + LowerCase('</' + Node + '>') + #13#10,
      FData, Length(FXMLFile.Text) - 8);
    FXMLFile.Text := FData;
  end;
  FXMLFile.SaveToFile(FFileName);
end;

end.
