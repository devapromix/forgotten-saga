unit uStringUtils;

interface

uses Classes, uTypes;

type
  TStringUtils = class(TObject)
  public
    function Path: string;
    function IsNumber(const Source: string): Boolean;
    function IsSubStr(const SubStr: string; const Source: string; const IgnoreCase: Boolean = True): Boolean;
    function After(const SubStr: String; const Source: string; const IgnoreCase: Boolean = True): String;
    function Before(const SubStr: String; const Source: string; const IgnoreCase: Boolean = True): String;
    function Count(const SubStr: string; const Source: string; const IgnoreCase: Boolean = True): Integer;
    function CopyLeft(const Source: string; const Count: Integer): string;
    function CopyRight(const Source: string; const Count: Integer): string;
    function Key(const SubStr, Source: string; const IgnoreCase: Boolean = True): string;
    function Value(const SubStr, Source, Default: string; const IgnoreCase: Boolean = True): string;
    function LastPos(const SubStr, Source: string; const IgnoreCase: Boolean = True): Integer;
    function Explode(const CharSeparator : Char; const Source: string): TStringList; overload;
    function Explode(const StringSeparator: string; const Source: string): TExplodeResult; overload;
    function Implode(const StringSeparator: string; const Strings: TExplodeResult): string; overload;
    function Implode(const Strings: TExplodeResult): string; overload;
    function Replace(Source: string; const Search, Replace: string; const IgnoreCase: Boolean = True): string;
    function Invert(Source: string): string;
  end;

var
  SU: TStringUtils;

implementation

uses SysUtils;

{ TStringUtils }

function TStringUtils.CopyLeft(const Source: string;
  const Count: Integer): string;
begin
  Result := Copy(Source, 1, Count);
end;

function TStringUtils.CopyRight(const Source: string;
  const Count: Integer): string;
begin
  Result := Copy(Source, Length(Source) - Count + 1, Length(Source));
end;

function TStringUtils.Explode(const CharSeparator: Char;
  const Source: string): TStringList;
var
  I: integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  Strings.Delimiter := CharSeparator;
  Strings.DelimitedText := Source;
  for I := Strings.Count - 1 downto 0 do
    if (Strings[i] = '') then Strings.Delete(I);
  Result := Strings;
end;

function TStringUtils.Explode(const StringSeparator,
  Source: string): TExplodeResult;
var
  I: Integer;
  S: string;
begin
  S := Source;
  SetLength(Result, 0);
  I := 0;
  while Pos(StringSeparator, S) > 0 do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[I] := Copy(S, 1, Pos(StringSeparator, S) - 1);
    Inc(I);
    S := Copy(S, Pos(StringSeparator, S) + Length(StringSeparator), Length(S));
  end;
  SetLength(Result, Length(Result) + 1);
  Result[I] := Copy(S, 1, Length(S));
end;

function TStringUtils.Implode(const StringSeparator: string;
  const Strings: TExplodeResult): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Strings) - 1 do
    Result := Result + StringSeparator + Strings[I];
  System.Delete(Result, 1, Length(StringSeparator));
end;

function TStringUtils.Implode(const Strings: TExplodeResult): string;
begin
  Result := Implode('', Strings);
end;

function TStringUtils.LastPos(const SubStr, Source: string; const IgnoreCase: Boolean): Integer;
var
  Found, Len, Pos: Integer;
begin
  Pos := Length(Source);
  Len := Length(SubStr);
  Found := 0;
  while (Pos > 0) and (Found = 0) do
  begin
    if IgnoreCase then
    begin
      if (LowerCase(Copy(Source, Pos, Len)) = LowerCase(SubStr)) then Found := Pos;
      Dec(Pos);
    end else
    begin
      if Copy(Source, Pos, Len) = SubStr then Found := Pos;
      Dec(Pos);
    end;
  end;
  Result := Found;
end;

function TStringUtils.Path: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function TStringUtils.Replace(Source: string; const Search,
  Replace: string; const IgnoreCase: Boolean): string;
var
  I: Integer;
begin
  Result := '';
  repeat
    if IgnoreCase then I := Pos(LowerCase(Search), LowerCase(Source))
      else I := Pos(Search, Source);
    if I > 0 then
    begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Search), MaxInt);
    end else Result := Result + Source;
  until I <= 0;
end;

function TStringUtils.IsSubStr(const SubStr, Source: string; const IgnoreCase: Boolean): Boolean;
var
  I: Integer;
begin
  if IgnoreCase then I := Pos(LowerCase(SubStr), LowerCase(Source))
    else I := Pos(SubStr, Source);
  Result := (I > 0);
end;

function TStringUtils.IsNumber(const Source: string): Boolean;
var
  I, L: Integer;
begin
  Result := False;
  L := Length(Source);
  if (L = 0) then Exit;
  for I := 1 to L do
    if not (Source[I] in ['0'..'9']) then Exit;
  Result := True;
end;

function TStringUtils.Count(const SubStr, Source: string; const IgnoreCase: Boolean): Integer;
var
  I: Integer;
begin
  if IgnoreCase then I := Pos(LowerCase(SubStr), LowerCase(Source))
    else I := Pos(SubStr, Source);
  if (Length(SubStr) = 0) or (Source = '') or (I = 0) then Result := 0
    else Result := Length(Source) - Length(Replace(Source, SubStr, '', IgnoreCase))
      div Length(SubStr);
end;

function TStringUtils.Invert(Source: string): string;
var
  I, Len: Integer;
  S: string;
begin
  Len := Length(Source);
  S := Source;
  for I := 1 to Len do
    Source[I] := S[Len - I + 1];
  Result := Source;  
end;

function TStringUtils.After(const SubStr, Source: string; const IgnoreCase: Boolean): String;
var
  I: Integer;
begin
  if IgnoreCase then I := Pos(LowerCase(SubStr), LowerCase(Source))
    else I := Pos(SubStr, Source);
  if (I > 0) then Result := Copy(Source, I + Length(SubStr), Length(Source)) else Result:='';
end;

function TStringUtils.Before(const SubStr, Source: string; const IgnoreCase: Boolean): String;
var
  I: Integer;
begin
  if IgnoreCase then I := Pos(LowerCase(SubStr), LowerCase(Source))
    else I := Pos(SubStr, Source);
  if (I > 0) then Result := Copy(Source, 1, I - 1) else Result := '';
end;

function TStringUtils.Key(const SubStr, Source: string;
  const IgnoreCase: Boolean): string;
var
  I: Integer;
begin
  if IgnoreCase then I := Pos(LowerCase(SubStr), LowerCase(Source))
    else I := Pos(SubStr, Source);
  if (I <= 0) then
  begin
    Result := Source;
    Exit;
  end;
  Result := CopyLeft(Source, I - 1);
end;

function TStringUtils.Value(const SubStr, Source, Default: string;
  const IgnoreCase: Boolean): string;
var
  I: Integer;
begin
  if IgnoreCase then I := Pos(LowerCase(SubStr), LowerCase(Source))
    else I := Pos(SubStr, Source);
  if (I <= 0) then
  begin
    Result := Default;
    Exit;
  end;
  Result := CopyRight(Source, Length(Source) - I - Length(SubStr) + 1);
end;

initialization
  SU := TStringUtils.Create;

finalization
  FreeAndNil(SU);

end.
