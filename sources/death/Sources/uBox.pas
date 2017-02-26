unit uBox;

interface

uses Windows, SysUtils;

procedure Box(); overload;
procedure Box(const BoxStrMessage: String); overload;
procedure Box(const BoxIntMessage: Integer); overload;
procedure Box(const BoxBoolMessage: Boolean); overload;

implementation

procedure Box(); overload;
begin
  MessageBox(0, '', '', MB_OK);
end;

procedure Box(const BoxStrMessage: String); overload;
begin
  MessageBox(0, PChar(BoxStrMessage), '', MB_OK);
end;

procedure Box(const BoxIntMessage: Integer); overload;
begin
  MessageBox(0, PChar(IntToStr(BoxIntMessage)), '', MB_OK);
end;

procedure Box(const BoxBoolMessage: Boolean); overload;
begin
  MessageBox(0, PChar(BoolToStr(BoxBoolMessage)), '', MB_OK);
end;

end.
