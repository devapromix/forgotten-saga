unit uScreenshot;

interface

procedure TakeScreenShot();

implementation

uses Windows, SysUtils, JPEG, uBox, uGraph;

procedure TakeScreenShot;
var
  T: TSystemTime;
  J: TJPEGImage;
  S: string;     
begin
  GetSystemTime(T);
  S := IntToStr(T.wYear) + IntToStr(T.wMonth) + IntToStr(T.wDay)
    + IntToStr(T.wHour) + IntToStr(T.wMinute) + IntToStr(T.wSecond) + '.jpg';
  J := TJPEGImage.Create;
  J.Assign(BG);
  J.SaveToFile(S);
  FreeAndNil(J);
//  Box(S);
end;

end.
