{*************************************************************}
{            ResFont Component for Delphi                     }
{ Version:   1.0                                              }
{ Author:    Sergey Tkach (APROMIX), Ukraine                  }
{ E-Mail:    bees@meta.ua                                     }
{ Home Page: http://rlgclub.ru                                }
{ Created:   Jan, 5, 2012                                     }
{ Modified:  Jan, 5, 2012                                     }
{*************************************************************}
{                     IMPORTANT NOTE:                         }
{ This software is provided 'as-is', without any express or   }
{ implied warranty. In no event will the author be held       }
{ liable for any damages arising from the use of this         }
{ software.                                                   }
{ Permission is granted to anyone to use this software for    }
{ any purpose, including commercial applications, and to      }
{ alter it and redistribute it freely, subject to the         }
{ following restrictions:                                     }
{ 1. The origin of this software must not be misrepresented,  }
{    you must not claim that you wrote the original software. }
{    If you use this software in a product, an acknowledgment }
{    in the product documentation would be appreciated but is }
{    not required.                                            }
{ 2. Altered source versions must be plainly marked as such,  }
{    and must not be misrepresented as being the original     }
{    software.                                                }
{ 3. This notice may not be removed or altered from any       }
{    source distribution.                                     }
{*************************************************************}

{
USE:
...
var
  A: TResFont;
...
  A := TResFont.Create;
  try
    A.LoadFromFile('BosaNova.ttf');
    Label1.Font.Name := A.FontName;
    Label1.Caption := 'ABCDEF ¿¡¬√ƒ≈';
  finally
    A.Free;
  end;
...
}

unit uResFont;  

interface

uses Windows, SysUtils;  

type
  TResFont = class
  private
    FFontMem: HFont;
    FFontName: string;
    function GetFontName (FontFileA : PChar) : String;
  public
    property FontName: string read FFontName;
    procedure LoadFromFile(FName: TFileName);
  end;

implementation

uses Classes, Graphics;

{ TResFont }

function TResFont.GetFontName(FontFileA: PChar): String;
type
  TGetFontResourceInfoW = function (FontPath : PWideChar; var BufSize : DWORD; FontName : PWideChar; dwFlags : DWORD) : DWORD; stdcall;
var
  GetFontResourceInfoW : TGetFontResourceInfoW;
  FontFileW : PWideChar;
  FontNameW : PWideChar;
  FontFileWSize, FontNameSize : DWORD;
begin
  Result := '';
  GetFontResourceInfoW := GetProcAddress(GetModuleHandle('gdi32.dll'), 'GetFontResourceInfoW');
  if @GetFontResourceInfoW = nil then Exit;
  if AddFontResource(FontFileA) = 0 then Exit;
  FontFileWSize := (Length(FontFileA)+1)*2;
  GetMem(FontFileW, FontFileWSize);
  StringToWideChar(FontFileA, FontFileW, FontFileWSize);
  FontNameSize := 0;
  FontNameW := nil;
  GetFontResourceInfoW (FontFileW, FontNameSize, FontNameW, 1);
  GetMem (FontNameW, FontNameSize);
  FontNameW^ := #0;
  GetFontResourceInfoW (FontFileW, FontNameSize, FontNameW, 1);
  Result := FontNameW;
  FreeMem (FontFileW);
  FreeMem (FontNameW);
  RemoveFontResource(FontFileA);
end;

procedure TResFont.LoadFromFile(FName: TFileName);
var
  A: TMemoryStream;
  I: DWord;
begin
  A := TMemoryStream.Create;
  if FileExists(FName) then
  try
    A.LoadFromFile(FName);
    fFontMem := AddFontMemResourceEx(A.Memory, A.Size, nil, @i);
    FFontName := GetFontName(PAnsiChar(FName));
    CreateFontA(36, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET,
      OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
      DEFAULT_PITCH, PAnsiChar(FFontName));
  finally
    FreeAndNil(A);
  end;
end;

end.
