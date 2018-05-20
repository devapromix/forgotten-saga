unit WorldEditor.ProgressBarForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls;

type
  TfProgressBar = class(TForm)
    ProgressBar1: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fProgressBar: TfProgressBar;

implementation

{$R *.dfm}

end.
