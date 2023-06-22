unit Shot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  Buttons, SpinEx;

type
  TShotRegion = (reScreen, reWindow, reArea);
  TShotOption = (opWindowTitle, opMouseCursor);
  TShotOptions = set of TShotOption;

  { TShotDlg }

  TShotDlg = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    chkIncludeWindowTitle: TCheckBox;
    chkIncludeMouseCursor: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    rbWholeScreen: TRadioButton;
    rbCurrentWindow: TRadioButton;
    rbArea: TRadioButton;
    edtDelay: TSpinEdit;
  private

  public
    function Execute(var Region: TShotRegion; var Options: TShotOptions; var Delay: Cardinal): Boolean;
  end;

var
  ShotDlg: TShotDlg;

implementation

{$R *.lfm}

{ TShotDlg }

function TShotDlg.Execute(var Region: TShotRegion; var Options: TShotOptions; var Delay: Cardinal): Boolean;
begin
  edtDelay.Value:= 0;
  Result := ShowModal = mrOK;
  if Result then begin
    Delay := edtDelay.Value;
    Options := [];
    if chkIncludeWindowTitle.Checked then
      Options := Options + [opWindowTitle];
    if chkIncludeMouseCursor.Checked then
      Options := Options + [opMouseCursor];
    if rbWholeScreen.Checked then
      Region := reScreen
    else if rbCurrentWindow.Checked then
      Region := reWindow
    else if rbArea.Checked then
      Region := reArea
  end
end;

end.

