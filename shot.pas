unit Shot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  Buttons, SpinEx;

type
  TShotRegion = (reScreen, reWindow, reArea);

  { TShotDlg }

  TShotDlg = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    chkCaptureMonitor: TCheckBox;
    cbMonitors: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    rbWholeScreen: TRadioButton;
    rbCurrentWindow: TRadioButton;
    rbArea: TRadioButton;
    edtDelay: TSpinEdit;
    procedure chkCaptureMonitorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    class function Execute(var Region: TShotRegion; var MonitorIndex: Integer; var Delay: Cardinal): Boolean; static;
  end;

implementation

{$R *.lfm}

{ TShotDlg }

procedure TShotDlg.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  cbMonitors.Clear;
  for I:=0 to Screen.MonitorCount-1 do begin
    if Screen.Monitors[I].Primary then
      cbMonitors.Items.Add('Primary monitor')
    else
      cbMonitors.Items.Add(Format('Monitor #%d',[I+1]));
  end;
  cbMonitors.ItemIndex:=0;
  if Screen.MonitorCount = 1 then
    cbMonitors.Enabled:=False;

  {$IFDEF LCLQT5}
    {$IFDEF LINUX}
    // NOTE: Not working on Linux (X11)
      rbCurrentWindow.Enabled := False;
    {$ENDIF}
  {$ENDIF}
end;

procedure TShotDlg.chkCaptureMonitorChange(Sender: TObject);
begin
  cbMonitors.Enabled:=chkCaptureMonitor.Checked;
end;

class function TShotDlg.Execute(var Region: TShotRegion; var MonitorIndex: Integer; var Delay: Cardinal): Boolean;
var
  frm: TShotDlg;
begin
  frm := TShotDlg.Create(nil);
  try
    with frm do begin
      edtDelay.Value:= 0;
      Result := ShowModal = mrOK;
      if Result then begin
        Delay := edtDelay.Value;

        if chkCaptureMonitor.Checked then
          MonitorIndex := cbMonitors.ItemIndex
        else
          MonitorIndex:= -1;

        if rbWholeScreen.Checked then
          Region := reScreen
        else if rbCurrentWindow.Checked then
          Region := reWindow
        else if rbArea.Checked then
          Region := reArea
      end
    end;
  finally
    frm.Free;
  end;
end;

end.

