unit PageSize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TPageSizeDlg }

  TPageSizeDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbTemplates: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    spWidth: TSpinEdit;
    spHeight: TSpinEdit;
    procedure cbTemplatesSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    function Execute(var nw, nh: Integer): Boolean;
  end;

var
  PageSizeDlg: TPageSizeDlg;

implementation

{$R *.lfm}

{ TPageSizeDlg }

type
  Template = packed record
    w, h: Integer;
  end;

var
  Templates : array[0..3] of Template = (
  (w:640; h:480),
  (w:800; h:600),
  (w:1280; h:1024),
  (w:1920; h:1080)
  );

procedure TPageSizeDlg.cbTemplatesSelect(Sender: TObject);
begin
  spWidth.Value := Templates[cbTemplates.ItemIndex].w;
  spHeight.Value := Templates[cbTemplates.ItemIndex].h;
end;

procedure TPageSizeDlg.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  cbTemplates.Items.Clear;
  for I:= 0 to 3 do begin
      cbTemplates.Items.Add('%dx%d', [Templates[I].w, Templates[I].h]);
  end;
end;

function TPageSizeDlg.Execute(var nw, nh: Integer): Boolean;
begin
  spWidth.Value := nw;
  spHeight.Value := nh;
  if ShowModal = mrOK then begin
    nw := spWidth.Value;
    nh := spHeight.Value;
    Result := True;
  end
  else
    Result := False;
end;

end.

