unit SelectArea;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCLType;

type

  { TSelectArea }

  TSelectArea = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FBitmap: TBitmap;
    FOverlay: TBitmap;
    FMouseDown: Boolean;
    FStartPos: TPoint;
    FSelection: TRect;
  public
    class function Execute(ABitmap: TBitmap; var Selection: TBitmap): Boolean; static;
  end;

implementation

{$R *.lfm}

{ TSelectArea }

procedure TSelectArea.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TSelectArea.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := True;
  FStartPos.X := X;
  FStartPos.Y := Y;
  FSelection := Rect(0,0,0,0);
end;

procedure TSelectArea.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  EndPos: TPoint;
begin
  if not FMouseDown then Exit;

  PaintBox1.Repaint;

  EndPos.X := X;
  EndPos.Y := Y;

  with FOverlay.Canvas do begin
    CopyRect(Rect(0,0,Width,Height), FBitmap.Canvas, Rect(Left,Top,Left+Width,Top+Height));
    Brush.Style := bsClear;
    Pen.Style := psDashDot;
    Pen.Width := 3;
    Pen.Color := clWhite;
    Pen.Mode:=pmXor;
    Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
    Pen.Mode := pmCopy;
  end;
end;

procedure TSelectArea.PaintBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  EndPos: TPoint;
begin
  FMouseDown := False;
  EndPos.X := X;
  EndPos.Y := Y;

  FSelection := Rect(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
  FSelection.NormalizeRect;

  ModalResult:=mrOK;
end;

procedure TSelectArea.PaintBox1Paint(Sender: TObject);
begin
  if FMouseDown then
    PaintBox1.Canvas.Draw(0,0,FOverlay)
  else
    with Screen.PrimaryMonitor do
      PaintBox1.Canvas.CopyRect(Rect(0,0,Width,Height), FBitmap.Canvas, Rect(Left,Top,Left+Width,Top+Height));
end;

procedure TSelectArea.Timer1Timer(Sender: TObject);
begin
  if not ClientRect.Contains(ScreenToClient(Mouse.CursorPos)) then
    ModalResult := mrCancel;
end;

class function TSelectArea.Execute(ABitmap: TBitmap; var Selection: TBitmap): Boolean;
var
  frm:TSelectArea;
begin
  frm:=TSelectArea.Create(nil);
  try
    with frm do begin
      //with Screen.PrimaryMonitor do
      //  WriteLn(Format('Primary %d,%d - %d,%d', [Left,Top,Width,Height]));

      FBitmap := ABitmap;
      FOverlay := TBitmap.Create;
      with Screen.PrimaryMonitor do begin
       FOverlay.SetSize(Width, Height);
       FOverlay.Canvas.CopyRect(Rect(0,0,Width,Height), FBitmap.Canvas, Rect(Left,Top,Left+Width,Top+Height));
      end;

      try
        WindowState := wsFullScreen;
        BorderStyle:=bsNone;
        KeyPreview:=True;
        FormStyle:=fsSystemStayOnTop;
        Result := ShowModal = mrOK;
        if Result then begin
          if FSelection.IsEmpty then
            Result := False
          else begin
            with Screen.PrimaryMonitor do begin
              Selection.SetSize(FSelection.Width,FSelection.Height);
              Selection.Canvas.CopyRect(Rect(0,0,FSelection.Width,FSelection.Height),FBitmap.Canvas,Rect(
                Left+FSelection.Left,
                Top+FSelection.Top,
                Left+FSelection.Left+FSelection.Width,
                Top+FSelection.Top+FSelection.Height
              ));
            end;
          end;
        end;
      finally
        FOverlay.Free;
      end;
    end;
  finally
    frm.Free;
  end;
end;

end.

