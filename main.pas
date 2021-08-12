unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, ActnList, StdActns, StdCtrls, Spin, fgl;

type

  TBitmapList = specialize TFPGObjectList<TBitmap>;

  { TDrawingTool }

  TDrawingTool = (dtNone, dtFreeHand, dtLine, dtEllipse, dtRect, dtArrow, dtText, dtSelect);

  { TMainForm }

  TMainForm = class(TForm)
    SpeedButton16: TSpeedButton;
    ToolCrop: TAction;
    SpeedButton15: TSpeedButton;
    ToolErase: TAction;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditUndo1: TEditUndo;
    FColorButton: TColorButton;
    ColorDialog1: TColorDialog;
    DrawingText: TAction;
    DrawingArrow: TAction;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    FFontSize: TSpinEdit;
    SpeedButton14: TSpeedButton;
    ToolBlur: TAction;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    DrawingSelect: TAction;
    DrawingRectangle: TAction;
    DrawingEllipse: TAction;
    DrawingLine: TAction;
    DrawingFreehand: TAction;
    BitBtn1: TBitBtn;
    FileNew: TAction;
    ActionList1: TActionList;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    ImageList1: TImageList;
    PaintBox1: TPaintBox;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure EditUndo1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ScrollBox1Paint(Sender: TObject);
    procedure ToolBlurExecute(Sender: TObject);
    procedure ToolCropExecute(Sender: TObject);
    procedure ToolEraseExecute(Sender: TObject);
  private
    FBitmap: TBitmap;
    FMouseDown: Boolean;
    FStartPos: TPoint;
    FPenWidth: Integer;
    FSelection: TRect;
    FBlurRadius: Integer;
    FUndoStack: TBitmapList;
    procedure NewBitmap;
    function GetCurDrawingTool: TDrawingTool;
    function ValidSelection: Boolean;
    procedure DrawArrow(ACanvas: TCanvas; X1,Y1,X2,Y2: Integer);
    procedure ClearSelection;
    procedure SaveUndo;
    procedure Undo;
    procedure Blurred(var bitmap: TBitmap; area: TRect; radius: Integer);
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  Math, IntfGraphics;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FileOpen1.Dialog.Filter:='Image files|*.png;*.bmp;*.jpg';
  FileOpen1.Dialog.Options:=[ofFileMustExist, ofAutoPreview, ofViewDetail];
  FileSaveAs1.Dialog.Filter:='Image files|*.png;*.bmp;*.jpg';
  FileSaveAs1.Dialog.DefaultExt:='png';
  FileSaveAs1.Dialog.Options:=[ofOverwritePrompt, ofPathMustExist, ofAutoPreview, ofViewDetail];

  FUndoStack := TBitmapList.Create;
  FPenWidth := 3;
  SpeedButton4.Down := True;
  DrawingFreehand.Checked:= True;
  FColorButton.ButtonColor := clRed;
  FFontSize.Value:= 24;
  FBlurRadius := 8;
  NewBitmap;
end;

procedure TMainForm.EditUndo1Execute(Sender: TObject);
begin
  Undo;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FBitmap) then FBitmap.Free;
  FUndoStack.Free;
end;

procedure TMainForm.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := True;
  FStartPos.X := X;
  FStartPos.Y := Y;
  FSelection := Rect(0,0,0,0);
end;

procedure TMainForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  EndPos: TPoint;
begin
  if not FMouseDown then Exit;
  EndPos.X := X;
  EndPos.Y := Y;

  PaintBox1.Repaint;

  with PaintBox1.Canvas do begin
    Pen.Color := FColorButton.ButtonColor;
    Pen.Width := FPenWidth;
    Pen.Style := psSolid;
  end;

  with FBitmap.Canvas do begin
    Pen.Color := FColorButton.ButtonColor;
    Pen.Width := FPenWidth;
    Pen.Style := psSolid;
  end;

  case GetCurDrawingTool of
    dtNone: begin
    end;
    dtFreeHand: begin
      with FBitmap.Canvas do begin
        Line(FStartPos, EndPos);
      end;
      FStartPos := EndPos;
    end;
    dtLine: begin
      with PaintBox1.Canvas do begin
        Line(FStartPos, EndPos);
      end;
    end;
    dtEllipse: begin
      with PaintBox1.Canvas do begin
        Brush.Style := bsClear;
        Ellipse(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtRect: begin
      with PaintBox1.Canvas do begin
        Brush.Style := bsClear;
        Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtArrow: begin
      with PaintBox1.Canvas do begin
        Brush.Style := bsClear;
        DrawArrow(PaintBox1.Canvas, FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtText: begin
      with PaintBox1.Canvas do begin
        Brush.Style := bsClear;
        Pen.Style := psDashDot;
        Pen.Width := 1;
        Pen.Color := clBlack;
        Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtSelect: begin
      with PaintBox1.Canvas do begin
        Brush.Style := bsClear;
        Pen.Style := psDashDot;
        Pen.Width := 1;
        Pen.Color := clBlack;
        Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
      StatusBar1.SimpleText := Format('Selection: %dx%d', [Abs(EndPos.X-FStartPos.X), Abs(EndPos.Y-FStartPos.Y)]);
    end;
  end;
end;

procedure TMainForm.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  EndPos: TPoint;
  AText: string;
begin
  FMouseDown := False;
  EndPos.X := X;
  EndPos.Y := Y;

  with FBitmap.Canvas do begin
    Pen.Color := FColorButton.ButtonColor;
    Pen.Width := FPenWidth;
    Pen.Style := psSolid;
  end;

  case GetCurDrawingTool of
    dtNone: begin
    end;
    dtFreeHand: begin
    end;
    dtLine: begin
      SaveUndo;
      with FBitmap.Canvas do begin
        Line(FStartPos, EndPos);
      end;
    end;
    dtEllipse: begin
      SaveUndo;
      with FBitmap.Canvas do begin
        Brush.Style := bsClear;
        Ellipse(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtRect: begin
      SaveUndo;
      with FBitmap.Canvas do begin
        Brush.Style := bsClear;
        Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtArrow: begin
      SaveUndo;
      with FBitmap.Canvas do begin
        Brush.Style := bsClear;
        DrawArrow(FBitmap.Canvas, FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtText: begin
      AText := InputBox('Enter text to insert', 'Text:', '');
      if Length(AText) > 0 then begin
        SaveUndo;
        with FBitmap.Canvas do begin
          Brush.Style := bsClear;
          Font.Color := FColorButton.ButtonColor;
          Font.Size := FFontSize.Value;
          TextRect(Rect(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y), FStartPos.X, FStartPos.Y, AText);
        end;
      end;
    end;
  end;
  PaintBox1.Repaint;

  if GetCurDrawingTool = dtSelect then begin
    // Save selection
    FSelection := Rect(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
    StatusBar1.SimpleText := Format('Selection: %dx%d', [FSelection.Width, FSelection.Height]);
    // Make selected are visible even when the mouse button is released.
    with PaintBox1.Canvas do begin
      Brush.Style := bsClear;
      Pen.Style := psDashDot;
      Pen.Width := 1;
      Pen.Color := clBlack;
      Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
    end;
  end;
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);
var
  cx, cy: Integer;
begin
  if Assigned(FBitmap) then begin
    //cx := (ScrollBox1.Width - FBitmap.Width) div 2;
    //cy := (ScrollBox1.Height - FBitmap.Height) div 2;
    cx := 0;
    cy := 0;
    PaintBox1.Canvas.Draw(cx,cy,FBitmap);
  end;
end;

procedure TMainForm.ScrollBox1Paint(Sender: TObject);
const
  CheckPatternSize = 32;
var
  ColorPattern: array[0..1] of TColor = (clLtGray, clWhite);
  I, J: Integer;
begin
  with ScrollBox1.Canvas do begin
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    for J:=0 to (Width div CheckPatternSize) do begin
      for I:=0 to (Height div CheckPatternSize) do begin
          Pen.Color := ColorPattern[(I+J) and 1];
          Brush.Color := ColorPattern[(I+J) and 1];
          FillRect(J*CheckPatternSize, I*CheckPatternSize, J*CheckPatternSize + CheckPatternSize, I*CheckPatternSize+CheckPatternSize);
      end;
    end;
  end;
end;

procedure TMainForm.ToolBlurExecute(Sender: TObject);
var
  s: string;
  l: LongInt;
begin
  if not ValidSelection then Exit;

  SaveUndo;
  with FBitmap.Canvas do begin
    //Pen.Color := FColorButton.ButtonColor;
    //Pen.Style := psSolid;
    //Pen.Width:=3;
    Brush.Style := bsDiagCross;
    Brush.Color := FColorButton.ButtonColor;
    FillRect(FSelection);
  end;

  ClearSelection;
  PaintBox1.Repaint;

  {$IFDEF XXXIGNORE}
  s := InputBox('Enter blur radius', 'Radius:', IntToStr(FBlurRadius));
  if (Length(s)<>0) and TryStrToInt(s, l) then begin
    FBlurRadius:= l;
    Blurred(FBitmap, FSelection, FBlurRadius);
    ClearSelection;
    PaintBox1.Repaint;
  end;
  {$ENDIF}
end;

procedure TMainForm.ToolCropExecute(Sender: TObject);
var
  temp: TBitmap;
begin
  if not ValidSelection then Exit;

  SaveUndo;

  temp := TBitmap.Create;
  temp.PixelFormat:=pf32bit;
  temp.SetSize(FSelection.Width, FSelection.Height);
  temp.Canvas.CopyRect(Rect(0,0,temp.Width, temp.Height), FBitmap.Canvas, FSelection);

  FBitmap.Free;
  FBitmap := temp;

  ClearSelection;
  PaintBox1.Repaint;
end;

procedure TMainForm.ToolEraseExecute(Sender: TObject);
begin
  if not ValidSelection then Exit;

  SaveUndo;

  with FBitmap.Canvas do begin
    Brush.Style := bsSolid;
    Brush.Color := FColorButton.ButtonColor;
    FillRect(FSelection);
  end;

  ClearSelection;
  PaintBox1.Repaint;
end;

procedure TMainForm.NewBitmap;
begin
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat:=pf32bit;
  FBitmap.SetSize(800,600);
  with FBitmap.Canvas do begin
    Brush.Color := clWhite;
    Pen.Color := clBlack;
    FillRect(FBitmap.Canvas.ClipRect);
  end;
  PaintBox1.Width := 800;
  PaintBox1.Height := 600;
end;

procedure TMainForm.ClearSelection;
begin
  FSelection := Rect(0,0,0,0);
  StatusBar1.SimpleText:='';
end;

procedure TMainForm.SaveUndo;
begin
  FUndoStack.Add(FBitmap);
end;

procedure TMainForm.Undo;
var
  tmp: TBitmap;
begin
  if FUndoStack.Count = 0 then Exit;
  tmp := FUndoStack.Last;
  FBitmap := tmp;
  FUndoStack.Remove(tmp);
end;

function TMainForm.GetCurDrawingTool: TDrawingTool;
begin
  if DrawingFreeHand.Checked then Result := dtFreeHand else
  if DrawingLine.Checked then Result := dtLine else
  if DrawingEllipse.Checked then Result := dtEllipse else
  if DrawingArrow.Checked then Result := dtArrow else
  if DrawingText.Checked then Result := dtText else
  if DrawingSelect.Checked then Result := dtSelect else
  if DrawingRectangle.Checked then Result := dtRect;
end;

function TMainForm.ValidSelection: Boolean;
begin
  Result := not (Abs(FSelection.Width) = 0) or (Abs(FSelection.Height) = 0);
end;

procedure TMainForm.DrawArrow(ACanvas: TCanvas; X1, Y1, X2, Y2: Integer);
const
  ArrowSize = 15.0;
var
  Angle: Double;
  ap1, ap2: TPoint;
begin
  ACanvas.Line(X1,Y1,X2,Y2);

  Angle := -(ArcTan2(Y1-Y2, X1-X2)) - pi();
  ap1.X := Round(Double(X2) + Sin(Angle - pi()/3) * ArrowSize);
  ap1.Y := Round(Double(Y2) + Cos(Angle - pi()/3) * ArrowSize);

  ap2.X := Round(Double(X2) + Sin(Angle - pi() + pi()/3) * ArrowSize);
  ap2.Y := Round(Double(Y2) + Cos(Angle - pi() + pi()/3) * ArrowSize);

  ACanvas.Line(X2,Y2,ap1.X, ap1.Y);
  ACanvas.Line(X2,Y2,ap2.X, ap2.Y);
end;

procedure TMainForm.Blurred(var bitmap: TBitmap; area: TRect; radius: Integer);
var
  tab: array of Integer = ( 14, 10, 8, 6, 5, 5, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2 );
  i,j, row, col, i1, i2, bpl, alpha, r1, r2, c1, c2: Integer;
  image: TLazIntfImage;
  rgba: array[0..3] of Integer;
  p: PByte;
begin
  if radius < 1 then alpha := 16 else
  if radius > 17 then alpha := 1 else
    alpha := tab[radius-1];

  r1 := area.Top;
  r2 := area.Bottom;
  c1 := area.Left;
  c2 := area.Right;

  image := bitmap.CreateIntfImage;
  bpl := image.DataDescription.BytesPerLine;

  i1 := 0;
  i2 := 3;

  (*
  for (int col = c1; col <= c2; col++) {
    p = result.scanLine(r1) + col * 4;
    for (int i = i1; i <= i2; i++)
      rgba[i] = p[i] << 4;

    p += bpl;
    for (int j = r1; j < r2; j++, p += bpl)
      for (int i = i1; i <= i2; i++)
	p[i] = (rgba[i] += ((p[i] << 4) - rgba[i]) * alpha / 16) >> 4;
  }
  *)
  for col := c1 to c2+1 do begin
    p := image.GetDataLineStart(r1) + col * 4;
    WriteLn(Format('%p', [p]));
    for i := i1 to i2+1 do begin
      rgba[i] := p[i] shl 4;
    end;
    p := image.GetDataLineStart(r1+1) + col * 4;
    for j := r1 to r2 do begin
      for i := i1 to i2+1 do begin
        inc(rgba[i], (((p[i] shl 4) - rgba[i]) * alpha div 16) shr 4);
        p[i] := rgba[i];
      end;
      p := image.GetDataLineStart(j-r1+1) + col * 4;
    end;
  end;

  {$IFDEF XXXX}
  (*
  for (int row = r1; row <= r2; row++) {
    p = result.scanLine(row) + c1 * 4;
    for (int i = i1; i <= i2; i++)
      rgba[i] = p[i] << 4;

    p += 4;
    for (int j = c1; j < c2; j++, p += 4)
      for (int i = i1; i <= i2; i++)
      p[i] = (rgba[i] += ((p[i] << 4) - rgba[i]) * alpha / 16) >> 4;
  }
  *)
  for row := r1 to r2+1 do begin
    p := image.GetDataLineStart(row) + c1 * 4;
    for i := i1 to i2+1 do begin
      rgba[i] := p[i] shl 4;
    end;

    inc(p, 4);
    for j := c1 to c2 do begin
      for i := i1 to i2+1 do begin
        inc(rgba[i], (((p[i] shl 4) - rgba[i]) * alpha div 16) shr 4);
        p[i] := rgba[i];
      end;
      inc(p, 4);
    end;
  end;

  (*
  for (int col = c1; col <= c2; col++) {
    p = result.scanLine(r2) + col * 4;
    for (int i = i1; i <= i2; i++)
      rgba[i] = p[i] << 4;

    p -= bpl;
    for (int j = r1; j < r2; j++, p -= bpl)
      for (int i = i1; i <= i2; i++)
	p[i] = (rgba[i] += ((p[i] << 4) - rgba[i]) * alpha / 16) >> 4;
  }
  *)
  for col := c1 to c2+1 do begin
    p := image.GetDataLineStart(r2) + col * 4;
    for i := i1 to i2+1 do begin
      rgba[i] := p[i] shl 4;
    end;
    Inc(p, -bpl);
    for j := r1 to r2 do begin
      for i := i1 to i2+1 do begin
        inc(rgba[i], (((p[i] shl 4) - rgba[i]) * alpha div 16) shr 4);
        p[i] := rgba[i];
      end;
      Inc(p, -bpl);
    end;
  end;

  (*
  for (int row = r1; row <= r2; row++) {
    p = result.scanLine(row) + c2 * 4;
    for (int i = i1; i <= i2; i++)
      rgba[i] = p[i] << 4;

    p -= 4;
    for (int j = c1; j < c2; j++, p -= 4)
      for (int i = i1; i <= i2; i++)
  	p[i] = (rgba[i] += ((p[i] << 4) - rgba[i]) * alpha / 16) >> 4;
  }
  *)
  for row := r1 to r2+1 do begin
    p := image.GetDataLineStart(row) + c2 * 4;
    for i := i1 to i2+1 do begin
      rgba[i] := p[i] shl 4;
    end;

    inc(p, -4);
    for j := c1 to c2 do begin
      for i := i1 to i2+1 do begin
        inc(rgba[i], (((p[i] shl 4) - rgba[i]) * alpha div 16) shr 4);
        p[i] := rgba[i];
      end;
      inc(p, -4);
    end;
  end;
  {$ENDIF}

  bitmap.LoadFromIntfImage(image);
  image.Free;

end;

end.

