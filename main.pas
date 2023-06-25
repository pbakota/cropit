unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, ActnList, StdActns, StdCtrls, Spin, Menus;

const
  MaxUndoDepth = 50;

type

  { TDrawingTool }

  TDrawingTool = (dtNone, dtFreeHand, dtLine, dtEllipse, dtRect, dtArrow, dtText, dtSelect, dtTarget);

  { TMainForm }

  TMainForm = class(TForm)
    ToolShot: TAction;
    DrawingTarget: TAction;
    EditPasteAsLayer: TAction;
    EditCopySelection: TAction;
    EditPaste: TAction;
    EditCopy: TAction;
    EditUndo: TAction;
    Label3: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuCopy: TPopupMenu;
    MenuPaste: TPopupMenu;
    rbDrawTarget: TSpeedButton;
    SpeedButton16: TSpeedButton;
    FLineWidth: TSpinEdit;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolCrop: TAction;
    SpeedButton15: TSpeedButton;
    ToolErase: TAction;
    FColorButton: TColorButton;
    DrawingText: TAction;
    DrawingArrow: TAction;
    Label1: TLabel;
    Label2: TLabel;
    rbDrawArrow: TSpeedButton;
    rbDrawText: TSpeedButton;
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
    rbDrawSelect: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    rbDrawFreehand: TSpeedButton;
    rbDrawLine: TSpeedButton;
    rbDrawRect: TSpeedButton;
    rbDrawEllipse: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure DrawingTargetExecute(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCopySelectionExecute(Sender: TObject);
    procedure EditPasteAsLayerExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditUndoExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
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
    procedure ToolShotExecute(Sender: TObject);
  private
    FBitmap: TBitmap;
    FOverlay: TBitmap;
    FMouseDown: Boolean;
    FStartPos: TPoint;
    FSelection: TRect;
    FBlurRadius: Integer;
    FUndoStack: packed array[0..MaxUndoDepth] of TBitmap;
    FUndoIndex: Integer;
    FLayer: TBitmap;
    FLayerPos, FLayerOffset: TPoint;
    FMoveLayer: Boolean;
    FTargetNumbering: Integer;
    FExitOnCancel: Boolean;
    procedure NewBitmap(nw, nh: Integer);
    function GetCurDrawingTool: TDrawingTool;
    function ValidSelection: Boolean;
    procedure DrawArrow(ACanvas: TCanvas; X1,Y1,X2,Y2: Integer);
    procedure ClearSelection;
    procedure SaveUndo;
    procedure DoUndo;
    procedure CloneBitmap(var dst: TBitmap; src: TBitmap);
    procedure PasteInto(var target: TBitmap);
    procedure FixBitmap(var bitmap: TBitmap);
    procedure UpdateOverlay;
    procedure DrawTarget(ACanvas: TCanvas; X,Y: Integer);
  public

  end;

var
  MainForm: TMainForm;

implementation

uses
  Math, Clipbrd, LCLIntf, LCLType, IntfGraphics, PageSize, Shot, SelectArea

  {$IFDEF LCLQT5}
    {$IFDEF LINUX}
      , qtint, InterfaceBase,LCLPlatformDef,TypInfo
    {$ENDIF}
  {$ENDIF}
  ;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FileOpen1.Dialog.Filter:='Image files|*.png;*.bmp;*.jpg';
  FileOpen1.Dialog.Options:=[ofFileMustExist, ofAutoPreview, ofViewDetail];
  FileSaveAs1.Dialog.Filter:='Image files|*.png';
  FileSaveAs1.Dialog.DefaultExt:='png';
  FileSaveAs1.Dialog.Options:=[ofOverwritePrompt, ofPathMustExist, ofAutoPreview, ofViewDetail];

  {$IFDEF LCLQT5}
    {$IFDEF LINUX}
      WriteLn(Format('WM: %s, WidgetType: %s', [GetWindowManager, LCLPlatformDisplayNames[GetDefaultLCLWidgetType]]));
    {$ENDIF}
  {$ENDIF}

  Caption := 'CropIt!';

  FUndoIndex := 0; // Empty stack
  FLineWidth.Value := 3;
  rbDrawFreehand.Down := True;
  DrawingFreehand.Checked:= True;
  FColorButton.ButtonColor := clRed;
  FFontSize.Value:= 24;
  FBlurRadius := 8;

  FMoveLayer := False;

  FOverlay := TBitmap.Create;
  NewBitmap(800, 600);

  FExitOnCancel:=False;
  if Application.HasOption('s','shot') then begin
    FExitOnCancel := True;
    ToolShot.Execute;
  end
end;

procedure TMainForm.ToolShotExecute(Sender: TObject);
var
  Region: TShotRegion;
  MonitorIndex: Integer;
  Delay: Cardinal;
  DC: HDC;
  ScreenBitmap, Selection, TempBitmap: TBitmap;
  WindowHwnd: HWND;
  TempCanvas: TCanvas;
begin
  if TShotDlg.Execute(Region, MonitorIndex, Delay) then begin
    Application.ProcessMessages;
    Application.ProcessMessages;
    if Delay <> 0 then begin
      Sleep(Cardinal(Delay*1000));
    end;
    try
      ScreenBitmap := TBitmap.Create;
      case Region of
        reScreen: begin
          DC := GetDC(0);
          ScreenBitmap.LoadFromDevice(DC);
          if MonitorIndex <> -1 then begin
            with Screen.Monitors[MonitorIndex].WorkareaRect do begin
              //WriteLn(Format('Monitor %d,%d - %d,%d',[Left,Top,Width,Height]));
              NewBitmap(Width, Height);
              FBitmap.Canvas.CopyRect(Rect(0,0,Width,Height),ScreenBitmap.Canvas,Rect(Left,Top,Left+Width,Top+Height));
            end;
          end
          else begin
            NewBitmap(ScreenBitmap.Width, ScreenBitmap.Height);
            FBitmap.Canvas.Draw(0,0,ScreenBitmap);
          end;

          FixBitmap(FBitmap);
          UpdateOverlay;
        end;
        reWindow: begin
          TempCanvas := TCanvas.Create;
          try
            // NOTE: It seems it is not possible to get the system level foreground window
            // It works on Windows though.
            WindowHWND := GetForegroundWindow();
            DC := GetDC(WindowHWND);
            TempCanvas.Handle := DC;
            ScreenBitmap.LoadFromDevice(DC);
            NewBitmap(ScreenBitmap.Width, ScreenBitmap.Height);
            FBitmap.Canvas.Draw(0,0,ScreenBitmap);

            FixBitmap(FBitmap);
            UpdateOverlay;
          finally
            TempCanvas.Free;
          end;
        end;
        reArea: begin
          DC := GetDC(0);
          ScreenBitmap.LoadFromDevice(DC);
          TempBitmap := TBitmap.Create;
          try
            if MonitorIndex <> -1 then begin
              with Screen.Monitors[MonitorIndex].WorkareaRect do begin
                //WriteLn(Format('Monitor %d,%d - %d,%d',[Left,Top,Width,Height]));
                TempBitmap.SetSize(Width,Height);
                TempBitmap.Canvas.CopyRect(Rect(0,0,Width,Height),ScreenBitmap.Canvas,Rect(Left,Top,Left+Width,Top+Height));
              end;
            end
            else begin
              with ScreenBitmap do begin
                //WriteLn(Format('Monitor %d,%d - %d,%d',[0,0,Width,Height]));
                TempBitmap.SetSize(Width,Height);
                TempBitmap.Canvas.Draw(0,0,ScreenBitmap);
              end;
            end;
            Selection := TBitmap.Create;
            try
              if TSelectArea.Execute(TempBitmap, Selection) then begin
                NewBitmap(Selection.Width, Selection.Height);
                FBitmap.Canvas.Draw(0,0,Selection);

                FixBitmap(FBitmap);
                UpdateOverlay;

                FExitOnCancel:=False;
              end
            finally
              Selection.Free;
            end;
          finally
            TempBitmap.Free;
          end;
          if FExitOnCancel then
            Halt;
        end;
      end;
    finally
      ScreenBitmap.Free;
      ReleaseDC(0, DC);
    end;
  end
  else
    if FExitOnCancel then
      Halt;

  PaintBox1.Repaint;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  if Assigned(FBitmap) then FBitmap.Free;
  if FUndoIndex <> 0 then begin
    for I:=0 to FUndoIndex-1 do begin
      FUndoStack[i].Free;
    end;
  end;
  if Assigned(FLayer) then FLayer.Free;
  FOverlay.Free;
end;

procedure TMainForm.EditUndoExecute(Sender: TObject);
begin
  DoUndo;
end;

procedure TMainForm.FileNewExecute(Sender: TObject);
var
  nw, nh: Integer;
begin
  nw := FBitmap.Width;
  nh := FBitmap.Height;
  if TPageSizeDlg.Execute(nw, nh) then begin
    SaveUndo;
    NewBitmap(nw, nh);
    PaintBox1.Repaint;
  end;
end;

procedure TMainForm.FileOpen1Accept(Sender: TObject);
var
  pic: TPicture;
begin
  SaveUndo;
  try
    pic := TPicture.Create;
    pic.LoadFromFile(FileOpen1.Dialog.FileName);
    FBitmap.SetSize(pic.Bitmap.Width,pic.Bitmap.Height);
    FBitmap.Canvas.Draw(0,0,pic.Bitmap);
    FixBitmap(FBitmap);

    PaintBox1.Width := FBitmap.Width;
    PaintBox1.Height := FBitmap.Height;

    StatusBar1.SimpleText:=Format('Loaded image: w:=%d, h=%d, bpp=%d, transparent=%d', [
        FBitmap.Width,
        FBitmap.Height,
        Integer(PIXELFORMAT_BPP[FBitmap.PixelFormat]),
        Integer(FBitmap.Transparent)
      ]);

    PaintBox1.Repaint;
  finally
    pic.Free;
  end;
end;

procedure TMainForm.FileSaveAs1Accept(Sender: TObject);
var
  aPng: TPortableNetworkGraphic;
begin
  try
    aPng := TPortableNetworkGraphic.Create;
    aPng.PixelFormat := pf24bit;
    aPng.Assign(FBitmap);
    aPng.Transparent := False;
    aPng.SaveToFile(FileSaveAs1.Dialog.FileName);
  finally
    aPng.Free;
  end;
end;

procedure TMainForm.EditCopyExecute(Sender: TObject);
begin
  Clipboard.Assign(FBitmap);
end;

procedure TMainForm.DrawingTargetExecute(Sender: TObject);
begin
  FTargetNumbering := 1;
end;

procedure TMainForm.EditCopySelectionExecute(Sender: TObject);
var
  temp: TBitmap;
  w, h: Integer;
begin
  if not ValidSelection then Exit;

  w := Abs(FSelection.Width);
  h := Abs(FSelection.Height);
  try
    temp := TBitmap.Create;
    temp.PixelFormat:=pf32bit;
    temp.SetSize(w,h);
    temp.Canvas.CopyRect(Rect(0,0,w,h), FBitmap.Canvas, FSelection);
    Clipboard.Assign(temp);

    ClearSelection;
    PaintBox1.Repaint;
  finally
    temp.Free;
  end;
end;

procedure TMainForm.EditPasteAsLayerExecute(Sender: TObject);
begin
  if Assigned(FLayer) then FLayer.Free;
  FLayer := TBitmap.Create;
  FLayer.PixelFormat:=pf32bit;

  PasteInto(FLayer);
  FLayerPos.X := (FBitmap.Width - FLayer.Width) div 2;
  FLayerPos.Y := (FBitmap.Height - FLayer.Height) div 2;
  FLayerOffset.X := 0;
  FLayerOffset.Y := 0;

  UpdateOverlay;
  PaintBox1.Repaint;
end;

procedure TMainForm.EditPasteExecute(Sender: TObject);
begin
  PasteInto(FBitmap);
  PaintBox1.Width := FBitmap.Width;
  PaintBox1.Height := FBitmap.Height;

  StatusBar1.SimpleText:=Format('Pasted image: w:=%d, h=%d, bpp=%d, transparent=%d', [
    FBitmap.Width,
    FBitmap.Height,
    Integer(PIXELFORMAT_BPP[FBitmap.PixelFormat]),
    Integer(FBitmap.Transparent)
  ]);

  PaintBox1.Repaint;
end;

procedure TMainForm.PasteInto(var target: TBitmap);
var
  tempBitmap: TBitmap;
  PictureAvailable: boolean = False;
begin
  // we determine if any image is on clipboard
  if (Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap))) then
    PictureAvailable := True;

  if PictureAvailable then begin
    tempBitmap := TBitmap.Create;
    tempBitmap.Transparent:=False;

    if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
      tempBitmap.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));

    target.SetSize(tempBitmap.Width,tempBitmap.Height);
    target.Canvas.Draw(0,0,tempBitmap);

    FixBitmap(target);
    tempBitmap.Free;
  end;
end;

(* Stupid bitmap fix for pasted images *)
procedure TMainForm.FixBitmap(var bitmap: TBitmap);
var
  temp: TBitmap;
begin
  try
    temp := TBitmap.Create;
    CloneBitmap(temp, FBitmap);
    FBitmap.Assign(temp);
  finally
    temp.Free;
  end;
end;

procedure TMainForm.UpdateOverlay;
begin
  {$IFDEF WIN32}
  {$ELSE}
  FOverlay.SetSize(FBitmap.Width, FBitmap.Height);
  FOverlay.Canvas.Draw(0,0,FBitmap);
  {$ENDIF}
end;

procedure TMainForm.DrawTarget(ACanvas: TCanvas; X, Y: Integer);
var
  TargetSize: Integer;
  TargetRect: TRect;
  Style: TTextStyle;
begin
  with ACanvas do begin
    Brush.Style := bsSolid;
    Brush.Color := FColorButton.ButtonColor;
    TargetSize := (FFontSize.Value div 2)+3;
    TargetRect := Rect(X-TargetSize, Y-TargetSize, X + TargetSize*2, Y + TargetSize*2);
    Ellipse(TargetRect);
    Brush.Style := bsClear;
    Font.Color := clWhite;
    Font.Size := FFontSize.Value;
    Style := TextStyle;
    Style.Alignment:=taCenter;
    Style.Wordbreak:=False;
    Style.SingleLine:=True;
    TextRect(TargetRect, X, Y-(TextHeight('0') div 3), IntToStr(FTargetNumbering), Style);
  end;
end;

procedure TMainForm.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  FMouseDown := True;
  FStartPos.X := X;
  FStartPos.Y := Y;
  FSelection := Rect(0,0,0,0);

  UpdateOverlay;

  if Assigned(FLayer) then begin
    FMoveLayer := PtInRect(Rect(FLayerPos.X, FLayerPos.Y, FLayerPos.X + FLayer.Width, FLayerPos.Y + FLayer.Height), FStartPos);
    if FMoveLayer then begin
      FLayerOffset.X := FStartPos.X - FLayerPos.X;
      FLayerOffset.Y := FStartPos.Y - FLayerPos.Y;
      StatusBar1.SimpleText:=Format('lx=%d,ly=%d sx=%d, sy=%d, ox=%d,oy=%d', [FLayerPos.X, FLayerPos.Y, FStartPos.X, FStartPos.Y, FLayerOffset.X, FLayerOffset.Y]);
    end
    else
      UpdateOverlay;
  end
  else if GetCurDrawingTool = dtFreeHand then
    SaveUndo
end;

procedure TMainForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  EndPos: TPoint;
  DrawingCanvas: TCanvas;
begin
  if not FMouseDown then Exit;
  EndPos.X := X;
  EndPos.Y := Y;

  {$IFDEF WIN32}
  DrawingCanvas := PaintBox1.Canvas;
  {$ELSE}
  DrawingCanvas := FOverlay.Canvas;
  {$ENDIF}

  with DrawingCanvas do begin
    Pen.Color := FColorButton.ButtonColor;
    Pen.Width := FLineWidth.Value;
    Pen.Style := psSolid;
  end;
  {$IFDEF WIN32}
  {$ELSE}
  FOverlay.Canvas.Draw(0,0,FBitmap);
  {$ENDIF}

  PaintBox1.Repaint;

  if Assigned(FLayer) then begin
    if FMoveLayer then begin
      FLayerPos.X := EndPos.X - FLayerOffset.X;
      FLayerPos.Y := EndPos.Y - FLayerOffset.Y;
    end;

    Exit;
  end;

  with FBitmap.Canvas do begin
    Pen.Color := FColorButton.ButtonColor;
    Pen.Width := FLineWidth.Value;
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
      with DrawingCanvas do begin
        Line(FStartPos, EndPos);
      end;
    end;
    dtEllipse: begin
      with DrawingCanvas do begin
        Brush.Style := bsClear;
        Ellipse(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtRect: begin
      with DrawingCanvas do begin
        Brush.Style := bsClear;
        Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtArrow: begin
      with DrawingCanvas do begin
        Brush.Style := bsClear;
        DrawArrow(DrawingCanvas, FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      end;
    end;
    dtText: begin
      with DrawingCanvas do begin
        SaveHandleState;
        Brush.Style := bsClear;
        Pen.Style := psDash;
        Pen.Width := 1;
        Pen.Color := clWhite;
        Pen.Mode := pmXor;
        Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
        Pen.Mode := pmCopy;
        RestoreHandleState;
      end;
    end;
    dtSelect: begin
      with DrawingCanvas do begin
        SaveHandleState;
        Brush.Style := bsClear;
        Pen.Style := psDash;
        Pen.Width := 2;
        Pen.Color := clWhite;
        Pen.Mode := pmXor;
        Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
        Pen.Mode := pmCopy;
        RestoreHandleState;
      end;
      StatusBar1.SimpleText := Format('Selection: %dx%d', [Abs(EndPos.X-FStartPos.X), Abs(EndPos.Y-FStartPos.Y)]);
    end;
    dtTarget: begin
      DrawTarget(DrawingCanvas,FStartPos.X,FStartPos.Y);
      FStartPos := EndPos;
    end;
  end;
end;

procedure TMainForm.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  EndPos: TPoint;
  AText: string;
  style: TTextStyle;
  DrawingCanvas: TCanvas;
begin
  FMouseDown := False;
  EndPos.X := X;
  EndPos.Y := Y;

  {$IFDEF WIN32}
  DrawingCanvas := PaintBox1.Canvas;
  {$ELSE}
  DrawingCanvas := FOverlay.Canvas;
  {$ENDIF}

  if Assigned(FLayer) then begin
    if not FMoveLayer then begin
      SaveUndo;
      FBitmap.Canvas.Draw(FLayerPos.X, FLayerPos.Y, FLayer);
      FixBitmap(FBitmap);
      FreeAndNil(FLayer);

      PaintBox1.Repaint;
    end;

    Exit;
  end;

  with FBitmap.Canvas do begin
    Pen.Color := FColorButton.ButtonColor;
    Pen.Width := FLineWidth.Value;
    Pen.Style := psSolid;
  end;

  DrawingCanvas.Draw(0,0,FBitmap);

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
          style := TextStyle;
          style.Wordbreak:=True;
          style.SingleLine:=False;
          TextRect(Rect(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y), FStartPos.X, FStartPos.Y, AText, style);
        end;
      end;
    end;
    dtTarget: begin
      SaveUndo;
      DrawTarget(FBitmap.Canvas,FStartPos.X,FStartPos.Y);
      Inc(FTargetNumbering);
    end;
  end;
  PaintBox1.Repaint;

  if GetCurDrawingTool = dtSelect then begin
    // Save selection
    FSelection := Rect(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
    FSelection.NormalizeRect;
    StatusBar1.SimpleText := Format('Selection: %dx%d', [Abs(FSelection.Width), Abs(FSelection.Height)]);
    // Make selected are visible even when the mouse button is released.
    with DrawingCanvas do begin
      //SaveHandleState;
      Brush.Style := bsClear;
      Pen.Style := psDash;
      Pen.Width := 2;
      Pen.Color := clWhite;
      Pen.Mode := pmXor;
      Rectangle(FStartPos.X, FStartPos.Y, EndPos.X, EndPos.Y);
      Pen.Mode := pmCopy;
      //RestoreHandleState;
    end;
  end;
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);
begin
  if Assigned(FLayer) then begin
    PaintBox1.Canvas.Draw(0,0,FBitmap);
    with PaintBox1.Canvas do begin
      Draw(FLayerPos.X,FLayerPos.Y,FLayer);
      Pen.Color := clBlack;
      Pen.Width := 1;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(FLayerPos.X,FLayerPos.Y,FLayerPos.X + FLayer.Width, FLayerPos.Y + FLayer.Height);
    end;
  end
  else if Assigned(FBitmap) then begin
    {$IFDEF WIN32}
    PaintBox1.Canvas.Draw(0,0,FBitmap);
    {$ELSE}
    if FMouseDown or ValidSelection then
      PaintBox1.Canvas.Draw(0,0,FOverlay)
    else
      PaintBox1.Canvas.Draw(0,0,FBitmap);
    {$ENDIF}
  end;
end;

procedure TMainForm.ScrollBox1Paint(Sender: TObject);
const
  CheckPatternSize = 8;
var
  ColorPattern: array[0..1] of TColor = (clLtGray, clWhite);
  I, J: Integer;
  R: TRect;
  Offset: TPoint;
begin
  Offset := Point(ScrollBox1.HorzScrollBar.Position, ScrollBox1.VertScrollBar.Position);
  with ScrollBox1.Canvas do begin
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    for J:=0 to (Width div CheckPatternSize) do begin
      for I:=0 to (Height div CheckPatternSize) do begin
          Pen.Color := ColorPattern[(I+J) and 1];
          Brush.Color := ColorPattern[(I+J) and 1];
          R:=Rect(
            J*CheckPatternSize, I*CheckPatternSize,
            J*CheckPatternSize + CheckPatternSize, I*CheckPatternSize+CheckPatternSize
          );
          OffsetRect(R, Offset.X, Offset.Y);
          FillRect(R);
      end;
    end;
  end;
  PaintBox1.Left := Max((ScrollBox1.Width - PaintBox1.Width) div 2, 0);
  PaintBox1.Top := Max((ScrollBox1.Height - PaintBox1.Height) div 2, 0);
end;

procedure TMainForm.ToolBlurExecute(Sender: TObject);
begin
  if not ValidSelection then Exit;

  SaveUndo;
  with FBitmap.Canvas do begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(FSelection);
    Brush.Style := bsDiagCross;
    Brush.Color := FColorButton.ButtonColor;
    FillRect(FSelection);
  end;

  ClearSelection;
  PaintBox1.Repaint;
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

  UpdateOverlay;

  ClearSelection;

  PaintBox1.Width := FBitmap.Width;
  PaintBox1.Height := FBitmap.Height;
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

procedure TMainForm.NewBitmap(nw, nh: Integer);
begin
  if Assigned(FBitmap) then FreeAndNil(FBitmap);

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat:=pf32bit;
  FBitmap.SetSize(nw,nh);
  with FBitmap.Canvas do begin
    Brush.Color := clWhite;
    Pen.Color := clBlack;
    FillRect(FBitmap.Canvas.ClipRect);
  end;
  PaintBox1.Width := FBitmap.Width;
  PaintBox1.Height := FBitmap.Height;

  UpdateOverlay;
end;

procedure TMainForm.ClearSelection;
begin
  FSelection := Rect(0,0,0,0);
  StatusBar1.SimpleText:='';
end;

procedure TMainForm.SaveUndo;
var
  tmp: TBitmap;
begin
  if FUndoIndex = MaxUndoDepth-1 then begin
    ShowMessage('Too many undo');
    FMouseDown:=False;
    Exit;
  end;
  tmp := TBitmap.Create;
  CloneBitmap(tmp, FBitmap);
  FUndoStack[FUndoIndex] := tmp;
  FUndoIndex := FUndoIndex + 1;
end;

procedure TMainForm.DoUndo;
var
  tmp: TBitmap;
begin
  if FUndoIndex = 0 then Exit;
  FUndoIndex := FUndoIndex - 1;
  tmp := FUndoStack[FUndoIndex];
  CloneBitmap(FBitmap, tmp);
  tmp.Free;

  PaintBox1.Width := FBitmap.Width;
  PaintBox1.Height := FBitmap.Height;
  PaintBox1.Repaint;
end;

procedure TMainForm.CloneBitmap(var dst: TBitmap; src: TBitmap);
begin
  dst.SetSize(src.Width, src.Height);
  // There is some funny bug in Lazarus, when we paste image and start to draw on it,
  // the default pen will be "transparent" that means it starts to draw by erasing the image
  // partially and the backdrop pattern will be visible.
  // With this hack. Everything will return to normal. :)
  dst.Canvas.CopyRect(Rect(0,0,src.Width,src.Height), src.Canvas, Rect(0,0,src.Width,src.Height));
end;

function TMainForm.GetCurDrawingTool: TDrawingTool;
begin
  if rbDrawFreehand.Down then Result := dtFreeHand else
  if rbDrawLine.Down then Result := dtLine else
  if rbDrawEllipse.Down then Result := dtEllipse else
  if rbDrawArrow.Down then Result := dtArrow else
  if rbDrawText.Down then Result := dtText else
  if rbDrawSelect.Down then Result := dtSelect else
  if rbDrawRect.Down then Result := dtRect else
  if rbDrawTarget.Down then Result := dtTarget else
     raise EInvalidArgument.Create('Invalid tool');
end;

function TMainForm.ValidSelection: Boolean;
begin
  Result := not FSelection.IsEmpty;
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

end.

