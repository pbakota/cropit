program cropit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, lazcontrols, main, PageSize, Shot, SelectArea;

{$R *.res}

procedure ShowHelp;
begin
  WriteLn(Format('Usage: %s [options]', ['cropit']));
  WriteLn('-h/--help = This help');
  WriteLn('-s/--shot = Take a screenshot');
end;

begin
  if Application.HasOption('h', 'help') then begin
    ShowHelp;
    Halt;
  end;

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

  {$if declared(useHeapTrace)}
  	globalSkipIfNoLeaks := true; // supported as of debugger version 3.2.0
  {$endIf}
end.

