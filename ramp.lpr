program ramp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  application;

var
  RAmpApp: TRetroAmber;

{$R *.res}

begin
  RAmpApp := TRetroAmber.Create(nil);
  RAmpApp.Title := 'Retro Amber Project';
  RAmpApp.Run;
  RAmpApp.Free;
end.

