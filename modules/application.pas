unit application;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp, crt,
  fileinfo,
  winpeimagereader, {need this for reading exe info}
  elfreader, {needed for reading ELF executables}

  // custom units
  defaults, configuration;

type

  { TRetroAmber }

  TRetroAmber = class(TCustomApplication)
  protected
    FVersion: String;
    FConfig: TConfiguration;
    function GetVersion: String;
    procedure DoRun; override;
    procedure LoadConfiguration;
    procedure InitializeModules;
    procedure DeinitializeModules;
    procedure PaintLogo(PrimaryColor, SecondaryColor: Byte);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    property Version: String read GetVersion;
  end;

implementation

{ TRetroAmber }

procedure TRetroAmber.DoRun;
begin

  ClrScr;
  cursoroff;
  SetCurrentDir(ExtractFileDir(ExeName));

  if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

  PaintLogo(Yellow, White);
  InitializeModules;
  LoadConfiguration;

  while not KeyPressed do
    begin
      CheckSynchronize(0);
    end;

  DeinitializeModules;
  Terminate(0);
end;

procedure TRetroAmber.LoadConfiguration;
begin
  if HasOption('c', 'config') then
    begin
      FConfig := TConfiguration.Create(Self, GetOptionValue('c', 'config'));
    end else begin
      FConfig := TConfiguration.Create(Self, RADefaultConfigFile);
    end;
end;

procedure TRetroAmber.InitializeModules;
begin
  WriteLn('--[ Initializing modules');
end;

procedure TRetroAmber.DeinitializeModules;
begin
  WriteLn('--[ Shuting down modules');
end;

function TRetroAmber.GetVersion: String;
var
  FileVerInfo: TFileVersionInfo;
begin

  if not (FVersion = '') then
    begin
      Result := FVersion;
      Exit;
    end;

  FileVerInfo := TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    FVersion := FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;

  Result := FVersion;
end;

procedure TRetroAmber.PaintLogo(PrimaryColor, SecondaryColor: Byte);
begin
  WriteLn;
  TextColor(SecondaryColor);
  Write('     ');
  TextColor(PrimaryColor);
  Write('___');
  TextColor(SecondaryColor);
  Write('     _             ');
  TextColor(PrimaryColor);
  Write('_');
  TextColor(SecondaryColor);
  Write('         _             ');
  TextColor(PrimaryColor);
  Write('___');
  TextColor(SecondaryColor);
  Write('          _        _     ' + LineEnding);
  Write('    ');
  TextColor(PrimaryColor);
  Write('| _ \');
  TextColor(SecondaryColor);
  Write('___| |_ _ _ ___  ');
  TextColor(PrimaryColor);
  Write('/_\  _ __');
  TextColor(SecondaryColor);
  Write(' | |__  ___ _ _');
  TextColor(PrimaryColor);
  Write('| _ \');
  TextColor(SecondaryColor);
  Write('_ _ ___ (_)___ __| |_   ' + LineEnding);
  Write('    ');
  TextColor(PrimaryColor);
  Write('|   /');
  TextColor(SecondaryColor);
  Write(' -_)  _| ''_/ _ \');
  TextColor(PrimaryColor);
  Write('/ _ \| ''  \');
  TextColor(SecondaryColor);
  Write('| ''_ \/ -_) ''_');
  TextColor(PrimaryColor);
  Write('|  _/');
  TextColor(SecondaryColor);
  Write(' ''_/ _ \| / -_) _|  _|  ' + LineEnding);
  Write('    ');
  TextColor(PrimaryColor);
  Write('|_|_\');
  TextColor(SecondaryColor);
  Write('___|\__|_| \___');
  TextColor(PrimaryColor);
  Write('/_/ \_\_|_|_|');
  TextColor(SecondaryColor);
  Write('_.__/\___|_| ');
  TextColor(PrimaryColor);
  Write('|_|');
  TextColor(SecondaryColor);
  Write(' |_| \___// \___\__|\__|  ' + LineEnding);
  Write('                                                             |__/ ');
  TextColor(Brown);
  Write(Format('%11s' + LineEnding, [Version]));
  Write(LineEnding);
  TextColor(LightGray);
end;

constructor TRetroAmber.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TRetroAmber.Destroy;
begin
  inherited Destroy;
end;

procedure TRetroAmber.WriteHelp;
begin
  Writeln('Usage: ', ExeName, ' -h');
end;

end.

