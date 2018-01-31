unit configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, CustApp
  // custom units
  ;

type

  { TConfiguration }

  TConfiguration = class
  private
    FOwner: TCustomApplication;
    FIniFile: String;
    FIni: TMemIniFile;
    procedure LoadConfiguration;
  public
    constructor Create(TheOwner: TCustomApplication; IniFile: String);
    destructor Destroy; override;
    function ReadString(Section, Key: String; DefaultValue: String = ''): String;
    function ReadInteger(Section, Key: String; DefaultValue: Integer = 0): Integer;
    function ReadBool(Section, Key: String; DefaultValue: Boolean = False): Boolean;
  end;

implementation

{ TConfiguration }

procedure TConfiguration.LoadConfiguration;
begin
  if not FileExists(FIniFile) then
    begin
      WriteLn(Format('/!\ Configuration file not found (%s). Falling back to defaults.', [FIniFile]));
    end else begin
      WriteLn(Format('--[ Loading configuration from %s', [FIniFile]));
    end;

  try
    FIni := TMemIniFile.Create(FIniFile);
  except
    WriteLn('Error while reading config file!');
  end;
end;

constructor TConfiguration.Create(TheOwner: TCustomApplication; IniFile: String);
begin
  FOwner := TheOwner;
  FIniFile := IniFile;
  LoadConfiguration;
end;

destructor TConfiguration.Destroy;
begin
  inherited Destroy;
  if Assigned(FIni) then FIni.Free;
end;

function TConfiguration.ReadString(Section, Key: String; DefaultValue: String
  ): String;
begin
  Result := FIni.ReadString(Section, Key, DefaultValue);
end;

function TConfiguration.ReadInteger(Section, Key: String; DefaultValue: Integer
  ): Integer;
begin
  Result := FIni.ReadInteger(Section, Key, DefaultValue);
end;

function TConfiguration.ReadBool(Section, Key: String; DefaultValue: Boolean
  ): Boolean;
begin
  Result := FIni.ReadBool(Section, Key, DefaultValue);
end;

end.


