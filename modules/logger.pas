unit logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt;

type

  { TLogger }

  TLogger = class(TObject)
    protected
      FLogStream: TFileStream;
    public
      constructor Create(LogFile: String);
      destructor Destroy; override;
      procedure Output(Text: String);
      procedure Header(Text: String);
      procedure Log(Message: String);
      procedure Log(Message: String; const Arguments: array of const);
      procedure Warning(const Message: String);
      procedure Warning(const Message: String; const Arguments: array of const);
  end;

  var
    MainLogger: TLogger;

implementation

{ TLogger }

procedure TLogger.Output(Text: String);
begin
  Write(Text);
  FLogStream.Write(Text[1], Length(Text));
end;

procedure TLogger.Header(Text: String);
begin
  Log(LineEnding + '--[ %s ]--' + LineEnding, [Text]);
end;

constructor TLogger.Create(LogFile: String);
begin
  if not DirectoryExists('logs') then CreateDir('logs');
  FLogStream := TFileStream.Create(Format('logs%s%s.log', [
    DirectorySeparator,
    LogFile
  ]), fmCreate or fmShareDenyWrite);
  //     FormatDateTime('yyymmdd', Now)
end;

destructor TLogger.Destroy;
begin
  FLogStream.Free;
  inherited Destroy;
end;

procedure TLogger.Log(Message: String);
begin
  Log(StringReplace(Message, '%', '%%', [rfReplaceAll]), []);
end;

procedure TLogger.Log(Message: String; const Arguments: array of const);
var
  S: String;
begin
  S := Format(Message, Arguments);
  WriteLn(S);
  S := S + LineEnding;
  FLogStream.Write(S[1], Length(S));
end;

procedure TLogger.Warning(const Message: String);
begin
  Warning(Message, []);
end;

procedure TLogger.Warning(const Message: String;
  const Arguments: array of const);
var
  S: String;
begin
  S := Format(Message, Arguments);
  Write('[');
  TextColor(LightRed);
  Write('Warning');
  TextColor(LightGray);
  Write('] ');
  WriteLn(S);
  S := S + LineEnding;
  FLogStream.Write(S[1], Length(S));
end;


initialization
  MainLogger := TLogger.Create('ramp');

finalization
  { Free file stream }
  MainLogger.Free;

end.

