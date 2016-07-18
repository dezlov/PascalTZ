program tester;

{*******************************************************************************
This file is a part of PascalTZ package:
  https://github.com/dezlov/pascaltz

License:
  GNU Library General Public License (LGPL) with a special exception.
  Read accompanying README and COPYING files for more details.

Authors:
  2016 - Denis Kozlov
*******************************************************************************}

{$mode objfpc}{$H+}

uses
  SysUtils,
  consoletestrunner,
  uPascalTZ_Tests;

const
  PauseOption = 'pause';
  RootDirOption = 'root';

type
  TApplication = class(TTestRunner)
  protected
    procedure AppendLongOpts; override;
    procedure WriteCustomHelp; override;
  end;

procedure TApplication.AppendLongOpts;
begin
  inherited AppendLongOpts;
  // Avoids a warning about invalid options in TCustomApplication.CheckOptions
  LongOpts.Append(PauseOption);
  LongOpts.Append(RootDirOption + ':');
end;

procedure TApplication.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  WriteLn;
  WriteLn(Format('  --%:-12s  %s', [PauseOption, 'Pause at the end before exiting.']));
  WriteLn(Format('  --%:-12s  %s', [RootDirOption + '=<dir>', 'Root directory for tzdata and vectors.']));
end;

var
  Application: TApplication;
  PauseOnExit: Boolean;
  RootDir: String;

begin
  Application := TApplication.Create(nil);
  Application.StopOnException := True;
  Application.Initialize;

  PauseOnExit := Application.HasOption(PauseOption);
  RootDir := Application.GetOptionValue(RootDirOption);
  TTZTestSetup.DatabaseDir := RootDir + 'tzdata';
  TTZTestSetup.VectorsDir := RootDir + 'vectors';
  TTZTestSetup.VectorFileMask := '*.txt';

  try
    Application.Run;
  finally
    Application.Free;
    if PauseOnExit then
    begin
      WriteLn('Press enter key to exit...');
      ReadLn;
    end;
  end;
end.
