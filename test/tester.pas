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
  OptionPause = 'pause';
  OptionDatabaseDir = 'tzdata';
  OptionVectorsDir = 'vectors';

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
  LongOpts.Append(OptionPause);
  LongOpts.Append(OptionDatabaseDir + ':');
  LongOpts.Append(OptionVectorsDir + ':');
end;

procedure TApplication.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  WriteLn;
  WriteLn(Format('  --%:-16s  %s', [OptionPause, 'Pause at the end, just before exiting.']));
  WriteLn(Format('  --%:-16s  %s', [OptionDatabaseDir + '=<dir>', 'Directory for tzdata.']));
  WriteLn(Format('  --%:-16s  %s', [OptionVectorsDir + '=<dir>', 'Directory for test vectors.']));
end;

var
  Application: TApplication;
  PauseOnExit: Boolean;

begin
  Application := TApplication.Create(nil);
  Application.StopOnException := True;
  Application.Initialize;

  PauseOnExit := Application.HasOption(OptionPause);
  TTZTestSetup.DatabaseDir := Application.GetOptionValue(OptionDatabaseDir);
  TTZTestSetup.VectorsDir := Application.GetOptionValue(OptionVectorsDir);
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
