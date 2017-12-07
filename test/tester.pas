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
  fpcunit,
  consoletestrunner,
  uPascalTZ_Tests;

const
  OptionPause = 'pause';
  OptionDatabaseDir = 'tzdata';
  OptionVectorsDir = 'vectors';

const
  DefaultVectorFileMask = '*.txt';

type
  TApplication = class(TTestRunner)
  public
    PauseOnExit: Boolean;
    PrintInfo: Boolean;
    TestsExecuted: Boolean;
  protected
    procedure AppendLongOpts; override;
    procedure WriteCustomHelp; override;
    procedure ParseOptions; override;
    procedure DoRun; override;
    procedure DoTestRun(ATest: TTest); override;
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

procedure TApplication.ParseOptions;
begin
  inherited ParseOptions;

  PrintInfo := Self.FormatParam in [fPlain, fPlainNoTiming];
  PauseOnExit := Self.HasOption(OptionPause);

  // Setup paths
  TTZTestSetup.DatabaseDir := Self.GetOptionValue(OptionDatabaseDir);
  TTZTestSetup.VectorsDir := Self.GetOptionValue(OptionVectorsDir);
  TTZTestSetup.VectorFileMask := DefaultVectorFileMask;

  // Print parameters
  if PrintInfo then
  begin
    WriteLn('Timezone database directory: ', TTZTestSetup.DatabaseDir);
    WriteLn('Test vectors directory: ', TTZTestSetup.VectorsDir);
    WriteLn('Test vectors file mask: ', TTZTestSetup.VectorFileMask);
    WriteLn;
  end;
end;

procedure TApplication.DoRun;
var
  LoadedFile: String;
begin
  // Run tests
  inherited DoRun;

  // Print loaded test vector count and files
  if PrintInfo and TestsExecuted then
  begin
    if TTZTestSetup.LoadedVectorCount > 0 then
      WriteLn(Format('Loaded %d test vectors.', [TTZTestSetup.LoadedVectorCount]))
    else
      WriteLn('No test vectors loaded.');
    if TTZTestSetup.LoadedVectorFiles.Count > 0 then
    begin
      WriteLn(Format('Loaded %d test vector files:', [TTZTestSetup.LoadedVectorFiles.Count]));
      for LoadedFile in TTZTestSetup.LoadedVectorFiles do
        WriteLn('  ', LoadedFile);
    end
    else
      WriteLn('No test vector files loaded.');
  end;

  // Pause on exit
  if PauseOnExit then
  begin
    if PrintInfo then
    begin
      WriteLn;
      WriteLn('Press enter key to exit...');
    end;
    ReadLn;
  end;
end;

procedure TApplication.DoTestRun(ATest: TTest);
begin
  TestsExecuted := True;
  inherited DoTestRun(ATest);
end;

var
  Application: TApplication;

begin
  Application := TApplication.Create(nil);
  Application.Initialize;
  Application.StopOnException := True;
  try
    Application.Run;
  finally
    Application.Free;
  end;
end.

