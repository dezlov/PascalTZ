unit uPascalTZ_Tests;

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

interface

uses
  Classes, SysUtils,
  fpcunit, testdecorator, testregistry,
  uPascalTZ, uPascalTZ_Types, uPascalTZ_Tools, uPascalTZ_Vectors;

type
  TTZTestSetup = class(TTestSetup)
  public class var
    TZ: TPascalTZ;
    DatabaseDir: String;
    VectorsDir: String;
    VectorFileMask: String;
    LoadedVectorFiles: TStringList;
    LoadedVectorCount: Int64;
  protected
    procedure OneTimeSetup; override;
    procedure OneTimeTearDown; override;
  end;

  TTZTestCaseVectors = class(TTestCase)
  private var
    TZ: TPascalTZ;
    Vectors: TTZTestVectorRepository;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestVectors;
  end;

  TTZTestCaseUtils = class(TTestCase)
  private
    procedure CheckMacroWeekDay(const DateIn, DateOut: String; WeekDay: TTZWeekDay; FirstOrLast: Boolean);
    procedure CheckMacroSolver(const DateIn, DateOut: TDateTime; const MacroCondition: String);
    procedure CheckMacroSolver(const DateIn, DateOut: String; const MacroCondition: String);
    procedure CheckFixTime(const T1, T2: TTZDateTime);
    function TryStrictTimeToSeconds(const ATime: String): Boolean;
  published
    procedure TestMacroWeekDay;
    procedure TestMacroSolver;
    procedure TestFixTime;
    procedure TestCompareDates;
    procedure TestWeekDay;
    procedure TestTimeToSeconds;
    procedure TestInvalidTimeToSeconds;
  end;

const
  TZ_DEFAULT_DATABASE_DIR = 'tzdata';
  TZ_DEFAULT_VECTORS_DIR = 'vectors';
  TZ_DEFAULT_VECTOR_FILE_MASK = '*.*';

implementation

const
  NL = LineEnding;

procedure TTZTestSetup.OneTimeSetup;
begin
  if Assigned(TZ) then
    FreeAndNil(TZ);
  if not DirectoryExists(DatabaseDir) then
    Fail(Format('Database directory "%s" does not exist. Check test setup.', [DatabaseDir]));
  if not DirectoryExists(VectorsDir) then
    Fail(Format('Vectors directory "%s" does not exist. Check test setup.', [VectorsDir]));
  TZ := TPascalTZ.Create;
  TZ.ParseDatabaseFromDirectory(DatabaseDir);
  if TZ.CountZones = 0 then
    Fail(Format('No zones loaded, check database direcory "%s".', [DatabaseDir]));
  if TZ.CountRules = 0 then
    Fail(Format('No rules loaded, check database direcory "%s".', [DatabaseDir]));
end;

procedure TTZTestSetup.OneTimeTearDown;
begin
  if Assigned(TZ) then
    FreeAndNil(TZ);
end;

procedure TTZTestCaseVectors.SetUp;
begin
  TZ := TTZTestSetup.TZ;
  if not Assigned(TZ) then
    Fail('Timezone database object is not assigned. Check test setup.');
  Vectors := TTZTestVectorRepository.Create;
  Vectors.LoadFromDirectory(TTZTestSetup.VectorsDir, TTZTestSetup.VectorFileMask);
  TTZTestSetup.LoadedVectorFiles.AddStrings(Vectors.LoadedFiles);
  TTZTestSetup.LoadedVectorCount += Vectors.Count;
  if Vectors.Count = 0 then
    Fail(Format('No test vectors loaded, check vectors direcory "%s".', [TTZTestSetup.VectorsDir]));
end;

procedure TTZTestCaseVectors.TearDown;
begin
  TZ := nil;
  if Assigned(Vectors) then
    FreeAndNil(Vectors);
end;

procedure TTZTestCaseVectors.TestVectors;
var
  I: Integer;
  Report: String;
begin
  for I := 0 to Vectors.Count - 1 do
  begin
    if not Vectors[I].Test(TZ, Report, trlFailed) then
      Fail('Timezone vector failed.' + LineEnding + Report);
  end;
end;

procedure TTZTestCaseUtils.CheckMacroWeekDay(const DateIn, DateOut: String;
  WeekDay: TTZWeekDay; FirstOrLast: Boolean);
var
  ActualDateOut: String;
begin
  if FirstOrLast then
  begin
    ActualDateOut := TZFormatDateTime(TZDateToPascalDate(MacroFirstWeekDay(
      PascalDateToTZDate(TZParseDateTime(DateIn)), WeekDay)));
  end
  else
  begin
    ActualDateOut := TZFormatDateTime(TZDateToPascalDate(MacroLastWeekDay(
      PascalDateToTZDate(TZParseDateTime(DateIn)), WeekDay)));
  end;
  if DateOut <> ActualDateOut then
  begin
    Fail('MacroWeekDay failed.' + NL +
      'INPUT DATE:      ' + DateIn + NL +
      'WEEKEDAY:        ' + WeekDayToString(WeekDay) + NL +
      'FIRST/LAST:      ' + BoolToStr(FirstOrLast, 'First', 'Last') + NL +
      'OUTPUT EXPECTED: ' + DateOut + NL +
      'OUTPUT ACTUAL:   ' + ActualDateOut);
  end;
end;

procedure TTZTestCaseUtils.TestMacroWeekDay;
begin
  CheckMacroWeekDay('2019-01-01 00:00:00', '2019-01-01 00:00:00', eTZTuesday, True);
  CheckMacroWeekDay('2019-01-01 00:00:00', '2019-01-01 00:00:00', eTZTuesday, False);

  CheckMacroWeekDay('2019-01-01 00:00:00', '2019-01-07 00:00:00', eTZMonday, True);
  CheckMacroWeekDay('2019-01-01 00:00:00', '2018-12-31 00:00:00', eTZMonday, False);

  CheckMacroWeekDay('2019-03-01 00:00:00', '2019-03-03 00:00:00', eTZSunday, True);
  CheckMacroWeekDay('2019-03-01 00:00:00', '2019-02-24 00:00:00', eTZSunday, False);

  CheckMacroWeekDay('1952-10-28 00:00:00', '1952-11-02 00:00:00', eTZSunday, True);
  CheckMacroWeekDay('1952-10-28 00:00:00', '1952-10-26 00:00:00', eTZSunday, False);
end;

procedure TTZTestCaseUtils.CheckMacroSolver(const DateIn, DateOut: String;
  const MacroCondition: String);
begin
  CheckMacroSolver(TZParseDateTime(DateIn), TZParseDateTime(DateOut), MacroCondition);
end;

procedure TTZTestCaseUtils.CheckMacroSolver(const DateIn, DateOut: TDateTime;
  const MacroCondition: String);
var
  TZDT: TTZDateTime;
  DateOutTest: TDateTime;
begin
  TZDT := PascalDateToTZDate(DateIn);
  MacroSolver(TZDT, MacroCondition);
  DateOutTest := TZDateToPascalDate(TZDT);
  if DateOut <> DateOutTest then
  begin
    Fail('MacroSolver failed.' + NL +
      'INPUT:           ' + TZFormatDateTime(DateIn) + NL +
      'MACRO:           ' + MacroCondition + NL +
      'OUTPUT EXPECTED: ' + TZFormatDateTime(DateOut) + NL +
      'OUTPUT ACTUAL:   ' + TZFormatDateTime(DateOutTest));
  end;
end;

procedure TTZTestCaseUtils.TestMacroSolver;
begin
  CheckMacroSolver(
    '2015-10-01 00:00:00',
    '2015-10-25 00:00:00',
    'lastSun');
  CheckMacroSolver(
    '2015-10-01 01:00:00',
    '2015-10-04 01:00:00',
    'firstSun');
  CheckMacroSolver(
    '2015-10-01 02:00:00',
    '2015-10-11 02:00:00',
    'Sun>=5');
  CheckMacroSolver(
    '2015-10-01 15:00:00',
    '2015-10-12 15:00:00',
    'Mon>=12');
  CheckMacroSolver(
    '2015-10-01 15:00:00',
    '2015-10-21 15:00:00',
    'Wed>=15');
  CheckMacroSolver(
    '2015-10-01 15:00:00',
    '2015-10-04 15:00:00',
    'Sun<=5');
  CheckMacroSolver(
    '2015-10-01 15:00:00',
    '2015-10-12 15:00:00',
    'Mon<=12');
  CheckMacroSolver(
    '2015-10-01 16:00:00',
    '2015-10-14 16:00:00',
    'Wed<=20');
  CheckMacroSolver(
    '1952-10-01 00:00:00',
    '1952-11-02 00:00:00',
    'Sun>=28');
end;

procedure TTZTestCaseUtils.CheckFixTime(const T1, T2: TTZDateTime);
var
  TT: TTZDateTime;
begin
  TT := T1;
  FixUpTime(TT);
  if TT <> T2 then
  begin
    Fail('FixUpTime failed.' + LineEnding +
      DateTimeToStr(TT) + ' <> ' + DateTimeToStr(T2));
  end;
end;

procedure TTZTestCaseUtils.TestFixTime;
begin
  CheckFixTime(
    MakeTZDate(1993, 3, 28, -3600*48),
    MakeTZDate(1993, 3, 26, 0));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, -3600*25),
    MakeTZDate(1993, 3, 26, 3600*23));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, -3600*24 - 1),
    MakeTZDate(1993, 3, 26, 3600*24 - 1));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, -3600*24),
    MakeTZDate(1993, 3, 27, 0));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, -3600*24 + 1),
    MakeTZDate(1993, 3, 27, 1));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, -3600*12),
    MakeTZDate(1993, 3, 27, 3600*12));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, 0),
    MakeTZDate(1993, 3, 28, 0));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, 3600*24 - 1),
    MakeTZDate(1993, 3, 28, 3600*24 - 1));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, 3600*24),
    MakeTZDate(1993, 3, 29, 0));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, 3600*24 + 1),
    MakeTZDate(1993, 3, 29, 1));
  CheckFixTime(
    MakeTZDate(1993, 3, 28, 3600*48),
    MakeTZDate(1993, 3, 30, 0));
end;

procedure TTZTestCaseUtils.TestCompareDates;
begin
  CheckEquals(0, CompareDates(
    MakeTZDate(2015, 1, 1, 0),
    MakeTZDate(2015, 1, 1, 0)));
  CheckEquals(-1, CompareDates(
    MakeTZDate(2015, 1, 1, 0),
    MakeTZDate(2016, 1, 1, 0)));
  CheckEquals(1, CompareDates(
    MakeTZDate(2015, 2, 1, 0),
    MakeTZDate(2015, 1, 1, 0)));
  CheckEquals(1, CompareDates(
    MakeTZDate(2015, 1, 2, 0),
    MakeTZDate(2015, 1, 1, 0)));
  CheckEquals(0, CompareDates(
    MakeTZDate(2015, 1, 1, 60),
    MakeTZDate(2015, 1, 1, 60)));
  CheckTrue(
    MakeTZDate(2015, 1, 1, 0) >
    MakeTZDate(2014, 1, 1, 0));
  CheckTrue(
    MakeTZDate(2015, 12, 30, 0) <
    MakeTZDate(2015, 12, 31, 0));
  CheckTrue(
    MakeTZDate(2015, 12, 31, 3000) =
    MakeTZDate(2015, 12, 31, 3000));
end;

procedure TTZTestCaseUtils.TestWeekDay;
begin
  CheckTrue(eTZThursday = WeekDayOf(MakeTZDate(2015, 12, 31, 0)));
  CheckTrue(eTZFriday = WeekDayOf(MakeTZDate(2016, 1, 1, 0)));
  CheckTrue(eTZSaturday = WeekDayOf(MakeTZDate(2016, 7, 2, 0)));
  CheckTrue(eTZSunday = WeekDayOf(MakeTZDate(2016, 7, 3, 0)));
end;

procedure TTZTestCaseUtils.TestTimeToSeconds;
const
  SecsPerHour = MinsPerHour * SecsPerMin;
  SecsPerDay = HoursPerDay * SecsPerHour;
begin
  CheckEquals(TimeToSeconds('1'), SecsPerHour);
  CheckEquals(TimeToSeconds('-1'), -SecsPerHour);
  CheckEquals(TimeToSeconds('1:05'), SecsPerHour + 5 * SecsPerMin);
  CheckEquals(TimeToSeconds('-1:05'), -(SecsPerHour + 5 * SecsPerMin));
  CheckEquals(TimeToSeconds('00:00:30'), 30);
  CheckEquals(TimeToSeconds('-00:00:30'), -30);
  CheckEquals(TimeToSeconds('00:30:00'), 30 * SecsPerMin);
  CheckEquals(TimeToSeconds('01:00:00'), SecsPerHour);
  CheckEquals(TimeToSeconds('23:59:59'), SecsPerDay - 1);
  CheckEquals(TimeToSeconds('24:00:00'), SecsPerDay);
  CheckEquals(TimeToSeconds('-24:00:00'), -SecsPerDay);
  CheckEquals(TimeToSeconds('24:59:59'), SecsPerDay + SecsPerHour - 1);
  CheckEquals(TimeToSeconds('25:00:00'), SecsPerDay + SecsPerHour);
  CheckEquals(TimeToSeconds('-25:00:00'), -SecsPerDay - SecsPerHour);
end;

procedure TTZTestCaseUtils.TestInvalidTimeToSeconds;
begin
  CheckFalse(TryStrictTimeToSeconds(''));
  CheckFalse(TryStrictTimeToSeconds('test'));
  CheckFalse(TryStrictTimeToSeconds('100'));
  CheckFalse(TryStrictTimeToSeconds('59'));
  CheckFalse(TryStrictTimeToSeconds('59:59'));
  CheckFalse(TryStrictTimeToSeconds('25:00:01'));
  CheckFalse(TryStrictTimeToSeconds('25:59:59'));
  CheckFalse(TryStrictTimeToSeconds('26:00:00'));
  CheckFalse(TryStrictTimeToSeconds('-26:00:00'));
end;

function TTZTestCaseUtils.TryStrictTimeToSeconds(const ATime: String): Boolean;
begin
  try
    TimeToSeconds(ATime, True);
    Result := True;
  except on E: TTZException do
    Result := False;
  end;
end;

initialization
  TTZTestSetup.DatabaseDir := TZ_DEFAULT_DATABASE_DIR;
  TTZTestSetup.VectorsDir := TZ_DEFAULT_VECTORS_DIR;
  TTZTestSetup.VectorFileMask := TZ_DEFAULT_VECTOR_FILE_MASK;
  TTZTestSetup.LoadedVectorFiles := TStringList.Create;
  TTZTestSetup.LoadedVectorCount := 0;
  RegisterTestDecorator(TTZTestSetup, TTZTestCaseVectors);
  RegisterTest(TTZTestCaseUtils);

finalization
  FreeAndNil(TTZTestSetup.LoadedVectorFiles);

end.

