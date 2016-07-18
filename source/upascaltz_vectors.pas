unit uPascalTZ_Vectors;

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
  Classes, SysUtils, contnrs,
  uPascalTZ, uPascalTZ_Tools, uPascalTZ_Types;

type
  TTZTestReportLevel = (trlNone, trlFailed, trlAll);

  TTZTestVector = class
  public
    Universal: TDateTime;
    LocalFromUniversal: TDateTime;
    UniversalFromLocal: TDateTime;
    Timezone: String;
  public
    function LoadFromLine(const Line: String): Boolean;
    procedure Setup(const UniversalDate, LocalFromUniversalDate,
      UniversalFromLocalDate, ATimezone: String); overload;
    procedure Setup(const UniversalDate, LocalFromUniversalDate,
      UniversalFromLocalDate: TDateTime; const ATimezone: String); overload;
    function Test(TZ: TPascalTZ; out Report: String; ReportLevel: TTZTestReportLevel): Boolean;
    function Test(TZ: TPascalTZ): Boolean;
  end;

  TTZTestVectorRepository = class
  strict private
    FTestVectors: TFPObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TTZTestVector;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const TestVector: TTZTestVector): Integer;
    procedure LoadFromLines(const Lines: TStrings);
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromFiles(const PathPrefix: String; const FileNames: Array of String);
    procedure LoadFromDirectory(const DirPath: String; const FileMask: String = '*.*');
  public
    property Item[Index: Integer]: TTZTestVector read GetItem; default;
    property Count: Integer read GetCount;
  end;

function TZParseDateTime(const Value: String): TDateTime; inline;
function TZFormatDateTime(const Value: TDateTime): String; inline;

const
  TZ_TEST_VECTOR_FORMAT = 'YYYY-MM-DD HH:MM:SS';
  TZ_TEST_VECTOR_VALUE_DELIM = ',';
  TZ_TEST_VECTOR_LINE_ESCAPE_CHARS = ['/', '#', ';'];


implementation

uses
  StrUtils, DateUtils;

function TZParseDateTime(const Value: String): TDateTime; inline;
begin
  Result := ScanDateTime(TZ_TEST_VECTOR_FORMAT, Value);
end;

function TZFormatDateTime(const Value: TDateTime): String; inline;
begin
  Result := FormatDateTime(TZ_TEST_VECTOR_FORMAT, Value);
end;

function SameDateTimeAsSign(const DT1, DT2: TDateTime): String; inline;
begin
  if SameDateTime(DT1, DT2) then
    Result := '=='
  else
    Result := '<>';
end;

procedure SplitString(const Input, Delimiter: String; Parts: TStrings);
var
  I, Start, L: Integer;
begin
  Parts.Clear;
  Start := 1;
  L := Length(Delimiter);
  I := Pos(Delimiter, Input);
  while I > 0 do
  begin
    Parts.Add(Copy(Input, Start, I-Start));
    Start := I+L;
    I := PosEx(Delimiter, Input, Start);
  end;
  Parts.Add(Copy(Input, Start, Length(Input)));
end;

function NeedTestReport(TestResult: Boolean; ReportLevel: TTZTestReportLevel): Boolean;
begin
  Result := (ReportLevel = trlAll) or ((ReportLevel = trlFailed) and not TestResult);
end;

{$REGION 'TTZTestVector'}

function TTZTestVector.LoadFromLine(const Line: String): Boolean;
var
  TrimmedLine: String;
  Parts: TStringList;
begin
  Result := False;
  TrimmedLine := Trim(Line);
  // Skip empty lines
  if Length(TrimmedLine) > 0 then
  begin
    // Skip escaped lines
    if not (TrimmedLine[1] in TZ_TEST_VECTOR_LINE_ESCAPE_CHARS) then
    begin
      Parts := TStringList.Create;
      try
        // Split line into parts
        SplitString(TrimmedLine, TZ_TEST_VECTOR_VALUE_DELIM, Parts);
        Result := Parts.Count >= 3;
        if not Result then
          raise Exception.Create('Incorrect number of delimiters in test vector line: ' + Line);
        if Result then
        begin
          while Parts.Count < 4 do
            Parts.Add('');
          try
            Setup(Trim(Parts[0]), Trim(Parts[1]), Trim(Parts[3]), Trim(Parts[2]));
          except on E: Exception do
            raise Exception.Create('Invalid format of test vector line: ' + Line);
          end;
        end;
      finally
        Parts.Free;
      end;
    end;
  end;
end;

procedure TTZTestVector.Setup(const UniversalDate, LocalFromUniversalDate,
  UniversalFromLocalDate, ATimezone: String);
begin
  Self.Universal := TZParseDateTime(UniversalDate);
  Self.LocalFromUniversal := TZParseDateTime(LocalFromUniversalDate);
  if Length(UniversalFromLocalDate) > 0 then
    Self.UniversalFromLocal := TZParseDateTime(UniversalFromLocalDate)
  else
    Self.UniversalFromLocal := Self.Universal;
  Self.Timezone := ATimezone;
end;

procedure TTZTestVector.Setup(const UniversalDate, LocalFromUniversalDate,
  UniversalFromLocalDate: TDateTime; const ATimezone: String);
begin
  Self.Universal := UniversalDate;
  Self.LocalFromUniversal := LocalFromUniversalDate;
  Self.UniversalFromLocal := UniversalFromLocalDate;
  Self.Timezone := ATimezone;
end;

function TTZTestVector.Test(TZ: TPascalTZ; out Report: String; ReportLevel: TTZTestReportLevel): Boolean;
const
  NL = LineEnding;
var
  TestUniversalFromLocal, TestLocalFromUniversal, TestLocalFromUniversalFromLocal: TDateTime;
  ATimeZoneSubFix: String;
begin
  TestUniversalFromLocal          {UTC<-Local} := TZ.LocalTimeToGMT({T2}Self.LocalFromUniversal, Self.Timezone);
  TestLocalFromUniversal          {Local<-UTC} := TZ.GMTToLocalTime({T1}Self.Universal, Self.Timezone, ATimeZoneSubFix);
  TestLocalFromUniversalFromLocal {Local<-UTC} := TZ.GMTToLocalTime({T3}Self.UniversalFromLocal, Self.Timezone);

  Result :=
    SameDateTime({T2}Self.LocalFromUniversal, TestLocalFromUniversal) and
    SameDateTime({T3}Self.UniversalFromLocal, TestUniversalFromLocal) and
    SameDateTime({T2}Self.LocalFromUniversal, TestLocalFromUniversalFromLocal);

  if NeedTestReport(Result, ReportLevel) then
  begin
    Report :=
      Self.Timezone + ' (' + ATimeZoneSubFix + ') ' + BoolToStr(Result, 'OK', 'FAILED') + NL +
      '  UTC:           ' + TZFormatDateTime(Self.Universal) + NL +
      '  UTC->LOC:      ' + TZFormatDateTime(Self.LocalFromUniversal) +
        ' ' + SameDateTimeAsSign(Self.LocalFromUniversal, TestLocalFromUniversal) +
        ' ' + TZFormatDateTime(TestLocalFromUniversal) + NL +
      '  LOC->UTC:      ' + TZFormatDateTime(Self.UniversalFromLocal) +
        ' ' + SameDateTimeAsSign(Self.UniversalFromLocal, TestUniversalFromLocal) +
        ' ' + TZFormatDateTime(TestUniversalFromLocal) + NL +
      '  LOC->UTC->LOC: ' + TZFormatDateTime(Self.LocalFromUniversal) +
        ' ' + SameDateTimeAsSign(Self.LocalFromUniversal, TestLocalFromUniversalFromLocal) +
        ' ' + TZFormatDateTime(TestLocalFromUniversalFromLocal);
  end;
end;

function TTZTestVector.Test(TZ: TPascalTZ): Boolean;
var
  DummyReport: String;
begin
  Result := Test(TZ, DummyReport, trlNone);
end;

{$ENDREGION}

{$REGION 'TTZTestVectorRepository'}

constructor TTZTestVectorRepository.Create;
begin
  FTestVectors := TFPObjectList.Create(True); // FreeObjects = True!
end;

destructor TTZTestVectorRepository.Destroy;
begin
  FreeAndNil(FTestVectors);
  inherited Destroy;
end;

procedure TTZTestVectorRepository.Clear;
begin
  FTestVectors.Clear;
end;

function TTZTestVectorRepository.GetCount: Integer;
begin
  Result := FTestVectors.Count;
end;

function TTZTestVectorRepository.GetItem(Index: Integer): TTZTestVector;
begin
  Result := TTZTestVector(FTestVectors[Index]);
end;

function TTZTestVectorRepository.Add(const TestVector: TTZTestVector): Integer;
begin
  Result := FTestVectors.Add(TestVector);
end;

procedure TTZTestVectorRepository.LoadFromLines(const Lines: TStrings);
var
  Line: String;
  Loaded: Boolean;
  Vector: TTZTestVector;
begin
  for Line in Lines do
  begin
    Loaded := False;
    Vector := TTZTestVector.Create;
    try
      Loaded := Vector.LoadFromLine(Line);
    except
      FreeAndNil(Vector);
      raise;
    end;
    if Loaded then
      Add(Vector)
    else
      FreeAndNil(Vector);
  end;
end;

procedure TTZTestVectorRepository.LoadFromFile(const FileName: String);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    LoadFromLines(Lines);
  finally
    Lines.Free;
  end;
end;

procedure TTZTestVectorRepository.LoadFromFiles(const PathPrefix: String;
  const FileNames: array of String);
var
  FileName: String;
begin
  for FileName in FileNames do
    LoadFromFile(PathPrefix + FileName);
end;

procedure TTZTestVectorRepository.LoadFromDirectory(const DirPath: String;
  const FileMask: String = '*.*');
var
  FindDirPath: String;
  FindResult: Integer;
  FileRec: TSearchRec;
begin
  FindDirPath := IncludeTrailingPathDelimiter(DirPath);
  FindResult := FindFirst(FindDirPath + FileMask, faAnyFile and not faDirectory, FileRec);
  try
    if FindResult = 0 then
    begin
      repeat
        LoadFromFile(FindDirPath + FileRec.Name);
        FindResult := FindNext(FileRec);
      until FindResult <> 0;
    end;
  finally
    FindClose(FileRec);
  end;
end;

{$ENDREGION}


end.

