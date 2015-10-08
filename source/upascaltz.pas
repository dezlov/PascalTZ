unit upascaltz;

{$mode objfpc}{$H+}

{
  File: upascaltz.pas

  This unit is designed to convert local times across different time zones
  at any given time in the past and probably in the future, this can not be
  ensured as time change rules could change in the future. For this purpose
  it uses the time rules database available at
  http://www.twinsun.com/tz/tz-link.htm

  License: The same as freepascal packages (basically LGPL)

  The database is presented in different files so you can use one of them
  using "ParseDatabaseFromFile" or concatenate the interested ones in a single
  file or stream. Once the database is loaded calling "ParseDatabaseFromXXXX"
  deletes the in memory parsed data.

  ProcessedLines
    Amount of read lines in database.

  DetectInvalidLocalTimes [True|False] (default true)
    When converting it will check if the given time is impossible (does not
    exists). This happends, in example, when a local time changes from 2:00
    to 3:00, if the time to be converted is 2:01 at the change date, that
    time does not exists as it is not linear.

  GetTimeZoneNames(const AZones: TStringList; const AOnlyGeoZones: Boolean=true);
    Returns a TStringList with the time zones available in the database. This
    names must be used for local times to perform conversions. It is not a
    country list, as many countries have several time zones. AOnlyGeoZones
    removes from the list the usual "GMT, BST, PST" from the list.

  GMTToLocalTime
    Converts a GMT/UTC/Zulu time to a time zone (AToZone). ATimeZoneSubFix
    returns the subfix for that zone like "GMT, BST, ...".

  LocalTimeToGMT
    Converts a local time at time zone "AFromZone" to GMT/UTC/Zulu time.

  TimeZoneToTimeZone
    Converts time across zones. Basically this performs a local time to
    GMT and GTM to the new local time.

  ParseDatabaseFromFile(const AFileName: String): Boolean;
    Reads the database from a file.

  ParseDatabaseFromStream(const AStream: TStream): Boolean;
    Reads the database from a stream.

  2009 - José Mejuto
}

interface

uses
  Classes,SysUtils,upascaltz_types,usorters;

type
  { TExceptionTZ }

  TTZException = class(Exception);

  { TPascalTZ }

  TPascalTZ = class(TObject)
  private
    ParseStatusTAG: AsciiString;
    ParseStatusPreviousZone: AsciiString;
    function FindZoneForDate(const ZoneIndexStart: integer;const ADateTime: TTZDateTime): integer;
    function FindZoneName(const AZone: ansistring): integer;
    function ParseUntilFields(const AIterator: TTZLineIterate; out AGMTTime: Boolean): TTZDateTime;
    procedure IsGregorianLeapException(const ADate: TTZDateTime);
    function IsGregorianLeap(const ADate: TTZDateTime): Boolean;
    function IsBeforeGregorianLeap(const ADate: TTZDateTime): Boolean;
    function fSortCompareRule(const AIndex,BIndex: SizeInt): TSortCompareResult;
    procedure fSortSwapRule(const AIndex,BIndex: SizeInt);
    procedure SortRules();
    procedure FixUpTime(var ADate: TTZDateTime);
    procedure DateTimeToTime(const ADate: TTZDateTime; out AHour,AMinute,ASecond: BYTE);
    function DateTimeToStr(const ADate: TTZDateTime): ansistring;
  protected
    FDetectInvalidLocalTimes: Boolean;
    FLineCounter: integer;
    FCurrentLine: AsciiString;
    FRules: array of TTZRules;
    FZones: array of TTzZone;
    function CompareDates(const ADate,BDate: TTZDateTime): integer;
    function MonthNumberFromShortName(const AMonth: AsciiString): TTZMonth;
    function IsLeapYear(const AYear: integer): Boolean;
    function DayNameToNumber(const ADayName: AsciiString): TTZWeekDay;
    function WeekDayToString(const AWeekDay: TTZWeekDay): AsciiString;
    function MacroFirstWeekDay(const ADate: TTZDateTime;const AWeekDay: TTZWeekDay; const ASinceMonthDay: TTZDay=1): TTZDay;
    function MacroLastWeekDay(const ADate: TTZDateTime;const AWeekDay: TTZWeekDay; const ASinceMonthDay: TTZDay=31): TTZDay;
    function TimeToSeconds(const ATime: AsciiString): integer;
    Function LookupRuleNonIndexed(const AName: AsciiString): Integer;
    function ParseLine(const ALine: AsciiString;const AParseSequence: TParseSequence): boolean;
    function ParseZone(const AIterator: TTZLineIterate; const AZone: AsciiString): boolean;
    function ParseRule(const AIterator: TTZLineIterate): Boolean;
    function IncDays(const ADate: TTZDateTime; const ADays: integer): TTZDateTime;
    function TZDateToPascalDate(const ADate: TTZDateTime): TDateTime;
    function PascalDateToTZDate(const ADate: TDateTime): TTZDateTime;
    function WeekDayOf(const ADate: TTZDateTime): TTZWeekDay;
    procedure MacroSolver(var ADate: TTZDateTime; const ADayString: AsciiString);
    function LocalTimeToGMT(const ADateTime: TTZDateTime; const AFromZone: ansistring): TTZDateTime;
    function GMTToLocalTime(const ADateTime: TTZDateTime; const AToZone: ansistring;out ATimeZoneName: ansistring): TTZDateTime;
    function ElapsedDaysSinceADToDate(const AElapsedDays: integer): TTZDateTime;
    function ElapsedDaysSinceAD(const ADate: TTZDateTime): integer;
  public
    property ProcessedLines: integer read FLineCounter;
    property DetectInvalidLocalTimes: Boolean read FDetectInvalidLocalTimes write FDetectInvalidLocalTimes;
    procedure GetTimeZoneNames(const AZones: TStringList; const AOnlyGeoZones: Boolean=true);
    function GMTToLocalTime(const ADateTime: TDateTime; const AToZone: ansistring;out ATimeZoneSubFix: ansistring): TDateTime;
    function LocalTimeToGMT(const ADateTime: TDateTime; const AFromZone: ansistring): TDateTime;
    function TimeZoneToTimeZone(const ADateTime: TDateTime; const AFromZone,AToZone: ansistring;out ATimeZoneSubFix: ansistring): TDateTime;
    function ParseDatabaseFromFile(const AFileName: String): Boolean;
    function ParseDatabaseFromStream(const AStream: TStream): Boolean;
    constructor Create();
  end;

implementation

uses RtlConsts,dateutils;

var
  TTZMonthDaysCount:
            array [TTZMonth] of TTZDay=(31,28,31,30,31,30,31,31,30,31,30,31);
  TTZMonthDaysLeapYearCount:
            array [TTZMonth] of TTZDay=(31,29,31,30,31,30,31,31,30,31,30,31);


{ TPascalTZ }

function TPascalTZ.ParseUntilFields(const AIterator: TTZLineIterate;
  out AGMTTime: Boolean): TTZDateTime;
var
  TmpWord: AsciiString;
  Letter: AsciiChar;
begin
  //Default Values...
  with Result do begin
    Year:=9999;
    Month:=1;
    Day:=1;
    SecsInDay:=0;
  end;
  AGMTTime:=true;
  TmpWord:=AIterator.GetNextWord;
  if TmpWord='' Then Exit;
  try
    Result.Year:=StrToInt(TmpWord);
  except
  On E: Exception do begin
          Raise TTZException.CreateFmt('Invalid date at line %d "%s"',[FLineCounter,FCurrentLine]);
        end;
  end;
  TmpWord:=AIterator.GetNextWord;
  if TmpWord='' Then Exit;
  Result.Month:=MonthNumberFromShortName(TmpWord);
  TmpWord:=AIterator.GetNextWord;
  if TmpWord='' then Exit;
  Result.Day:=StrToIntDef(TmpWord,0);
  if Result.Day=0 then begin
    //Not a number...
    //Try using macros...
    MacroSolver(Result,TmpWord);
  end;

  TmpWord:=AIterator.GetNextWord;
  if TmpWord='' then Exit;
  if not (TmpWord[Length(TmpWord)] in ['0'..'9']) then begin
    Letter:=TmpWord[Length(TmpWord)];
    if Letter='s' then begin
      AGMTTime:=false;
    end;
    SetLength(TmpWord,Length(TmpWord)-1);
  end;
  Result.SecsInDay:=TimeToSeconds(TmpWord);
end;

function TPascalTZ.FindZoneName(const AZone: ansistring): integer;
var
  ZoneIndex: integer;
  j: integer;
begin
  ZoneIndex:=-1;
  for j := 0 to High(FZones) do begin
    if FZones[j].Name=AZone then begin
      ZoneIndex:=j;
      Break;
    end;
  end;
  if ZoneIndex<0 then begin
    raise TTZException.CreateFmt('Zone not found [%s]', [AZone]);
  end;
  Result:=ZoneIndex;
end;

function TPascalTZ.FindZoneForDate(const ZoneIndexStart: integer;const ADateTime: TTZDateTime): integer;
var
  Found: Boolean;
  AZone: AsciiString;
  j: integer;
begin
  AZone:=FZones[ZoneIndexStart].Name;
  j:=ZoneIndexStart;
  Found:=false;
  while (j<=High(FZones)) and (FZones[j].Name=AZone) do begin
    if CompareDates(FZones[j].RuleValidUntil, ADateTime)=1 then begin
      Found:=true;
      break;
    end;
    inc(j);
  end;
  if not Found then begin
    raise TTZException.CreateFmt('No valid conversion rule for Zone [%s]', [AZone]);
  end;
  Result:=j;
end;

procedure TPascalTZ.IsGregorianLeapException(const ADate: TTZDateTime);
begin
  if IsGregorianLeap(ADate) Then begin
    Raise TTZException.CreateFmt('Gregorian Leap, date does not exists. [%.4d.%.2d.%.2d]',[ADate.Year,ADate.Month,ADate.Day])
  end;
end;

function TPascalTZ.IsGregorianLeap(const ADate: TTZDateTime): Boolean;
begin
  Result:=false;
  if (ADate.Year=1582) and (ADate.Month=10) then begin
    if (ADate.Day>4) and (ADate.Day<15) Then begin
      Result:=true;
    end;
  end;
end;

function TPascalTZ.IsBeforeGregorianLeap(const ADate: TTZDateTime): Boolean;
begin
  if ADate.Year<1582 then Exit(true);
  if ADate.Year>1582 then Exit(false);
  //Now Year is 1582 :)
  if ADate.Month<10 then Exit(true);
  if ADate.Month>10 then Exit(false);
  //Now year 1582 and month 10
  if ADate.Day<=4 Then Exit (true);
  Result:=false;
end;

function TPascalTZ.fSortCompareRule(const AIndex, BIndex: SizeInt
  ): TSortCompareResult;
begin
  if FRules[AIndex].Name>FRules[BIndex].Name Then begin
    Exit(eSortCompareBigger);
  end else if FRules[AIndex].Name<FRules[BIndex].Name Then begin
    Exit(eSortCompareLesser);
  end;
  if FRules[AIndex].FromYear>FRules[BIndex].FromYear then begin
    Exit(eSortCompareBigger);
  end else if FRules[AIndex].FromYear<FRules[BIndex].FromYear Then begin
    Exit(eSortCompareLesser);
  end;
  if FRules[AIndex].ToYear>FRules[BIndex].ToYear then begin
    Exit(eSortCompareBigger);
  end else if FRules[AIndex].ToYear<FRules[BIndex].ToYear Then begin
    Exit(eSortCompareLesser);
  end;
  if FRules[AIndex].InMonth>FRules[BIndex].InMonth then begin
    Exit(eSortCompareBigger);
  end else if FRules[AIndex].InMonth<FRules[BIndex].InMonth Then begin
    Exit(eSortCompareLesser);
  end;
  //This should not happend
//  Raise TTZException.CreateFmt('Invalid rule sorting',[]);
  Result:=eSortCompareEqual;
end;

procedure TPascalTZ.fSortSwapRule(const AIndex, BIndex: SizeInt);
var
  Temporal: TTZRules;
begin
  Temporal:=FRules[AIndex];
  FRules[AIndex]:=FRules[BIndex];
  FRules[BIndex]:=Temporal;
end;

procedure TPascalTZ.SortRules();
var
  Sorter: THeapSort;
begin
  //Sort the rules by name
  Sorter:=THeapSort.Create(Length(FRules));
  Sorter.OnCompareOfClass:=@fSortCompareRule;
  Sorter.OnSwapOfClass:=@fSortSwapRule;
  Sorter.Sort();
  Sorter.Free;
end;

procedure TPascalTZ.FixUpTime(var ADate: TTZDateTime);
var
  Days: integer;
begin
  if ADate.SecsInDay<0 then begin
    //Decrease some day(s)
    Days:=(ADate.SecsInDay*-1) div (3600*24);
    ADate:=IncDays(ADate,Days*-1);
    ADate.SecsInDay:=ADate.SecsInDay+Days*3600*24;
  end else if ADate.SecsInDay>=(3600*24) then begin
    //Increase some day(s)
    Days:=ADate.SecsInDay div (3600*24);
    ADate:=IncDays(ADate,Days);
    ADate.SecsInDay:=ADate.SecsInDay-Days*3600*24;
  end;
end;

procedure TPascalTZ.DateTimeToTime(const ADate: TTZDateTime; out AHour,
  AMinute, ASecond: BYTE);
var
  Temp: integer;
begin
  Temp:=ADate.SecsInDay;
  AHour:=ADate.SecsInDay div 3600;
  Dec(Temp,AHour*3600);
  AMinute:=Temp div 60;
  Dec(Temp,AMinute*60);
  ASecond:=Temp;
end;

function TPascalTZ.CompareDates(const ADate, BDate: TTZDateTime): integer;
begin
  if ADate.Year>BDate.Year then begin
    Exit(1);
  end else if ADate.Year<BDate.Year then begin
    Exit(-1);
  end;
  if ADate.Month>BDate.Month then begin
    Exit(1);
  end else if ADate.Month<BDate.Month then begin
    Exit(-1);
  end;
  if ADate.Day>BDate.Day then begin
    Exit(1);
  end else if ADate.Day<BDate.Day then begin
    Exit(-1);
  end;
  if ADate.SecsInDay>BDate.SecsInDay then begin
    Exit(1);
  end else if ADate.SecsInDay<BDate.SecsInDay then begin
    Exit(-1);
  end;
  Result:=0;
end;

function TPascalTZ.MonthNumberFromShortName(const AMonth: AsciiString
  ): TTZMonth;
begin
  if AMonth='Jan' then begin
    Result:=1;
  end else if AMonth='Feb' then begin
    Result:=2;
  end else if AMonth='Mar' then begin
    Result:=3;
  end else if AMonth='Apr' then begin
    Result:=4;
  end else if AMonth='May' then begin
    Result:=5;
  end else if AMonth='Jun' then begin
    Result:=6;
  end else if AMonth='Jul' then begin
    Result:=7;
  end else if AMonth='Aug' then begin
    Result:=8;
  end else if AMonth='Sep' then begin
    Result:=9;
  end else if AMonth='Oct' then begin
    Result:=10;
  end else if AMonth='Nov' then begin
    Result:=11;
  end else if AMonth='Dec' then begin
    Result:=12;
  end else begin
    Raise TTZException.CreateFmt('Invalid short month name "%s"',[AMonth]);
  end;
end;

function TPascalTZ.ElapsedDaysSinceAD(const ADate: TTZDateTime): integer;
var
  Elapsed: integer;
  j: integer;
  TempYear: integer;
begin
  IsGregorianLeapException(ADate);
  TempYear:=ADate.Year-1;
  if IsBeforeGregorianLeap(ADate) Then begin
    Elapsed:=(TempYear*365)+(TempYear div 4);
  end else begin
    Elapsed:=(TempYear*365)+(TempYear div 4)-((TempYear) div 100)+((TempYear) div 400);
    Elapsed:=Elapsed-10+15-3; //Gregorian elapsed days and false leap years
  end;
  //Here some maths will simplify it...
  if IsLeapYear(ADate.Year) then begin
    for j := 1 to ADate.Month-1 do begin
      Elapsed:=Elapsed+TTZMonthDaysLeapYearCount[j];
    end;
  end else begin
    for j := 1 to ADate.Month-1 do begin
      Elapsed:=Elapsed+TTZMonthDaysCount[j];
    end;
  end;
  inc(Elapsed,ADate.Day);
  Result:=Elapsed;
end;

function TPascalTZ.WeekDayOf(const ADate: TTZDateTime): TTZWeekDay;
var
  TempYear: integer;
  FirstDayOfYear: integer;
  j: integer;
  DaysAD: integer;
begin
  IsGregorianLeapException(ADate);
  TempYear:=ADate.Year-1;
  //Is date before Gregory Pope change ?

  if IsBeforeGregorianLeap(ADate) then begin
    FirstDayOfYear:= 6+TempYear+(TempYear div 4);
  end else begin
    FirstDayOfYear:= 1+TempYear+(TempYear div 4)-(TempYear div 100)+(TempYear div 400);
  end;
  DaysAD:=FirstDayOfYear;
  if IsLeapYear(ADate.Year) Then begin
    for j := 1 to ADate.Month-1 do begin
      inc(DaysAD,TTZMonthDaysLeapYearCount[j]);
    end;
  end else begin
    for j := 1 to ADate.Month-1 do begin
      inc(DaysAD,TTZMonthDaysCount[j]);
    end;
  end;
  inc(DaysAD,ADate.Day-1);
  Result:=TTZWeekDay((DaysAD mod 7)+1);
end;

function TPascalTZ.IsLeapYear(const AYear: integer): Boolean;
begin
  if AYear>1582 then begin
    if (AYear mod 400)=0 Then Exit(true); //All years every 400 years are leap years.
    if (AYear mod 100)=0 then Exit(false); //Centuries are not leap years (except previous % 400)
  end;
  if (AYear mod 4)=0 then exit(true); //Each 4 years a leap year comes in play.
  Result:=false;
end;

function TPascalTZ.DayNameToNumber(const ADayName: AsciiString): TTZWeekDay;
begin
  if ADayName='Sun' then Result:=eTZSunday
  else if ADayName='Mon' then Result:=eTZMonday
  else if ADayName='Tue' then Result:=eTZTuesday
  else if ADayName='Wed' then Result:=eTZWednesday
  else if ADayName='Thu' then Result:=eTZThursday
  else if ADayName='Fri' then Result:=eTZFriday
  else if ADayName='Sat' then Result:=eTZSaturday
  else
    Raise TTZException.CreateFmt('Macro expansion unknown day name at line %d "%s"',[FLineCounter,FCurrentLine]);
end;

function TPascalTZ.WeekDayToString(const AWeekDay: TTZWeekDay): AsciiString;
begin
  case AWeekDay of
    eTZSunday     : Result:='Sunday';
    eTZMonday     : Result:='Monday';
    eTZTuesday    : Result:='Tuesday';
    eTZWednesday  : Result:='Wednesday';
    eTZThursday   : Result:='Thursday';
    eTZFriday     : Result:='Friday';
    eTZSaturday   : Result:='Saturday';
  end;
end;

procedure TPascalTZ.MacroSolver(var ADate: TTZDateTime;
  const ADayString: AsciiString);
var
  j,k: integer;
  ConditionalMacro: Boolean;
  ConditionalBegin: integer;
  ConditionalEnd: integer;
  ConditionA,ConditionOp,ConditionB: AsciiString;
  WeekDay: TTZWeekDay;
  LastWeekDay: TTZDay;
  FirstWeekDay: TTZDay;
begin
  //First check regular number...
  j:=StrToIntDef(ADayString,-1);
  if j<>-1 then begin
    ADate.Day:=j;
    Exit;
  end;

  //This function check for some macros to specify a given day in a
  //month and a year, like "lastSun" or "Mon>=15"

  //First check if it is a conditional macro
  ConditionalMacro:=false;
  ConditionalBegin:=0;
  ConditionalEnd:=0;
  for j := 1 to Length(ADayString) do begin
    if ADayString[j] in ['<','=','>'] then begin
      ConditionalMacro:=true;
      ConditionalBegin:=j;
      for k := j+1 to Length(ADayString) do begin
        if not (ADayString[k] in ['<','=','>']) then begin
          ConditionalEnd:=k;
        end;
      end;
      break;
    end;
  end;
  if ConditionalMacro then begin
    if (ConditionalEnd=0) or (ConditionalBegin=0) then begin
      Raise TTZException.CreateFmt('Macro expansion not possible at line %d "%s"',[FLineCounter,FCurrentLine]);
    end;
    ConditionA:=Copy(ADayString,1,ConditionalBegin-1);
    ConditionOp:=Copy(ADayString,ConditionalBegin,ConditionalEnd-ConditionalBegin-1);
    ConditionB:=Copy(ADayString,ConditionalEnd,Length(ADayString)-ConditionalEnd+1);
    WeekDay:=DayNameToNumber(ConditionA);
    j:=StrToInt(ConditionB);
    if ConditionOp='>' then begin
      ADate.Day:=j+1;
      FirstWeekDay:=MacroFirstWeekDay(ADate,WeekDay);
    end else if ConditionOp='>=' then begin
      ADate.Day:=j;
      FirstWeekDay:=MacroFirstWeekDay(ADate,WeekDay);
    end else if ConditionOp='<' then begin
      ADate.Day:=j-1;
      FirstWeekDay:=MacroLastWeekDay(ADate,WeekDay);
    end else if ConditionOp='<=' then begin
      ADate.Day:=j;
      FirstWeekDay:=MacroLastWeekDay(ADate,WeekDay);
    end else begin
      Raise TTZException.CreateFmt('Macro expansion not possible at line %d "%s" Unknown conditional.',[FLineCounter,FCurrentLine]);
    end;
    ADate.Day:=FirstWeekDay;
  end else begin
    //It is not a conditional macro, so it could be firstXXX or lastXXX
    if LeftStr(ADayString,5)='first' then begin
      WeekDay:=DayNameToNumber(Copy(ADayString,6,Length(ADayString)-5));
      FirstWeekDay:=MacroFirstWeekDay(ADate,WeekDay);
      ADate.Day:=FirstWeekDay;
    end else if LeftStr(ADayString,4)='last' then begin
      WeekDay:=DayNameToNumber(Copy(ADayString,5,Length(ADayString)-4));
      LastWeekDay:=MacroLastWeekDay(ADate,WeekDay);
      ADate.Day:=LastWeekDay;
    end else begin
      Raise TTZException.CreateFmt('Macro expansion not possible at line %d "%s"',[FLineCounter,FCurrentLine]);
    end;
  end;
end;

function TPascalTZ.MacroFirstWeekDay(const ADate: TTZDateTime;
  const AWeekDay: TTZWeekDay; const ASinceMonthDay: TTZDay): TTZDay;
var
  TmpDate: TTZDateTime;
  FirstWDInMonth: TTZWeekDay;
  TheDay: Integer;
begin
  TmpDate:=ADate;
  TmpDate.Day:=ASinceMonthDay;
  FirstWDInMonth:=WeekDayOf(TmpDate);
  TheDay:=1;
  if FirstWDInMonth<AWeekDay Then begin
    inc(TheDay,(integer(AWeekDay)-integer(FirstWDInMonth)));
  end else if FirstWDInMonth>AWeekDay then begin
    inc(TheDay,7-(integer(FirstWDInMonth)-integer(AWeekDay)));
  end;
  if IsLeapYear(ADate.Year) Then begin
    if TheDay>TTZMonthDaysLeapYearCount[ADate.Month] Then TheDay:=0; //Invalidate day
  end else begin
    if TheDay>TTZMonthDaysCount[ADate.Month] Then TheDay:=0; //Invalidate day
  end;
  if TheDay<1 then begin
    Raise TTZException.CreateFmt('No valid week day for "%s" after %.4d.%.2d.%.2d',[WeekDayToString(AWeekDay),TmpDate.Year,TmpDate.Month,TmpDate.Day]);
  end;
  Result:=TheDay;
end;

function TPascalTZ.MacroLastWeekDay(const ADate: TTZDateTime;
  const AWeekDay: TTZWeekDay; const ASinceMonthDay: TTZDay): TTZDay;
var
  TmpDate: TTZDateTime;
  LastWDInMonth: TTZWeekDay;
  TheDay: Integer;
begin
  TmpDate:=ADate;
  if not IsLeapYear(ADate.Year) Then begin
    if ASinceMonthDay>TTZMonthDaysCount[TmpDate.Month] then begin
      TmpDate.Day:=TTZMonthDaysCount[TmpDate.Month];
    end else begin
      TmpDate.Day:=ASinceMonthDay;
    end;
  end else begin
    if ASinceMonthDay>TTZMonthDaysLeapYearCount[TmpDate.Month] then begin
      TmpDate.Day:=TTZMonthDaysLeapYearCount[TmpDate.Month];
    end else begin
      TmpDate.Day:=ASinceMonthDay;
    end;
  end;
  LastWDInMonth:=WeekDayOf(TmpDate);
  TheDay:=TmpDate.Day;
  if LastWDInMonth<AWeekDay Then begin
    Dec(TheDay,7-(integer(AWeekDay)-integer(LastWDInMonth)));
  end else if LastWDInMonth>AWeekDay then begin
    Dec(TheDay,(integer(LastWDInMonth)-integer(AWeekDay)));
  end;
  if TheDay<1 then begin
    Raise TTZException.CreateFmt('No valid week day for "%s" before %.4d.%.2d.%.2d',[WeekDayToString(AWeekDay),TmpDate.Year,TmpDate.Month,TmpDate.Day]);
  end;
  Result:=TheDay;
end;

function TPascalTZ.TimeToSeconds(const ATime: AsciiString): integer;
var
  Sign: integer;
  TwoColons: integer;
  j: integer;
  TmpTime: AsciiString;
  TimeIterator: TTZLineIterate;
  Hours: TTZHour;
  Minutes: TTZMinute;
  Seconds: TTZSecond;
begin
  //Time could be expressed in:
  // [-]m = minutes
  // [-]h:m = hours:minutes
  // [-]h:m:s = hours:minutes:seconds
  //So count the amount of ':' to get the format

  if ATime[1]='-' Then begin
    Sign:=-1; //Negative seconds...
    TmpTime:=Copy(ATime,2,Length(ATime)-1);
  end else begin
    Sign:=1;  //Positive seconds...
    TmpTime:=ATime;
  end;
  TwoColons:=0;
  for j := 1 to Length(TmpTime) do begin
    if TmpTime[j]=':' then begin
      inc(TwoColons);
    end;
  end;
  case TwoColons of
    0:  begin //Format is "m" minutes.
          Result:=StrToInt(TmpTime)*60; //No range check as it could be a big amount.
        end;
    1:  begin //Format is "hh:mm"
          TimeIterator:=TTZLineIterate.Create(TmpTime,':');
          try
            Hours:=StrToInt(TimeIterator.GetNextWord);
            Minutes:=StrToInt(TimeIterator.GetNextWord);
            Result:=Hours*3600+Minutes*60;
          except
          On E: Exception do begin
              TimeIterator.Free;
              Raise TTZException.CreateFmt('Parse time error at line %d "%s" Error: [%s]',[FLineCounter,FCurrentLine,E.Message]);
            end;
          end;
          TimeIterator.Free;
        end;
    2:  begin //Format is "hh:mm:ss"
          TimeIterator:=TTZLineIterate.Create(TmpTime,':');
          try
            Hours:=StrToInt(TimeIterator.GetNextWord);
            Minutes:=StrToInt(TimeIterator.GetNextWord);
            Seconds:=StrToInt(TimeIterator.GetNextWord);
            Result:=Hours*3600+Minutes*60+Seconds;
          except
          On E: Exception do begin
              TimeIterator.Free;
              Raise TTZException.CreateFmt('Parse time error at line %d "%s" Error: [%s]',[FLineCounter,FCurrentLine,E.Message]);
            end;
          end;
          TimeIterator.Free;
        end;
    else
        begin
          TimeIterator.Free;
          Raise TTZException.CreateFmt('Parse time error at line %d "%s"',[FLineCounter,FCurrentLine]);
        end;
  end;
  Result:=Result*Sign;
end;

function TPascalTZ.LookupRuleNonIndexed(const AName: AsciiString): Integer;
var
  j: integer;
begin
  Result:=-1;
  for j := 0 to High(FRules) do begin
    if FRules[j].Name=AName then begin
      Result:=j;
      break;
    end;
  end;
end;

function TPascalTZ.ParseLine(const ALine: AsciiString;const AParseSequence: TParseSequence): boolean;
var
  j: integer;
  Parser: TTZLineIterate;
  PreParseLine: AsciiString;
  ZoneContinue: Boolean;
  spCount: integer;
begin
  PreParseLine:=ALine;
  j:=Pos('#',PreParseLine);
  if j>0 then PreParseLine:=Copy(PreParseLine,1,j-1);
  spCount:=0;
  for j := 1 to Length(PreParseLine) do begin
    if PreParseLine[j]=#9 then PreParseLine[j]:=#32;
    if PreParseLine[j]=#32 then begin
      inc(spCount);
    end;
  end;
  if spCount=Length(PreParseLine) Then PreParseLine:=''; //all spaces in line
  if Length(PreParseLine)>0 then begin
    FCurrentLine:=ALine;
    Parser:=TTZLineIterate.Create(PreParseLine);
    try
      ZoneContinue:=false;
      if (PreParseLine[1]=#32) or (PreParseLine[1]=#9) then begin
        //Its a continuation
        if ParseStatusTAG<>'Zone' then begin
          Parser.Free;
          Raise TTZException.CreateFmt('Continue error at line: "%s" (No Zone)',[ALine]);
        end;
        ZoneContinue:=true;
      end else begin
        ParseStatusTAG:=Parser.GetNextWord;
      end;
      if (ParseStatusTAG='Zone') then begin
        if (AParseSequence=TTzParseZone) Then begin
          if not ZoneContinue then begin
            ParseStatusPreviousZone:=Parser.GetNextWord;
          end;
          ParseZone(Parser,ParseStatusPreviousZone);
        end;
      end else if (ParseStatusTAG='Rule') then begin
        if (AParseSequence=TTzParseRule) then begin
          ParseRule(Parser);
        end;
      end else if ParseStatusTAG='Link' then begin

      end else begin
        Parser.Free;
        Raise TTZException.CreateFmt('Parsing error at line: "%s"',[ALine]);
      end;
    finally
      Parser.Free;
    end;
    Result:=true;
  end else begin
    Result:=true;
  end;
end;

function TPascalTZ.ParseZone(const AIterator: TTZLineIterate;
  const AZone: AsciiString): boolean;
var
  Index: integer;
  RuleName: AsciiString;
  RuleTmpIndex: integer;
  TmpWord: AsciiString;
begin
  Index:=Length(FZones);
  SetLength(FZones,Index+1);
  with FZones[Index] do begin
    //First is the zone name
    if Length(AZone)>TZ_ZONENAME_SIZE then begin
      SetLength(FZones,Index); //Remove put information
      Raise TTZException.CreateFmt('Name on Zone line "%s" too long. (Increase source code TZ_ZONENAME_SIZE)',[AIterator.CurrentLine]);
    end;
    Name:=AZone;

    //Now check the offset
    TmpWord:=AIterator.GetNextWord; //Offset
    Offset:=TimeToSeconds(TmpWord);

    //Now check the rules...
    RuleName:=AIterator.GetNextWord;
    if RuleName='' Then begin
      SetLength(FZones,Index); //Remove put information
      Raise TTZException.CreateFmt('Rule on Zone line "%s" empty.',[AIterator.CurrentLine]);
    end;
    if RuleName='-' then begin
      //Standard time (Local time)
      RuleFixedOffset:=0;
      RuleIndex:=-1;
    end else if RuleName[1] in ['0'..'9'] then begin
      //Fixed offset time to get standard time (Local time)
      RuleFixedOffset:=TimeToSeconds(RuleName);
      RuleIndex:=-1;
    end else begin
      RuleTmpIndex:=LookupRuleNonIndexed(RuleName);
      if RuleTmpIndex<0 then begin
        SetLength(FZones,Index); //Remove put information
        Raise TTZException.CreateFmt('Rule on Zone line "%s" not found.',[AIterator.CurrentLine]);
      end else begin
        RuleIndex:=RuleTmpIndex;
        RuleFixedOffset:=0; //Nonsense value.
      end;
    end;

    //Now its time for the format (GMT, BST, ...)
    TmpWord:=AIterator.GetNextWord;
    if Length(TmpWord)>TZ_TIMEZONELETTERS_SIZE Then begin
      SetLength(FZones,Index); //Remove put information
      Raise TTZException.CreateFmt('Format on Zone line "%s" too long. (Increase source code TZ_TIMEZONELETTERS_SIZE)',[AIterator.CurrentLine]);
    end;
    TimeZoneLetters:=TmpWord;

    //And finally the UNTIL field which format is optional fields from
    //left to right: year month day hour[s]
    //defaults:      YEAR Jan   1   0:00:00
    RuleValidUntil:=ParseUntilFields(AIterator,RuleValidUntilGMT);
  end;
  Result:=true;
end;

function TPascalTZ.ParseRule(const AIterator: TTZLineIterate): Boolean;
var
  Index: integer;
  TmpWord: AsciiString;
  StandardTimeFlag: char;
begin
  Index:=Length(FRules);
  SetLength(FRules,Index+1);
  with FRules[Index] do begin
    TmpWord:=AIterator.GetNextWord;
    if Length(TmpWord)>TZ_RULENAME_SIZE then begin
      SetLength(FRules,Index); //Remove put information
      Raise TTZException.CreateFmt('Name on Rule line "%s" too long. (Increase source code TZ_RULENAME_SIZE)',[AIterator.CurrentLine]);
    end;
    Name:=TmpWord;
    //Begin year...
    TmpWord:=AIterator.GetNextWord;
    FromYear:=StrToInt(TmpWord);
    //End year...
    TmpWord:=AIterator.GetNextWord;
    if TmpWord='only' then begin
      ToYear:=FromYear;
    end else if TmpWord='max' then begin
      //max year, so in this case 9999
      ToYear:=9999;
    end else begin
      ToYear:=StrToInt(TmpWord);
    end;
    //Year type (macro)
    TmpWord:=AIterator.GetNextWord;
    if TmpWord='-' then begin
      //No year type, so all years.
    end else begin
      //Special year... check macro...
      //No one defined by now, so raise an exception if found.
      Raise TTZException.CreateFmt('Year type not supported in line "%s"',[AIterator.CurrentLine]);
    end;
    //In month...
    TmpWord:=AIterator.GetNextWord;
    InMonth:=MonthNumberFromShortName(TmpWord);
    //On Rule...
    TmpWord:=AIterator.GetNextWord;
    if sizeOf(Onrule)<Length(TmpWord) then begin
      Raise TTZException.CreateFmt('ON Rule condition at "%s" too long. (Increase source code TZ_ONRULE_SIZE)',[AIterator.CurrentLine]);
    end;
    OnRule:=TmpWord;
    //AT field
    TmpWord:=AIterator.GetNextWord;
    StandardTimeFlag:=TmpWord[Length(TmpWord)];
    if StandardTimeFlag in ['s','u','g'] Then begin
      if StandardTimeFlag='s' then begin
        AtHourGMT:=false;
      end else begin
        AtHourGMT:=true;
      end;
      TmpWord:=Copy(TmpWord,1,Length(TmpWord)-1); //remove the standard time flag
    end;
    AtHourTime:=TimeToSeconds(TmpWord);
    //SAVE field
    TmpWord:=AIterator.GetNextWord;
    SaveTime:=TimeToSeconds(TmpWord);
    //LETTERS field
    TimeZoneLetters:=AIterator.GetNextWord;
    if TimeZoneLetters='-' Then TimeZoneLetters:='';
  end;
  Result:=true;
end;

function TPascalTZ.DateTimeToStr(const ADate: TTZDateTime): ansistring;
var
  H,M,S: BYTE;
begin
  DateTimeToTime(ADate,H,M,S);
  Result:=format('%.4d.%.2d.%.2d %.2d:%.2d:%.2d',[ADate.Year,ADate.Month,ADate.Day,H,M,S]);
end;

function TPascalTZ.IncDays(const ADate: TTZDateTime; const ADays: integer
  ): TTZDateTime;
var
  Elapsed: integer;
begin
  Elapsed:=ElapsedDaysSinceAD(ADate);
  inc(Elapsed,ADays);
  Result:=ElapsedDaysSinceADToDate(Elapsed);
  Result.SecsInDay:=Adate.SecsInDay;
end;

function TPascalTZ.TZDateToPascalDate(const ADate: TTZDateTime): TDateTime;
begin
  Result:=EncodeDate(ADate.Year,Adate.Month,ADate.Day);
  Result:=IncSecond(Result,ADate.SecsInDay);
end;

function TPascalTZ.PascalDateToTZDate(const ADate: TDateTime): TTZDateTime;
begin
  Result.Year:=YearOf(ADate);
  Result.Month:=MonthOf(ADate);
  Result.Day:=DayOf(ADate);
  Result.SecsInDay:=HourOf(ADate)*3600+MinuteOf(ADate)*60+SecondOf(ADate);
end;

procedure TPascalTZ.GetTimeZoneNames(const AZones: TStringList;
  const AOnlyGeoZones: Boolean);
var
  j: integer;
  LT: AsciiString;
begin
  AZones.Clear;
  LT:='';
  for j := 0 to High(FZones) do begin
    if FZones[j].Name<>LT Then begin
      LT:=FZones[j].Name;
      if AOnlyGeoZones then begin
        if Pos('/',LT)>0 then begin
          AZones.Add(LT);
        end;
      end else begin
        AZones.Add(LT);
      end;
    end;
  end;
end;

function TPascalTZ.GMTToLocalTime(const ADateTime: TDateTime;
  const AToZone: ansistring; out ATimeZoneSubFix: ansistring): TDateTime;
var
  MilliSeconds: integer;
begin
  MilliSeconds:=MilliSecondOfTheSecond(ADateTime);
  Result:=TZDateToPascalDate(GMTToLocalTime(PascalDateToTZDate(ADateTime),AToZone,ATimeZoneSubFix));
  Result:=IncMilliSecond(Result,MilliSeconds);
end;

function TPascalTZ.GMTToLocalTime(const ADateTime: TTZDateTime;
  const AToZone: ansistring; out ATimeZoneName: ansistring): TTZDateTime;
var
  j: integer;
  ZoneIndex: integer;
  RuleIndex: integer;
  ApplyRuleName: AsciiString;
  RuleBeginDate,RuleEndDate: TTZDateTime;
  SaveTime: integer;
  RuleLetters: AsciiString;
  ZoneNameCut: integer;
begin
  //Find zone matching target...
  ZoneIndex:=FindZoneName(AToZone);
  // Now check which zone configuration line matches the given date.
  ZoneIndex:=FindZoneForDate(ZoneIndex,ADateTime);
  RuleIndex:=FZones[ZoneIndex].RuleIndex;
  if RuleIndex=-1 then begin
    //No rule is applied, so use the zone fixed offset
    Result:=ADateTime;
    inc(Result.SecsInDay,FZones[ZoneIndex].RuleFixedOffset+FZones[ZoneIndex].Offset);
    ATimeZoneName:=FZones[ZoneIndex].TimeZoneLetters;
    FixUpTime(Result);
    exit;
  end;
  //Now we have the valid rule index...
  ApplyRuleName:=FRules[RuleIndex].Name;
  j:=RuleIndex;
  SaveTime:=0;
  while (j<=High(FRules)) and (FRules[j].Name=ApplyRuleName) do begin
    if (ADateTime.Year>=FRules[j].FromYear) and (ADateTime.Year<=FRules[j].ToYear) then begin
      //The year is in the rule range, so discard year information...
      RuleBeginDate.Year:=ADateTime.Year;
      RuleBeginDate.Month:=FRules[j].InMonth;
      MacroSolver(RuleBeginDate,FRules[j].OnRule);
      RuleBeginDate.SecsInDay:=FRules[j].AtHourTime;

      RuleEndDate.Year:=ADateTime.Year;
      RuleEndDate.Month:=12;
      RuleEndDate.Day:=31;
      RuleEndDate.SecsInDay:=86400;

      if (CompareDates(ADateTime,RuleBeginDate)>-1) and
         (CompareDates(ADateTime,RuleEndDate)<1) then begin
        SaveTime:=FRules[j].SaveTime;
        RuleLetters:=FRules[j].TimeZoneLetters;
      end;
    end;
    inc(j);
  end;
  Result:=ADateTime;
  inc(Result.SecsInDay,SaveTime+FZones[ZoneIndex].Offset);
  ATimeZoneName:=format(FZones[ZoneIndex].TimeZoneLetters,[RuleLetters]);
  //When timezonename is XXX/YYY XXX is no daylight and YYY is daylight saving.
  ZoneNameCut:=Pos('/',ATimeZoneName);
  if ZoneNameCut>0 then begin
    if SaveTime=0 then begin
      //Use the XXX
      ATimeZoneName:=Copy(ATimeZoneName,1,ZoneNameCut-1);
    end else begin
      //Use the YYY
      ATimeZoneName:=Copy(ATimeZoneName,ZoneNameCut+1,Length(ATimeZoneName)-ZoneNameCut);
    end;
  end;
  FixUpTime(Result);
end;

function TPascalTZ.LocalTimeToGMT(const ADateTime: TDateTime;
  const AFromZone: ansistring): TDateTime;
var
  MilliSeconds: integer;
begin
  MilliSeconds:=MilliSecondOfTheSecond(ADateTime);
  Result:=TZDateToPascalDate(LocalTimeToGMT(PascalDateToTZDate(ADateTime),AFromZone));
  Result:=IncMilliSecond(Result,MilliSeconds);
end;

function TPascalTZ.TimeZoneToTimeZone(const ADateTime: TDateTime;
  const AFromZone, AToZone: ansistring; out ATimeZoneSubFix: ansistring
  ): TDateTime;
var
  Tmp: TTZDateTime;
begin
  Tmp:=PascalDateToTZDate(ADateTime);
  Tmp:=LocalTimeToGMT(Tmp,AFromZone);
  Tmp:=GMTToLocalTime(Tmp,AToZone,ATimeZoneSubFix);
  Result:=TZDateToPascalDate(Tmp);
end;

function TPascalTZ.LocalTimeToGMT(const ADateTime: TTZDateTime;
  const AFromZone: ansistring): TTZDateTime;
var
  ZoneIndex,RuleIndex: integer;
  ApplyRuleName: AsciiString;
  RuleBeginDate,RuleEndDate: TTZDateTime;
  SaveTime: integer;
  j: integer;
begin
  //Find zone matching target...
  ZoneIndex:=FindZoneName(AFromZone);
  // Now check which zone configuration line matches the given date.
  ZoneIndex:=FindZoneForDate(ZoneIndex,ADateTime);
  RuleIndex:=FZones[ZoneIndex].RuleIndex;
  if RuleIndex=-1 then begin
    //No rule is applied, so use the zone fixed offset
    Result:=ADateTime;
    Dec(Result.SecsInDay,FZones[ZoneIndex].RuleFixedOffset+FZones[ZoneIndex].Offset);
    FixUpTime(Result);
    exit;
  end;
  //Now we have the valid rule index...
  ApplyRuleName:=FRules[RuleIndex].Name;
  j:=RuleIndex;
  SaveTime:=0;
  while (j<=High(FRules)) and (FRules[j].Name=ApplyRuleName) do begin
    if (ADateTime.Year>=FRules[j].FromYear) and (ADateTime.Year<=FRules[j].ToYear) then begin
      //The year is in the rule range, so discard year information...
      RuleBeginDate.Year:=ADateTime.Year;
      RuleBeginDate.Month:=FRules[j].InMonth;
      MacroSolver(RuleBeginDate,FRules[j].OnRule);
      RuleBeginDate.SecsInDay:=FRules[j].AtHourTime+FZones[ZoneIndex].Offset;

      RuleEndDate.Year:=ADateTime.Year;
      RuleEndDate.Month:=12;
      RuleEndDate.Day:=31;
      RuleEndDate.SecsInDay:=SecsPerDay;

      if (CompareDates(ADateTime,RuleBeginDate)>-1) and
         (CompareDates(ADateTime,RuleEndDate)<1) then begin
        SaveTime:=FRules[j].SaveTime;
      end;
    end;
    inc(j);
  end;
  Result:=ADateTime;
  Dec(Result.SecsInDay,SaveTime+FZones[ZoneIndex].Offset);
  FixUpTime(Result);
  if FDetectInvalidLocalTimes then begin
    //Applyrulename here is a dummy variable
    if CompareDates(ADateTime,GMTToLocalTime(Result,AFromZone,ApplyRuleName))<>0 then begin
      Raise TTZException.CreateFmt('The time %s does not exists in %s',[DateTimeToStr(ADateTime),AFromZone]);
    end;
  end;
end;

function TPascalTZ.ElapsedDaysSinceADToDate(const AElapsedDays: integer
  ): TTZDateTime;
var
  TYear: integer;
  Temporal,Acumulator: integer;
  j: integer;
begin
  if AElapsedDays>=FIRST_POSTGREGORIAN_DAY then begin
    Acumulator:=AElapsedDays-15+3+10; // -15+3+10 missing leap years pre gregorian and gregorian leap days
    //400 years
    Temporal:=Acumulator div 146097;
    TYear:=Temporal*400;
    dec(Acumulator,Temporal*146097);
    //Centuries
    Temporal:=Acumulator div 36524;
    inc(TYear,Temporal*100);
    dec(Acumulator,Temporal*36524);
    //"Normal" leap every 4
    Temporal:=Acumulator div 1461;
    inc(TYear,Temporal*4);
    dec(Acumulator,Temporal*1461);
    //Regular years...
    Temporal:=Acumulator div 365;
    inc(TYear,Temporal);
    dec(Acumulator,Temporal*365);

    //now d is the day in the year...

    if Acumulator>0 then begin
      inc(TYear);
    end else begin
      if IsLeapYear(TYear) then begin
        Acumulator:=366;
      end else begin
        Acumulator:=365;
      end;
    end;
    Result.Year:=TYear;
  end else begin
    Acumulator:=AElapsedDays;

    //"Normal" leap every 4
    Temporal:=Acumulator div 1461;
    TYear:=Temporal*4;
    dec(Acumulator,Temporal*1461);
    //Regular years...
    Temporal:=Acumulator div 365;
    inc(TYear,Temporal);
    dec(Acumulator,Temporal*365);
    //now d is the day in the year...

    if Acumulator>0 then begin
      inc(TYear);
    end else begin
      if IsLeapYear(TYear) then begin
        Acumulator:=366;
      end else begin
        Acumulator:=365;
      end;
    end;
    Result.Year:=TYear;
  end;
  //This could be done with some maths, but for now leave it with a for..loop
  if IsLeapYear(TYear) Then begin
    for j := 1 to 12 do begin
      if Acumulator<=TTZMonthDaysLeapYearCount[j] then begin
        Result.Month:=j;
        Result.Day:=Acumulator;
        dec(Acumulator,Acumulator);
        break;
      end else begin
        dec(Acumulator,TTZMonthDaysLeapYearCount[j]);
      end;
    end;
  end else begin
    for j := 1 to 12 do begin
      if Acumulator<=TTZMonthDaysCount[j] then begin
        Result.Month:=j;
        Result.Day:=Acumulator;
        dec(Acumulator,Acumulator);
        break;
      end else begin
        dec(Acumulator,TTZMonthDaysCount[j]);
      end;
    end;
  end;
  if Acumulator<>0 then begin
    Raise TTZException.Create('Internal error');
  end;
end;

function TPascalTZ.ParseDatabaseFromFile(const AFileName: String): Boolean;
var
  FileStream: TFileStream;
begin
  FileStream:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    Result:=ParseDatabaseFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TPascalTZ.ParseDatabaseFromStream(const AStream: TStream): Boolean;
var
  Buffer: PChar;
  FileSize: integer;
  LineBegin: integer;
  LineSize: integer;
  ThisLine: AsciiString;
  ParseSequence: TParseSequence;
begin
  FileSize:=AStream.Size;
  Buffer:=nil;
  GetMem(Buffer,FileSize);
  if not Assigned(Buffer) Then begin
    raise EOutOfMemory.Create(SErrOutOfMemory);
  end;
  try
    if AStream.Read(Buffer^,FileSize)<>FileSize then begin
      Raise EStreamError.Create('Stream read error');
    end;
    ParseSequence:=TTzParseRule;
    while ParseSequence<TTzParseFinish do begin
      FLineCounter:=1;
      LineBegin:=0;
      LineSize:=0;
      while LineBegin<FileSize do begin
        if (Buffer[LineBegin+LineSize]=#13) or (Buffer[LineBegin+LineSize]=#10) then begin
          SetLength(ThisLine,LineSize);
          Move(Buffer[LineBegin],ThisLine[1],LineSize);
          ParseLine(ThisLine,ParseSequence);
          inc(LineBegin,LineSize);
          LineSize:=0;
          while (LineBegin<FileSize) and ((Buffer[LineBegin]=#13) or (Buffer[LineBegin]=#10)) do begin
            if Buffer[LineBegin]=#10 then begin
              inc(FLineCounter);
            end;
            inc(LineBegin);
          end;
        end else begin
          inc(LineSize);
        end;
      end;
      if LineSize>0 then begin
        inc(FLineCounter);
        SetLength(ThisLine,LineSize);
        Move(Buffer[LineBegin],ThisLine[1],LineSize);
        ParseLine(ThisLine,ParseSequence);
      end;
      if ParseSequence=TTzParseRule then begin
        //Sort the rules...
        SortRules();
      end;
      ParseSequence:=Succ(ParseSequence);
    end;
    Result:=true;
  finally
    FreeMem(Buffer);
  end;
end;

constructor TPascalTZ.Create();
begin
  FDetectInvalidLocalTimes:=true;
end;

end.

