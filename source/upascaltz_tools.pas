unit uPascalTZ_Tools;

{*******************************************************************************
This file is a part of PascalTZ package:
  https://github.com/dezlov/pascaltz

License:
  GNU Library General Public License (LGPL) with a special exception.
  Read accompanying README and COPYING files for more details.

Authors:
  2009 - José Mejuto
  2015 - Denis Kozlov
*******************************************************************************}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, uPascalTZ_Types;

procedure FixUpTime(var ADate: TTZDateTime);
function IncDays(const ADate: TTZDateTime; const ADays: Integer): TTZDateTime;
procedure DateTimeToTime(const ADate: TTZDateTime; out AHour,AMinute,ASecond: BYTE);
function DateTimeToStr(const ADate: TTZDateTime): String;
function TZDateToPascalDate(const ADate: TTZDateTime): TDateTime;
function PascalDateToTZDate(const ADate: TDateTime): TTZDateTime;
function MonthNumberFromShortName(const AMonth: AsciiString): TTZMonth;
function CompareDates(const ADate,BDate: TTZDateTime): integer;
function IsGregorianLeap(const ADate: TTZDateTime): Boolean;
procedure IsGregorianLeapException(const ADate: TTZDateTime);
function IsBeforeGregorianLeap(const ADate: TTZDateTime): Boolean;
function ExtractTimeForm(var TimeStr: AsciiString; out TimeForm: TTZTimeForm): Boolean;
function ExtractTimeFormDefault(var TimeStr: AsciiString; const Default: TTZTimeForm): TTZTimeForm;
function ParseUntilFields(const AIterator: TTZLineIterate; out ATimeForm: TTZTimeForm): TTZDateTime;
procedure MacroSolver(var ADate: TTZDateTime; const ADayString: AsciiString);
function MacroFirstWeekDay(const ADate: TTZDateTime;const AWeekDay: TTZWeekDay): TTZDay;
function MacroLastWeekDay(const ADate: TTZDateTime;const AWeekDay: TTZWeekDay): TTZDay;
function WeekDayOf(const ADate: TTZDateTime): TTZWeekDay;
function IsLeapYear(const AYear: integer): Boolean;
function WeekDayToString(const AWeekDay: TTZWeekDay): AsciiString;
function DayNameToNumber(const ADayName: AsciiString): TTZWeekDay;
function TimeToSeconds(const ATime: AsciiString): integer;
function IsGeoZoneName(const AZone: AsciiString): Boolean;
function GregorianDateToJulianDays(const Value: TTZDateTime): Integer;
function JulianDaysToGregorianDate(const Value: Integer): TTZDateTime;

const
  TTZMonthDaysCount:
            array [TTZMonth] of TTZDay=(31,28,31,30,31,30,31,31,30,31,30,31);
  TTZMonthDaysLeapYearCount:
            array [TTZMonth] of TTZDay=(31,29,31,30,31,30,31,31,30,31,30,31);

implementation

uses
  DateUtils;

function IsGeoZoneName(const AZone: AsciiString): Boolean;
begin
  Result := (Pos('/', AZone) > 0);
end;

procedure IsGregorianLeapException(const ADate: TTZDateTime);
begin
  if IsGregorianLeap(ADate) Then begin
    Raise TTZException.CreateFmt('Gregorian Leap, date does not exists. [%.4d.%.2d.%.2d]',[ADate.Year,ADate.Month,ADate.Day])
  end;
end;

function IsGregorianLeap(const ADate: TTZDateTime): Boolean;
begin
  Result:=false;
  if (ADate.Year=1582) and (ADate.Month=10) then begin
    if (ADate.Day>4) and (ADate.Day<15) Then begin
      Result:=true;
    end;
  end;
end;

function IsBeforeGregorianLeap(const ADate: TTZDateTime): Boolean;
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

procedure FixUpTime(var ADate: TTZDateTime);
var
  Days: integer;
begin
  if ADate.SecsInDay<0 then begin
    //Decrease some day(s)
    Days:=(ADate.SecsInDay) div (3600*24) - 1; // negative days
    ADate:=IncDays(ADate,Days); // negative days
    ADate.SecsInDay:=ADate.SecsInDay-Days*3600*24; // double negative days => positive
  end else if ADate.SecsInDay>=(3600*24) then begin
    //Increase some day(s)
    Days:=ADate.SecsInDay div (3600*24);
    ADate:=IncDays(ADate,Days);
    ADate.SecsInDay:=ADate.SecsInDay-Days*3600*24;
  end;
end;

procedure DateTimeToTime(const ADate: TTZDateTime; out AHour,
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

function CompareDates(const ADate, BDate: TTZDateTime): integer;
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

function MonthNumberFromShortName(const AMonth: AsciiString
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

function WeekDayOf(const ADate: TTZDateTime): TTZWeekDay;
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

function IsLeapYear(const AYear: integer): Boolean;
begin
  if AYear>1582 then begin
    if (AYear mod 400)=0 Then Exit(true); //All years every 400 years are leap years.
    if (AYear mod 100)=0 then Exit(false); //Centuries are not leap years (except previous % 400)
  end;
  if (AYear mod 4)=0 then exit(true); //Each 4 years a leap year comes in play.
  Result:=false;
end;

function WeekDayToString(const AWeekDay: TTZWeekDay): AsciiString;
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

function DayNameToNumber(const ADayName: AsciiString): TTZWeekDay;
begin
  if ADayName='Sun' then Result:=eTZSunday
  else if ADayName='Mon' then Result:=eTZMonday
  else if ADayName='Tue' then Result:=eTZTuesday
  else if ADayName='Wed' then Result:=eTZWednesday
  else if ADayName='Thu' then Result:=eTZThursday
  else if ADayName='Fri' then Result:=eTZFriday
  else if ADayName='Sat' then Result:=eTZSaturday
  else
    Raise TTZException.Create('Unknown day name: ' + ADayName);
end;

function ExtractTimeForm(var TimeStr: AsciiString; out TimeForm: TTZTimeForm): Boolean;
var
  TimeFormChar: AsciiChar;
begin
  Result := False;
  if Length(TimeStr) > 0 then
  begin
    TimeFormChar := TimeStr[Length(TimeStr)];
    case TimeFormChar of
      'w':         begin TimeForm := tztfWallClock; Result := True; end;
      's':         begin TimeForm := tztfStandard;  Result := True; end;
      'u','g','z': begin TimeForm := tztfUniversal; Result := True; end;
    end;
    if Result then
      Delete(TimeStr, Length(TimeStr), 1);
  end;
end;

function ExtractTimeFormDefault(var TimeStr: AsciiString; const Default: TTZTimeForm): TTZTimeForm;
begin
  if not ExtractTimeForm(TimeStr, Result) then
    Result := Default;
end;

function ParseUntilFields(const AIterator: TTZLineIterate;
  out ATimeForm: TTZTimeForm): TTZDateTime;
var
  TmpWord: AsciiString;
begin
  //Default Values...
  with Result do begin
    Year:=9999;
    Month:=1;
    Day:=1;
    SecsInDay:=0;
  end;
  ATimeForm:=tztfUniversal;
  TmpWord:=AIterator.GetNextWord;
  if TmpWord='' Then Exit;
  try
    Result.Year:=StrToInt(TmpWord);
  except
  On E: Exception do begin
          Raise TTZException.Create('Invalid date in "until" fields');
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
  // TODO: Need to properly use all possible time forms in ParseUntilFields
  // Default time form for UNTIL field in ZONE definition seems to be UTC.
  // It is not officially documented by can be extracted from examples in ZIC man page:
  // > Zurich was 34 minutes and 8 seconds west of GMT until 1848-09-12 at 00:00,
  // > when the offset changed to 29 minutes and 44 seconds.
  // > # Zone  NAME           GMTOFF   RULES       FORMAT  UNTIL
  // > Zone    Europe/Zurich  0:34:08  -           LMT     1848 Sep 12
  // >                        0:29:44  -           BMT     1894 Jun
  // The default UNTIL time of 00:00 applies whis assumed to be GMT in ZIC man page.
  ATimeForm := ExtractTimeFormDefault(TmpWord, tztfUniversal);
  Result.SecsInDay:=TimeToSeconds(TmpWord);
end;

procedure MacroSolver(var ADate: TTZDateTime;
  const ADayString: AsciiString);
var
  j,k: integer;
  ConditionalMacro: Boolean;
  ConditionalBegin: integer;
  ConditionalEnd: integer;
  ConditionA,ConditionOp,ConditionB: AsciiString;
  WeekDay: TTZWeekDay;
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
          Break;
        end;
      end;
      break;
    end;
  end;
  if ConditionalMacro then begin
    if (ConditionalEnd=0) or (ConditionalBegin=0) then begin
      Raise TTZException.Create('Macro expansion not possible: Unrecognised conditional part');
    end;
    ConditionA:=Copy(ADayString,1,ConditionalBegin-1);
    ConditionOp:=Copy(ADayString,ConditionalBegin,ConditionalEnd-ConditionalBegin);
    ConditionB:=Copy(ADayString,ConditionalEnd,Length(ADayString)-ConditionalEnd+1);
    WeekDay:=DayNameToNumber(ConditionA);
    j:=StrToInt(ConditionB);
    if ConditionOp='>' then begin
      ADate.Day:=j+1;
      ADate.Day:=MacroFirstWeekDay(ADate,WeekDay);
    end else if ConditionOp='>=' then begin
      ADate.Day:=j;
      ADate.Day:=MacroFirstWeekDay(ADate,WeekDay);
    end else if ConditionOp='<' then begin
      ADate.Day:=j-1;
      ADate.Day:=MacroLastWeekDay(ADate,WeekDay);
    end else if ConditionOp='<=' then begin
      ADate.Day:=j;
      ADate.Day:=MacroLastWeekDay(ADate,WeekDay);
    end else begin
      Raise TTZException.Create('Macro expansion not possible: Unknown condition operator');
    end;
  end else begin
    //It is not a conditional macro, so it could be firstXXX or lastXXX
    if LeftStr(ADayString,5)='first' then begin
      WeekDay:=DayNameToNumber(Copy(ADayString,6,Length(ADayString)-5));
      ADate.Day:=Low(TTZDay);
      ADate.Day:=MacroFirstWeekDay(ADate,WeekDay);
    end else if LeftStr(ADayString,4)='last' then begin
      WeekDay:=DayNameToNumber(Copy(ADayString,5,Length(ADayString)-4));
      ADate.Day:=High(TTZDay);
      ADate.Day:=MacroLastWeekDay(ADate,WeekDay);
    end else begin
      Raise TTZException.Create('Macro expansion not possible: Unrecognised macro');
    end;
  end;
end;

function TimeToSeconds(const ATime: AsciiString): integer;
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
              Raise TTZException.Create('Failed to parse time: ' + ATime);
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
              Raise TTZException.Create('Failed to parse time: ' + ATime);
            end;
          end;
          TimeIterator.Free;
        end;
    else
        begin
          TimeIterator.Free;
          Raise TTZException.Create('Failed to parse time: ' + ATime);
        end;
  end;
  Result:=Result*Sign;
end;

function MacroFirstWeekDay(const ADate: TTZDateTime;
  const AWeekDay: TTZWeekDay): TTZDay;
var
  FirstWDInMonth: TTZWeekDay;
  TheDay: Integer;
begin
  FirstWDInMonth:=WeekDayOf(ADate);
  TheDay:=ADate.Day;
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
    Raise TTZException.CreateFmt('No valid first week day for "%s" after %.4d.%.2d.%.2d',
      [WeekDayToString(AWeekDay), ADate.Year, ADate.Month, ADate.Day]);
  end;
  Result:=TheDay;
end;

function MacroLastWeekDay(const ADate: TTZDateTime;
  const AWeekDay: TTZWeekDay): TTZDay;
var
  TmpDate: TTZDateTime;
  LastWDInMonth: TTZWeekDay;
  TheDay: Integer;
begin
  TmpDate:=ADate;
  if not IsLeapYear(TmpDate.Year) Then begin
    if TmpDate.Day>TTZMonthDaysCount[TmpDate.Month] then
      TmpDate.Day:=TTZMonthDaysCount[TmpDate.Month];
  end else begin
    if TmpDate.Day>TTZMonthDaysLeapYearCount[TmpDate.Month] then begin
      TmpDate.Day:=TTZMonthDaysLeapYearCount[TmpDate.Month];
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
    Raise TTZException.CreateFmt('No valid last week day for "%s" before %.4d.%.2d.%.2d',
      [WeekDayToString(AWeekDay), ADate.Year, ADate.Month, ADate.Day]);
  end;
  Result:=TheDay;
end;

function DateTimeToStr(const ADate: TTZDateTime): String;
var
  H,M,S: BYTE;
begin
  DateTimeToTime(ADate,H,M,S);
  Result:=format('%.4d.%.2d.%.2d %.2d:%.2d:%.2d',[ADate.Year,ADate.Month,ADate.Day,H,M,S]);
end;

function IncDays(const ADate: TTZDateTime; const ADays: Integer): TTZDateTime;
var
  JulianDays: Integer;
begin
  JulianDays := GregorianDateToJulianDays(ADate);
  Inc(JulianDays, ADays);
  Result := JulianDaysToGregorianDate(JulianDays);
  Result.SecsInDay := ADate.SecsInDay;
end;

function TZDateToPascalDate(const ADate: TTZDateTime): TDateTime;
begin
  Result:=EncodeDate(ADate.Year,Adate.Month,ADate.Day);
  Result:=IncSecond(Result,ADate.SecsInDay);
end;

function PascalDateToTZDate(const ADate: TDateTime): TTZDateTime;
begin
  Result.Year:=YearOf(ADate);
  Result.Month:=MonthOf(ADate);
  Result.Day:=DayOf(ADate);
  Result.SecsInDay:=HourOf(ADate)*3600+MinuteOf(ADate)*60+SecondOf(ADate);
end;

//******************************************************************************
// Convert "Gregorian Calendar Date" to "Julian Day Number". Time of day is ignored.
// The number of days is since Julian Date Epox: 12:00 Jan 1, 4713 BC.
//
// The algorithm is valid at least for all positive Julian Day Numbers.
//
// Implemented according to a formula on Wikipedia page:
// https://en.wikipedia.org/wiki/Julian_day#Converting_Julian_or_Gregorian_calendar_date_to_Julian_Day_Number
//
// See Also:
// DateTimeToJulianDate, DateTimeToModifiedJulianDate in FPC DateUtils unit.
//******************************************************************************
function GregorianDateToJulianDays(const Value: TTZDateTime): Integer;
var
  a, y, m: Integer;
begin
  a := (14 - Value.Month) div 12;
  y := Value.Year + 4800 - a;
  m := Value.Month + 12 * a - 3;
  Result := Value.Day
    + ((153 * m + 2) div 5)
    + (365 * y)
    + (y div 4)
    - (y div 100)
    + (y div 400)
    - 32045;
end;

//******************************************************************************
// Convert "Julian Day Number" to "Gregorian Calendar Date". Time of day is set to zero.
// The number of days is since Julian Date Epox: 12:00 Jan 1, 4713 BC.
//
// This is an algorithm by Richards to convert a Julian Day Number to
// a date in the Gregorian calendar (proleptic, when applicable).
// Richards does not state which dates the algorithm is valid for.
//
// Implemented according to a formula on Wikipedia page:
// https://en.wikipedia.org/wiki/Julian_day#Julian_or_Gregorian_calendar_from_Julian_day_number
//
// See Also:
// JulianDateToDateTime, ModifiedJulianDateToDateTime in FPC DateUtils unit.
//******************************************************************************
function JulianDaysToGregorianDate(const Value: Integer): TTZDateTime;
const
  y=4716; v=3;
  j=1401; u=5;
  m=2;    s=153;
  n=12;   w=2;
  r=4;    B=274277;
  p=1461; C=-38;
var
  f, e, g, h: Integer;
begin
  // f = J + j + (((4 × J + B) div 146097) × 3) div 4 + C
  f := Value + j + (((4 * Value + B) div 146097) * 3) div 4 + C;
  // e = r × f + v
  e := r * f + v;
  // g = mod(e, p) div r
  g := (e mod p) div r;
  // h = u × g + w
  h := u * g + w;
  // D = (mod(h, s)) div u + 1
  Result.Day := (h mod s) div u + 1;
  // M = mod(h div s + m, n) + 1
  Result.Month := ((h div s + m) mod n) + 1;
  // Y = (e div p) - y + (n + m - M) div n
  Result.Year := (e div p) - y + (n + m - Result.Month) div n;
  Result.SecsInDay := 0;
end;


end.

