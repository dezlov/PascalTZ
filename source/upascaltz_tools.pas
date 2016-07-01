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
function IncSeconds(const ADate: TTZDateTime; const ASeconds: Integer): TTZDateTime;
function IncDays(const ADate: TTZDateTime; const ADays: Integer): TTZDateTime;
procedure DateTimeToTime(const ADate: TTZDateTime; out AHour,AMinute,ASecond: BYTE);
function DateTimeToStr(const ADate: TTZDateTime): String;
function TZDateToPascalDate(const ADate: TTZDateTime): TDateTime;
function PascalDateToTZDate(const ADate: TDateTime): TTZDateTime;
function MakeTZDate(const Year, Month, Day, SecsInDay: Integer): TTZDateTime; inline;
function MonthNumberToShortName(const AMonthNumber: Integer): AsciiString;
function MonthNumberFromShortName(const AMonthName: AsciiString): TTZMonth;
function MinDate(const ADate, BDate: TTZDateTime): TTZDateTime;
function MaxDate(const ADate, BDate: TTZDateTime): TTZDateTime;
function CompareDates(const ADate,BDate: TTZDateTime): Integer;
function IsGregorianLeap(const ADate: TTZDateTime): Boolean;
procedure IsGregorianLeapException(const ADate: TTZDateTime);
function IsBeforeGregorianLeap(const ADate: TTZDateTime): Boolean;
function TimeFormToChar(const ATimeForm: TTZTimeForm): AsciiChar;
function CharToTimeForm(const AChar: AsciiChar; out ATimeForm: TTZTimeForm): Boolean;
function ExtractTimeForm(var TimeStr: AsciiString; out TimeForm: TTZTimeForm): Boolean;
function ExtractTimeFormDefault(var TimeStr: AsciiString; const Default: TTZTimeForm): TTZTimeForm;
function ParseUntilFields(const AIterator: TTZLineIterate; out ATimeForm: TTZTimeForm;
  const ADefaultTimeForm: TTZTimeForm): TTZDateTime;
procedure MacroSolver(var ADate: TTZDateTime; const ADayString: AsciiString);
function MacroFirstWeekDay(const ADate: TTZDateTime;const AWeekDay: TTZWeekDay): TTZDay;
function MacroLastWeekDay(const ADate: TTZDateTime;const AWeekDay: TTZWeekDay): TTZDay;
function WeekDayOf(const ADate: TTZDateTime): TTZWeekDay;
function IsLeapYear(const AYear: integer): Boolean;
function WeekDayToString(const AWeekDay: TTZWeekDay): AsciiString;
function DayNameToNumber(const ADayName: AsciiString): TTZWeekDay;
function SecondsToShortTime(const ASeconds: Integer): String;
function SecondsToTime(const ASeconds: Integer; AAllowShortTime: Boolean = False): String;
function TimeToSeconds(const ATime: AsciiString): integer;
function GregorianDateToJulianDays(const Value: TTZDateTime): Integer;
function JulianDaysToGregorianDate(const Value: Integer): TTZDateTime;
function ResolveTimeZoneAbbreviation(const AZoneLetters, ARuleLetters: AsciiString;
  const IsDST: Boolean): AsciiString;
function ConvertToTimeForm(const SourceSecondsInDay, StandardTimeOffset, SaveTimeOffset: Integer;
  const SourceTimeForm, TargetTimeForm: TTZTimeForm): Integer;
function ConvertToTimeForm(const SourceDateTime: TTZDateTime; const StandardTimeOffset, SaveTimeOffset: Integer;
  const SourceTimeForm, TargetTimeForm: TTZTimeForm): TTZDateTime;

operator < (const ADate, BDate: TTZDateTime): Boolean;
operator > (const ADate, BDate: TTZDateTime): Boolean;
operator = (const ADate, BDate: TTZDateTime): Boolean;
operator <> (const ADate, BDate: TTZDateTime): Boolean;
operator >= (const ADate, BDate: TTZDateTime): Boolean;
operator <= (const ADate, BDate: TTZDateTime): Boolean;

const
  TTZMonthDaysCount: array [TTZMonth] of TTZDay = (
    31,28,31,30,31,30,31,31,30,31,30,31);
  TTZMonthDaysLeapYearCount: array [TTZMonth] of TTZDay = (
    31,29,31,30,31,30,31,31,30,31,30,31);
  TTZShortMonthNames: array [TTZMonth] of AsciiString = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

implementation

uses
  DateUtils;

operator < (const ADate, BDate: TTZDateTime): Boolean;
begin
  Result := CompareDates(ADate, BDate) < 0;
end;

operator > (const ADate, BDate: TTZDateTime): Boolean;
begin
  Result := CompareDates(ADate, BDate) > 0;
end;

operator = (const ADate, BDate: TTZDateTime): Boolean;
begin
  Result := CompareDates(ADate, BDate) = 0;
end;

operator <> (const ADate, BDate: TTZDateTime): Boolean;
begin
  Result := CompareDates(ADate, BDate) <> 0;
end;

operator >= (const ADate, BDate: TTZDateTime): Boolean;
begin
  Result := CompareDates(ADate, BDate) >= 0;
end;

operator <= (const ADate, BDate: TTZDateTime): Boolean;
begin
  Result := CompareDates(ADate, BDate) <= 0;
end;

function ConvertToTimeForm(const SourceSecondsInDay, StandardTimeOffset, SaveTimeOffset: Integer;
  const SourceTimeForm, TargetTimeForm: TTZTimeForm): Integer;
var
  InvalidTimeForms: Boolean;
begin
  Result := SourceSecondsInDay;
  InvalidTimeForms := False;
  case SourceTimeForm of
    tztfUniversal: // UTC
    begin
      case TargetTimeForm of
        tztfUniversal: // UTC => UTC
          begin end;
        tztfStandard:  // UTC => STD
          Result := Result + StandardTimeOffset;
        tztfWallClock: // UTC => STD+DST
          Result := Result + StandardTimeOffset + SaveTimeOffset;
        else
          InvalidTimeForms := True;
      end;
    end;
    tztfWallClock: // STD+DST
    begin
      case TargetTimeForm of
        tztfUniversal: // STD+DST => UTC
          Result := Result - StandardTimeOffset - SaveTimeOffset;
        tztfStandard:  // STD+DST => STD
          Result := Result - SaveTimeOffset;
        tztfWallClock: // STD+DST => STD+DST
          begin end;
        else
          InvalidTimeForms := True;
      end;
    end;
    tztfStandard: // STD
    begin
      case TargetTimeForm of
        tztfUniversal: // STD => UTC
          Result := Result - StandardTimeOffset;
        tztfStandard:  // STD => STD
          begin end;
        tztfWallClock: // STD => STD+DST
          Result := Result + SaveTimeOffset;
        else
          InvalidTimeForms := True;
      end;
    end;
    else
      InvalidTimeForms := True;
  end;
  if InvalidTimeForms then
    raise TTZException.CreateFmt('Invalid time form conversion from "%d" to "%d".',
      [Ord(SourceTimeForm), Ord(TargetTimeForm)]);
end;

function ConvertToTimeForm(const SourceDateTime: TTZDateTime;
  const StandardTimeOffset, SaveTimeOffset: Integer;
  const SourceTimeForm, TargetTimeForm: TTZTimeForm): TTZDateTime;
begin
  Result := SourceDateTime;
  Result.SecsInDay := ConvertToTimeForm(Result.SecsInDay,
    StandardTimeOffset, SaveTimeOffset, SourceTimeForm, TargetTimeForm);
  FixUpTime(Result);
end;

function ResolveTimeZoneAbbreviation(const AZoneLetters, ARuleLetters: AsciiString;
  const IsDST: Boolean): AsciiString;
var
  ZoneNameCut: integer;
begin
  Result := AZoneLetters;

  // Placeholders "%s" in time zone abbreviations seem to be documented as lower case,
  // but use rfIgnoreCase flag in StringReplace just to be safe.
  Result := StringReplace(Result, '%s', ARuleLetters, [rfReplaceAll, rfIgnoreCase]);

  // When timezonename is XXX/YYY, XXX is no daylight and YYY is daylight saving.
  ZoneNameCut := Pos('/', Result);
  if ZoneNameCut > 0 then
  begin
    if not IsDST then
      // Use the XXX
      Result := Copy(Result, 1, ZoneNameCut - 1)
    else
      // Use the YYY
      Result := Copy(Result, ZoneNameCut + 1, Length(Result) - ZoneNameCut);
  end;
end;

procedure IsGregorianLeapException(const ADate: TTZDateTime);
begin
  if IsGregorianLeap(ADate) then
    raise TTZException.CreateFmt('Gregorian Leap, date does not exist. [%.4d.%.2d.%.2d]',
      [ADate.Year, ADate.Month, ADate.Day]);
end;

function IsGregorianLeap(const ADate: TTZDateTime): Boolean;
begin
  Result := False;
  if (ADate.Year = 1582) and (ADate.Month = 10) then
    if (ADate.Day > 4) and (ADate.Day < 15) then
      Result := True;
end;

function IsBeforeGregorianLeap(const ADate: TTZDateTime): Boolean;
begin
  if ADate.Year < 1582 then Exit(True);
  if ADate.Year > 1582 then Exit(False);
  //Now Year is 1582 :)
  if ADate.Month < 10 then Exit(True);
  if ADate.Month > 10 then Exit(False);
  //Now year 1582 and month 10
  if ADate.Day <= 4 Then Exit(True);
  Result := False;
end;

procedure FixUpTime(var ADate: TTZDateTime);
var
  Days: Integer;
begin
  if (ADate.SecsInDay < 0) or (ADate.SecsInDay >= (3600*24)) then
  begin
    Days := ADate.SecsInDay div (3600*24);
    if ADate.SecsInDay < 0 then
    begin
      if ADate.SecsInDay mod (3600*24) < 0 then
        Dec(Days);
    end;
    if Days <> 0 then
    begin
      ADate := IncDays(ADate, Days);
      Dec(ADate.SecsInDay, Days * (3600*24))
    end;
  end;
end;

procedure DateTimeToTime(const ADate: TTZDateTime; out AHour,
  AMinute, ASecond: BYTE);
var
  Temp: Integer;
begin
  Temp := ADate.SecsInDay;
  AHour := ADate.SecsInDay div 3600;
  Dec(Temp, AHour*3600);
  AMinute := Temp div 60;
  Dec(Temp, AMinute*60);
  ASecond := Temp;
end;

function MinDate(const ADate, BDate: TTZDateTime): TTZDateTime;
begin
  if ADate < BDate then
    Result := ADate
  else
    Result := BDate;
end;

function MaxDate(const ADate, BDate: TTZDateTime): TTZDateTime;
begin
  if ADate > BDate then
    Result := ADate
  else
    Result := BDate;
end;

function CompareDates(const ADate, BDate: TTZDateTime): Integer;
begin
  // Year
  if ADate.Year > BDate.Year then
    Exit(1)
  else if ADate.Year < BDate.Year then
    Exit(-1);
  // Month
  if ADate.Month > BDate.Month then
    Exit(1)
  else if ADate.Month < BDate.Month then
    Exit(-1);
  // Day
  if ADate.Day > BDate.Day then
    Exit(1)
  else if ADate.Day < BDate.Day then
    Exit(-1);
  // SecsInDay
  if ADate.SecsInDay > BDate.SecsInDay then
    Exit(1)
  else if ADate.SecsInDay < BDate.SecsInDay then
    Exit(-1);
  // Same
  Result := 0;
end;

function MonthNumberToShortName(const AMonthNumber: Integer): AsciiString;
begin
  if (AMonthNumber >= Low(TTZShortMonthNames)) and (AMonthNumber <= High(TTZShortMonthNames)) then
    Result := TTZShortMonthNames[AMonthNumber]
  else
    raise TTZException.CreateFmt('Invalid month number "%s"', [IntToStr(AMonthNumber)]);
end;

function MonthNumberFromShortName(const AMonthName: AsciiString): TTZMonth;
var
  AMonthNumber: TTZMonth;
begin
  for AMonthNumber := Low(TTZShortMonthNames) to High(TTZShortMonthNames) do
    if AMonthName = TTZShortMonthNames[AMonthNumber] then
      Exit(AMonthNumber);
  raise TTZException.CreateFmt('Invalid short month name "%s"', [AMonthName]);
end;

function WeekDayOf(const ADate: TTZDateTime): TTZWeekDay;
var
  TempYear: Integer;
  FirstDayOfYear: Integer;
  j: Integer;
  DaysAD: Integer;
begin
  IsGregorianLeapException(ADate);
  TempYear := ADate.Year - 1;
  //Is date before Gregory Pope change ?

  if IsBeforeGregorianLeap(ADate) then
    FirstDayOfYear:= 6+TempYear+(TempYear div 4)
  else
    FirstDayOfYear:= 1+TempYear+(TempYear div 4)-(TempYear div 100)+(TempYear div 400);

  DaysAD := FirstDayOfYear;
  if IsLeapYear(ADate.Year) then
  begin
    for j := 1 to ADate.Month-1 do
      Inc(DaysAD,TTZMonthDaysLeapYearCount[j]);
  end
  else
  begin
    for j := 1 to ADate.Month-1 do
      Inc(DaysAD, TTZMonthDaysCount[j]);
  end;
  Inc(DaysAD, ADate.Day-1);
  Result := TTZWeekDay((DaysAD mod 7)+1);
end;

function IsLeapYear(const AYear: integer): Boolean;
begin
  if AYear > 1582 then
  begin
    if (AYear mod 400) = 0 Then Exit(True); // All years every 400 years are leap years.
    if (AYear mod 100) = 0 then Exit(False); // Centuries are not leap years (except previous % 400)
  end;
  if (AYear mod 4) = 0 then Exit(True); // Each 4 years a leap year comes in play.
  Result := False;
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

function TimeFormToChar(const ATimeForm: TTZTimeForm): AsciiChar;
begin
  case ATimeForm of
    tztfWallClock: Result := 'w';
    tztfStandard:  Result := 's';
    tztfUniversal: Result := 'u';
    else
      Result := '?';
  end;
end;

function CharToTimeForm(const AChar: AsciiChar; out ATimeForm: TTZTimeForm): Boolean;
begin
  Result := True;
  case AChar of
    'w':         ATimeForm := tztfWallClock;
    's':         ATimeForm := tztfStandard;
    'u','g','z': ATimeForm := tztfUniversal;
    else
      Result := False;
  end;
end;

function ExtractTimeForm(var TimeStr: AsciiString; out TimeForm: TTZTimeForm): Boolean;
var
  TimeFormChar: AsciiChar;
begin
  Result := False;
  if Length(TimeStr) > 0 then
  begin
    TimeFormChar := TimeStr[Length(TimeStr)];
    Result := CharToTimeForm(TimeFormChar, TimeForm);
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
  out ATimeForm: TTZTimeForm; const ADefaultTimeForm: TTZTimeForm): TTZDateTime;
var
  TmpWord: AsciiString;
begin
  // Default values
  Result := MakeTZDate(TZ_YEAR_MAX, 1, 1, 0);
  ATimeForm := ADefaultTimeForm;

  // Year
  TmpWord := AIterator.GetNextWord;
  if TmpWord = '' then Exit;
  try
    Result.Year:=StrToInt(TmpWord);
  except on E: Exception do
    raise TTZException.Create('Invalid date in "until" fields');
  end;

  // Month
  TmpWord := AIterator.GetNextWord;
  if TmpWord = '' Then Exit;
  Result.Month := MonthNumberFromShortName(TmpWord);

  // Day
  TmpWord := AIterator.GetNextWord;
  if TmpWord = '' then Exit;
  Result.Day := StrToIntDef(TmpWord, 0);

  // Day is not a number, try to resolve macro
  if Result.Day = 0 then
    MacroSolver(Result, TmpWord);

  // Seconds
  TmpWord := AIterator.GetNextWord;
  if TmpWord = '' then Exit;
  ATimeForm := ExtractTimeFormDefault(TmpWord, ADefaultTimeForm);
  Result.SecsInDay := TimeToSeconds(TmpWord);
end;

// This function check for some macros to specify a given day
// in a month and a year, like "lastSun" or "Mon>=15".
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
  // Check if it is a regular number
  j:=StrToIntDef(ADayString,-1);
  if j<>-1 then
  begin
    ADate.Day:=j;
    Exit;
  end;

  // Check if it is a conditional macro
  ConditionalMacro:=false;
  ConditionalBegin:=0;
  ConditionalEnd:=0;
  for j := 1 to Length(ADayString) do
  begin
    if ADayString[j] in ['<','=','>'] then
    begin
      ConditionalMacro:=true;
      ConditionalBegin:=j;
      for k := j+1 to Length(ADayString) do
      begin
        if not (ADayString[k] in ['<','=','>']) then
        begin
          ConditionalEnd:=k;
          Break;
        end;
      end;
      Break;
    end;
  end;

  if ConditionalMacro then
  begin
    if (ConditionalEnd=0) or (ConditionalBegin=0) then
      raise TTZException.Create('Macro expansion not possible: Unrecognised conditional part');
    ConditionA:=Copy(ADayString,1,ConditionalBegin-1);
    ConditionOp:=Copy(ADayString,ConditionalBegin,ConditionalEnd-ConditionalBegin);
    ConditionB:=Copy(ADayString,ConditionalEnd,Length(ADayString)-ConditionalEnd+1);
    WeekDay:=DayNameToNumber(ConditionA);
    j:=StrToInt(ConditionB);
    if ConditionOp='>' then
    begin
      ADate.Day:=j+1;
      ADate.Day:=MacroFirstWeekDay(ADate,WeekDay);
    end
    else if ConditionOp='>=' then
    begin
      ADate.Day:=j;
      ADate.Day:=MacroFirstWeekDay(ADate,WeekDay);
    end
    else if ConditionOp='<' then
    begin
      ADate.Day:=j-1;
      ADate.Day:=MacroLastWeekDay(ADate,WeekDay);
    end
    else if ConditionOp='<=' then
    begin
      ADate.Day:=j;
      ADate.Day:=MacroLastWeekDay(ADate,WeekDay);
    end
    else
      raise TTZException.Create('Macro expansion not possible: Unknown condition operator');
  end
  else
  begin
    // It is not a conditional macro, so it could be firstXXX or lastXXX
    if LeftStr(ADayString,5)='first' then
    begin
      WeekDay:=DayNameToNumber(Copy(ADayString,6,Length(ADayString)-5));
      ADate.Day:=Low(TTZDay);
      ADate.Day:=MacroFirstWeekDay(ADate,WeekDay);
    end
    else if LeftStr(ADayString,4)='last' then
    begin
      WeekDay:=DayNameToNumber(Copy(ADayString,5,Length(ADayString)-4));
      ADate.Day:=High(TTZDay);
      ADate.Day:=MacroLastWeekDay(ADate,WeekDay);
    end
    else
      raise TTZException.Create('Macro expansion not possible: Unrecognised macro');
  end;
end;

function SecondsToShortTime(const ASeconds: Integer): String;
begin
  Result := SecondsToTime(ASeconds, True);
end;

function SecondsToTime(const ASeconds: Integer; AAllowShortTime: Boolean = False): String;
var
  H, M, S: Integer;  // Hours, Minutes, Seconds
begin
  S := Abs(ASeconds);
  H := S div 3600;
  Dec(S, H * 3600);
  M := S div 60;
  Dec(S, M * 60);

  // Negative
  if ASeconds < 0 then
    Result := '-'
  else
    Result := '';

  // Hours
  if H < 10 then
    Result := Result + '0';
  Result := Result + IntToStr(H);

  // Minutes
  Result := Result + ':';
  if M < 10 then
    Result := Result + '0';
  Result := Result + IntToStr(M);

  // Seconds (optional)
  if not AAllowShortTime or (S <> 0) then
  begin
    Result := Result + ':';
    if S < 10 then
      Result := Result + '0';
    Result := Result + IntToStr(S);
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
  // Time could be expressed in:
  //   [-]h = hours
  //   [-]h:m = hours:minutes
  //   [-]h:m:s = hours:minutes:seconds
  // So count the amount of ':' to get the format

  if ATime[1]='-' then
  begin
    Sign:=-1; //Negative seconds...
    TmpTime:=Copy(ATime,2,Length(ATime)-1);
  end
  else
  begin
    Sign:=1;  //Positive seconds...
    TmpTime:=ATime;
  end;

  TwoColons:=0;
  for j := 1 to Length(TmpTime) do
  begin
    if TmpTime[j] = ':' then
      Inc(TwoColons);
  end;

  case TwoColons of
    // Format is "h"
    0:  begin
          Result:=StrToInt(TmpTime)*3600;
        end;
    // Format is "hh:mm"
    1:  begin
          TimeIterator:=TTZLineIterate.Create(TmpTime,':');
          try
            Hours:=StrToInt(TimeIterator.GetNextWord);
            Minutes:=StrToInt(TimeIterator.GetNextWord);
            Result:=Hours*3600+Minutes*60;
          except on E: Exception do
            begin
              TimeIterator.Free;
              raise TTZException.Create('Failed to parse time: ' + ATime);
            end;
          end;
          TimeIterator.Free;
        end;
    // Format is "hh:mm:ss"
    2:  begin
          TimeIterator:=TTZLineIterate.Create(TmpTime,':');
          try
            Hours:=StrToInt(TimeIterator.GetNextWord);
            Minutes:=StrToInt(TimeIterator.GetNextWord);
            Seconds:=StrToInt(TimeIterator.GetNextWord);
            Result:=Hours*3600+Minutes*60+Seconds;
          except on E: Exception do
            begin
              TimeIterator.Free;
              raise TTZException.Create('Failed to parse time: ' + ATime);
            end;
          end;
          TimeIterator.Free;
        end;
    else
        begin
          TimeIterator.Free;
          raise TTZException.Create('Failed to parse time: ' + ATime);
        end;
  end;

  Result:=Result * Sign;
end;

function MacroFirstWeekDay(const ADate: TTZDateTime; const AWeekDay: TTZWeekDay): TTZDay;
var
  FirstWDInMonth: TTZWeekDay;
  TheDay: Integer;
begin
  FirstWDInMonth := WeekDayOf(ADate);
  TheDay := ADate.Day;

  if FirstWDInMonth < AWeekDay then
    Inc(TheDay, (Integer(AWeekDay)-Integer(FirstWDInMonth)))
  else if FirstWDInMonth>AWeekDay then
    Inc(TheDay, 7-(Integer(FirstWDInMonth)-Integer(AWeekDay)));

  if IsLeapYear(ADate.Year) then
  begin
    if TheDay>TTZMonthDaysLeapYearCount[ADate.Month] then
      TheDay := 0; // Invalidate day
  end
  else
  begin
    if TheDay>TTZMonthDaysCount[ADate.Month] then
      TheDay := 0; // Invalidate day
  end;

  if TheDay < 1 then
    raise TTZException.CreateFmt('No valid first week day for "%s" after %.4d.%.2d.%.2d',
      [WeekDayToString(AWeekDay), ADate.Year, ADate.Month, ADate.Day]);

  Result := TheDay;
end;

function MacroLastWeekDay(const ADate: TTZDateTime; const AWeekDay: TTZWeekDay): TTZDay;
var
  TmpDate: TTZDateTime;
  LastWDInMonth: TTZWeekDay;
  TheDay: Integer;
begin
  TmpDate := ADate;
  if not IsLeapYear(TmpDate.Year) then
  begin
    if TmpDate.Day > TTZMonthDaysCount[TmpDate.Month] then
      TmpDate.Day := TTZMonthDaysCount[TmpDate.Month];
  end
  else
  begin
    if TmpDate.Day > TTZMonthDaysLeapYearCount[TmpDate.Month] then
      TmpDate.Day := TTZMonthDaysLeapYearCount[TmpDate.Month];
  end;

  LastWDInMonth:=WeekDayOf(TmpDate);
  TheDay:=TmpDate.Day;
  if LastWDInMonth < AWeekDay then
    Dec(TheDay, 7-(integer(AWeekDay)-integer(LastWDInMonth)))
  else if LastWDInMonth > AWeekDay then
    Dec(TheDay, (integer(LastWDInMonth)-integer(AWeekDay)));

  if TheDay < 1 then
    raise TTZException.CreateFmt('No valid last week day for "%s" before %.4d.%.2d.%.2d',
      [WeekDayToString(AWeekDay), ADate.Year, ADate.Month, ADate.Day]);

  Result := TheDay;
end;

function DateTimeToStr(const ADate: TTZDateTime): String;
var
  H, M, S: BYTE;
begin
  DateTimeToTime(ADate, H, M, S);
  Result := Format('%.4d.%.2d.%.2d %.2d:%.2d:%.2d',
    [ADate.Year, ADate.Month, ADate.Day, H, M, S]);
end;

function IncSeconds(const ADate: TTZDateTime; const ASeconds: Integer): TTZDateTime;
begin
  Result := ADate;
  Inc(Result.SecsInDay, ASeconds);
  FixUpTime(Result);
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

function MakeTZDate(const Year, Month, Day, SecsInDay: Integer): TTZDateTime; inline;
begin
  Result.Year := Year;
  Result.Month := Month;
  Result.Day := Day;
  Result.SecsInDay := SecsInDay;
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

