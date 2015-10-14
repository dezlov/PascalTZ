unit uPascalTZ_Tools;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, uPascalTZ_Types;

procedure FixUpTime(var ADate: TTZDateTime);
function IncDays(const ADate: TTZDateTime; const ADays: integer): TTZDateTime;
procedure DateTimeToTime(const ADate: TTZDateTime; out AHour,AMinute,ASecond: BYTE);
function DateTimeToStr(const ADate: TTZDateTime): String;
function TZDateToPascalDate(const ADate: TTZDateTime): TDateTime;
function PascalDateToTZDate(const ADate: TDateTime): TTZDateTime;
function MonthNumberFromShortName(const AMonth: AsciiString): TTZMonth;
function CompareDates(const ADate,BDate: TTZDateTime): integer;
function IsGregorianLeap(const ADate: TTZDateTime): Boolean;
procedure IsGregorianLeapException(const ADate: TTZDateTime);
function IsBeforeGregorianLeap(const ADate: TTZDateTime): Boolean;
function ParseUntilFields(const AIterator: TTZLineIterate; out AGMTTime: Boolean): TTZDateTime;
procedure MacroSolver(var ADate: TTZDateTime; const ADayString: AsciiString);
function MacroFirstWeekDay(const ADate: TTZDateTime;const AWeekDay: TTZWeekDay; const ASinceMonthDay: TTZDay=1): TTZDay;
function MacroLastWeekDay(const ADate: TTZDateTime;const AWeekDay: TTZWeekDay; const ASinceMonthDay: TTZDay=31): TTZDay;
function WeekDayOf(const ADate: TTZDateTime): TTZWeekDay;
function IsLeapYear(const AYear: integer): Boolean;
function WeekDayToString(const AWeekDay: TTZWeekDay): AsciiString;
function DayNameToNumber(const ADayName: AsciiString): TTZWeekDay;
function TimeToSeconds(const ATime: AsciiString): integer;
function ElapsedDaysSinceADToDate(const AElapsedDays: integer): TTZDateTime;
function ElapsedDaysSinceAD(const ADate: TTZDateTime): integer;

const
  TTZMonthDaysCount:
            array [TTZMonth] of TTZDay=(31,28,31,30,31,30,31,31,30,31,30,31);
  TTZMonthDaysLeapYearCount:
            array [TTZMonth] of TTZDay=(31,29,31,30,31,30,31,31,30,31,30,31);

implementation

uses
  DateUtils;

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

function ElapsedDaysSinceAD(const ADate: TTZDateTime): integer;
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

function ParseUntilFields(const AIterator: TTZLineIterate;
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
  if not (TmpWord[Length(TmpWord)] in ['0'..'9']) then begin
    Letter:=TmpWord[Length(TmpWord)];
    if Letter='s' then begin
      AGMTTime:=false;
    end;
    SetLength(TmpWord,Length(TmpWord)-1);
  end;
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
      Raise TTZException.Create('Macro expansion not possible: Unrecognised conditional part');
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
      Raise TTZException.Create('Macro expansion not possible: Unknown condition operator');
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

function MacroLastWeekDay(const ADate: TTZDateTime;
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

function DateTimeToStr(const ADate: TTZDateTime): String;
var
  H,M,S: BYTE;
begin
  DateTimeToTime(ADate,H,M,S);
  Result:=format('%.4d.%.2d.%.2d %.2d:%.2d:%.2d',[ADate.Year,ADate.Month,ADate.Day,H,M,S]);
end;

function IncDays(const ADate: TTZDateTime; const ADays: integer
  ): TTZDateTime;
var
  Elapsed: integer;
begin
  Elapsed:=ElapsedDaysSinceAD(ADate);
  inc(Elapsed,ADays);
  Result:=ElapsedDaysSinceADToDate(Elapsed);
  Result.SecsInDay:=Adate.SecsInDay;
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

function ElapsedDaysSinceADToDate(const AElapsedDays: integer
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

end.

