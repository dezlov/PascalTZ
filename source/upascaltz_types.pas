unit uPascalTZ_Types;

{$mode objfpc}{$H+}

{
  File: upascaltz_types.pas

  Helper types and routines to be used in PascalTZ unit.
}

interface

uses
  Classes, SysUtils; 

const
  // Longest 'backward' zone name is 32 characters:
  //   "America/Argentina/ComodRivadavia"
  // Longest 'current' zones names are 30 characters:
  //   "America/Argentina/Buenos_Aires"
  //   "America/Argentina/Rio_Gallegos"
  //   "America/North_Dakota/New_Salem"
  TZ_RULENAME_SIZE=12;
  TZ_ZONENAME_SIZE=32; // max 32 characters is 'backward' compatible!
  TZ_TIMEZONELETTERS_SIZE=8;
  TZ_ONRULE_SIZE=7;

  TZ_SECONDSIN_MINUTE=60;
  TZ_SECONDSIN_HOUR=TZ_SECONDSIN_MINUTE*60;
  TZ_SECONDSIN_DAY=TZ_SECONDSIN_HOUR*24;

  LAST_PREGREGORIAN_DAY=577737;
  FIRST_POSTGREGORIAN_DAY=LAST_PREGREGORIAN_DAY+1;

type
  AsciiChar=AnsiChar;
  AsciiString=AnsiString;
  TTZRuleName=array [0..TZ_RULENAME_SIZE-1] of AsciiChar;
  TTZZoneName=array [0..TZ_ZONENAME_SIZE-1] of AsciiChar;
  TTZTimeZoneLetters=array [0..TZ_TIMEZONELETTERS_SIZE-1] of AsciiChar;
  TTZOnRule=array [0..TZ_ONRULE_SIZE-1] of AsciiChar;
  TParseSequence=(TTzParseRule,TTzParseZone,TTzParseLink,TTzParseFinish);
  TTZMonth=  1..12;
  TTZDay=    1..31;
  TTZHour=   0..23;
  TTZMinute= 0..59;
  TTZSecond= 0..59;
  TTZWeekDay=(eTZSunday=1,eTZMonday,eTZTuesday,eTZWednesday,eTZThursday,eTZFriday,eTZSaturday);

type
TTZDateTime=record
  Year: smallint;
  Month: BYTE;
  Day: BYTE;
  SecsInDay: integer;
end;

TTZRule=record
  Name: TTZRuleName;
  FromYear: integer;
  ToYear: integer;
  InMonth: BYTE;
  OnRule: TTZOnRule;
  AtHourGMT: Boolean;
  AtHourTime: integer; //seconds
  SaveTime: integer;   //seconds
  TimeZoneLetters: TTZTimeZoneLetters;
end;

TTzZone=record
  Name: TTZZoneName;
  Offset: integer; //seconds
  RuleIndex: integer; //Rule index in rules array.
  RuleFixedOffset: integer; //seconds
  TimeZoneLetters: TTZTimeZoneLetters;
  RuleValidUntilGMT: Boolean;
  RuleValidUntil: TTZDateTime;
end;

{ TTZLineIterate }

TTZLineIterate = class(TObject)
private
  Position: integer;
  Line: AsciiString;
  LineSize: Integer;
protected
  FIterateChar: AsciiChar;
public
  property IterateChar: AsciiChar read FIterateChar write FIterateChar;
  property CurrentLine: AsciiString read Line;
  function GetNextWord: AsciiString;
  constructor Create(const ALine: AsciiString; const AIterateChar: AsciiChar=#32);
end;

{ TExceptionTZ }

TTZException = class(Exception);


implementation

const
  CHAR_SPACE=#32;
  CHAR_TAB=  #09;

{ TTZLineIterate }

function TTZLineIterate.GetNextWord: AsciiString;
var
  BeginPos: integer;
begin
  if (FIterateChar=CHAR_SPACE) or (FIterateChar=CHAR_TAB) then begin
    while (Position<=LineSize) and ((Line[Position]=CHAR_SPACE) or (Line[Position]=CHAR_TAB)) do begin
      inc(Position);
    end;
    BeginPos:=Position;
    while (Position<=LineSize) and ((Line[Position]<>CHAR_SPACE) and (Line[Position]<>CHAR_TAB)) do begin
      inc(Position);
    end;
  end else begin
    if Line[Position]=FIterateChar then inc(Position);
    BeginPos:=Position;
    while (Position<=LineSize) and (Line[Position]<>FIterateChar) do begin
      inc(Position);
    end;
  end;
  Result:=Copy(Line,BeginPos,Position-BeginPos);
end;

constructor TTZLineIterate.Create(const ALine: AsciiString;
  const AIterateChar: AsciiChar);
begin
  Line:=ALine;
  Position:=1;
  LineSize:=Length(ALine);
  FIterateChar:=AIterateChar;
end;

end.

