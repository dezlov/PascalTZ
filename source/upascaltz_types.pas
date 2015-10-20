unit uPascalTZ_Types;

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
  Classes, SysUtils, FGL;

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
  TParseSequence=(TTzParseRule,TTzParseZone,TTzParseLink,TTzParseFinish);
  TTZMonth=  1..12;
  TTZDay=    1..31;
  TTZHour=   0..23;
  TTZMinute= 0..59;
  TTZSecond= 0..59;
  TTZWeekDay=(eTZSunday=1,eTZMonday,eTZTuesday,eTZWednesday,eTZThursday,eTZFriday,eTZSaturday);
  TTZTimeForm=(tztfWallClock, tztfStandard, tztfUniversal);

type
TTZDateTime=record
  Year: smallint;
  Month: BYTE;
  Day: BYTE;
  SecsInDay: integer;
end;

TTZRule=class
public
  Name: AsciiString;
  FromYear: integer;
  ToYear: integer;
  InMonth: BYTE;
  OnRule: AsciiString;
  AtHourTimeForm: TTZTimeForm;
  AtHourTime: integer; //seconds
  SaveTime: integer;   //seconds
  TimeZoneLetters: AsciiString;
end;

TTZRuleList = specialize TFPGObjectList<TTZRule>;

TTzZone=class
public
  Name: AsciiString;
  Offset: integer; //seconds
  RuleName: AsciiString;
  RuleFixedOffset: integer; //seconds
  TimeZoneLetters: AsciiString;
  RuleValidUntilForm: TTZTimeForm;
  RuleValidUntil: TTZDateTime;
end;

TTZZoneList = specialize TFPGObjectList<TTZZone>;

TTZLink=class
public
  LinkFrom: AsciiString; // existing zone name
  LinkTo: AsciiString; // alternative zone name
end;

TTZLinkList = specialize TFPGObjectList<TTZLink>;

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

