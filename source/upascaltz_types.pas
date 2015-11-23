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
  TZ_FILE_CONTINENT_AFRICA       = 'africa';
  TZ_FILE_CONTINENT_ANTARCTICA   = 'antarctica';
  TZ_FILE_CONTINENT_ASIA         = 'asia';
  TZ_FILE_CONTINENT_AUSTRALASIA  = 'australasia';
  TZ_FILE_CONTINENT_EUROPE       = 'europe';
  TZ_FILE_CONTINENT_NORTHAMERICA = 'northamerica';
  TZ_FILE_CONTINENT_SOUTHAMERICA = 'southamerica';

  TZ_FILE_OTHER_BACKWARD    = 'backward';
  TZ_FILE_OTHER_BACKZONE    = 'backzone';
  TZ_FILE_OTHER_ETCETERA    = 'etcetera';
  TZ_FILE_OTHER_FACTORY     = 'factory';
  TZ_FILE_OTHER_LEAPSECONDS = 'leapseconds';
  TZ_FILE_OTHER_PACIFICNEW  = 'pacificnew';
  TZ_FILE_OTHER_SYSTEMV     = 'systemv';

const
  TZ_FILES_STANDARD: array [1..9] of String = (
    TZ_FILE_CONTINENT_AFRICA,
    TZ_FILE_CONTINENT_ANTARCTICA,
    TZ_FILE_CONTINENT_ASIA,
    TZ_FILE_CONTINENT_AUSTRALASIA,
    TZ_FILE_CONTINENT_EUROPE,
    TZ_FILE_CONTINENT_NORTHAMERICA,
    TZ_FILE_CONTINENT_SOUTHAMERICA,
    TZ_FILE_OTHER_BACKWARD,
    TZ_FILE_OTHER_ETCETERA
  );

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

  TZ_YEAR_MAX = 9999;

type
  PAsciiChar=^AsciiChar;
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

const
  // ZIC man page:
  // For RULE IN/ON/AT fields and ZONE UNTIL fields,
  // in the absence of an indicator, wall clock time is assumed.
  TZ_TIME_FORM_DEFAULT = tztfWallClock;

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
  function GetBeginDate(const AYear: Integer): TTZDateTime;
end;

TTZRuleList = specialize TFPGObjectList<TTZRule>;

TTZRuleGroup = class
private
  FList: TTZRuleList;
  FName: AsciiString;
public
  constructor Create(const AName: AsciiString);
  destructor Destroy; override;
  property List: TTZRuleList read FList;
  property Name: AsciiString read FName write FName;
end;

TTZRuleGroupList = specialize TFPGObjectList<TTZRuleGroup>;

TTZDateListItem=class
public
  Date: TTZDateTime;
  Rule: TTZRule;
  constructor Create(const ARule: TTZRule; const ADate: TTZDateTime);
end;

TTZDateList = specialize TFPGObjectList<TTZDateListItem>;

TTZDateStack = class(TTZDateList)
public
  procedure SortByDate;
end;

TTZZone=class
public
  // Standard zone definition attributes:
  Name: AsciiString;
  Offset: integer; //seconds
  RuleName: AsciiString;
  FixedSaveTime: integer; //seconds
  TimeZoneLetters: AsciiString;
  ValidUntilForm: TTZTimeForm;
  ValidUntil: TTZDateTime;
  // Additionally calculated attributes:
  ValidUntilSaveTime: Integer;
  ValidUntilUTC: TTZDateTime;
end;

TTZZoneList = specialize TFPGObjectList<TTZZone>;

TTZZoneListHelper = class helper for TTZZoneList
public
  procedure SortByValidUntilUTC;
end;

TTZZoneGroup = class
private
  FList: TTZZoneList;
  FName: AsciiString;
public
  constructor Create(const AName: AsciiString);
  destructor Destroy; override;
  property List: TTZZoneList read FList;
  property Name: AsciiString read FName write FName;
end;

TTZZoneGroupList = specialize TFPGObjectList<TTZZoneGroup>;

TTZLink=class
public
  LinkTarget: AsciiString; // existing zone name
  LinkName: AsciiString; // alternative zone name
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

uses
  uPascalTZ_Tools;


function TTZRule.GetBeginDate(const AYear: Integer): TTZDateTime;
begin
  Result := MakeTZDate(AYear, Self.InMonth, 1, 0);
  MacroSolver(Result, Self.OnRule);
  Result.SecsInDay := Self.AtHourTime;
end;

constructor TTZRuleGroup.Create(const AName: AsciiString);
begin
  FName := AName;
  FList := TTZRuleList.Create(True); // FreeObjects = True
end;

destructor TTZRuleGroup.Destroy;
begin
  FreeAndNil(FList);
end;

function CompareZonesByValidUntilUTC(const ItemA, ItemB: TTZZone): Integer;
begin
  Result := CompareDates(ItemA.ValidUntilUTC, ItemB.ValidUntilUTC);
end;

procedure TTZZoneListHelper.SortByValidUntilUTC;
begin
  Self.Sort(@CompareZonesByValidUntilUTC);
end;

constructor TTZZoneGroup.Create(const AName: AsciiString);
begin
  FName := AName;
  FList := TTZZoneList.Create(True); // FreeObjects = True
end;

destructor TTZZoneGroup.Destroy;
begin
  FreeAndNil(FList);
end;

constructor TTZDateListItem.Create(const ARule: TTZRule; const ADate: TTZDateTime);
begin
  Self.Rule := ARule;
  Self.Date := ADate;
end;

function CompareDateListItems(const ItemA, ItemB: TTZDateListItem): Integer;
begin
  Result := CompareDates(ItemA.Date, ItemB.Date);
end;

procedure TTZDateStack.SortByDate;
begin
  Self.Sort(@CompareDateListItems);
end;

{ TTZLineIterate }

function TTZLineIterate.GetNextWord: AsciiString;
const
  CHAR_SPACE = #32;
  CHAR_TAB   = #09;
var
  BeginPos: integer;
begin
  if (FIterateChar=CHAR_SPACE) or (FIterateChar=CHAR_TAB) then
  begin
    while (Position<=LineSize) and ((Line[Position]=CHAR_SPACE) or (Line[Position]=CHAR_TAB)) do
      inc(Position);
    BeginPos:=Position;
    while (Position<=LineSize) and ((Line[Position]<>CHAR_SPACE) and (Line[Position]<>CHAR_TAB)) do
      inc(Position);
  end
  else
  begin
    if Line[Position]=FIterateChar then
      inc(Position);
    BeginPos:=Position;
    while (Position<=LineSize) and (Line[Position]<>FIterateChar) do
      inc(Position);
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

