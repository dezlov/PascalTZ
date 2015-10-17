unit uPascalTZ;

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
  Classes,SysUtils,uPascalTZ_Types;

type
  { TPascalTZ }

  TPascalTZ = class(TObject)
  private
    FDatabaseLoaded: Boolean;
    ParseStatusTAG: AsciiString;
    ParseStatusPreviousZone: AsciiString;
    function FindZoneForDate(const ZoneIndexStart: integer;const ADateTime: TTZDateTime): integer;
    function FindZoneName(const AZone: AsciiString; const AIncludeLinks: Boolean): integer;
    function FindLinkName(const AZoneLinkTo: AsciiString): integer;
    function FindRuleName(const AName: AsciiString): Integer;
    procedure SortRules;
    function GetCountZones: Integer;
    function GetCountRules: Integer;
    procedure CheckCanLoadDatabase;
  protected
    FDetectInvalidLocalTimes: Boolean;
    FLineCounter: integer;
    FRules: TTZRuleList;
    FZones: TTZZoneList;
    FLinks: TTZLinkList;
    procedure ParseLine(const ALineNumber: Integer; const ALine: AsciiString; const AParseSequence: TParseSequence);
    procedure BareParseLine(const ALine: AsciiString; const AParseSequence: TParseSequence);
    procedure BareParseZone(const AIterator: TTZLineIterate; const AZone: AsciiString);
    procedure BareParseRule(const AIterator: TTZLineIterate);
    procedure BareParseLink(const AIterator: TTZLineIterate);
    function LocalTimeToGMT(const ADateTime: TTZDateTime; const AFromZone: String): TTZDateTime;
    function GMTToLocalTime(const ADateTime: TTZDateTime; const AToZone: String;out ATimeZoneName: String): TTZDateTime;
    function Convert(const ADateTime: TTZDateTime; const AZone: String;
      const AConvertToZone: Boolean): TTZDateTime; overload;
    function Convert(const ADateTime: TTZDateTime; const AZone: String;
      const AConvertToZone: Boolean; out ATimeZoneName: String): TTZDateTime; overload;
  public
    property CountZones: Integer read GetCountZones;
    property CountRules: Integer read GetCountRules;
    property ProcessedLines: integer read FLineCounter;
    property DetectInvalidLocalTimes: Boolean read FDetectInvalidLocalTimes write FDetectInvalidLocalTimes;
    procedure GetTimeZoneNames(const AZones: TStringList; const AOnlyGeoZones: Boolean=true);
    function TimeZoneExists(const AZone: String): Boolean;
    function GMTToLocalTime(const ADateTime: TDateTime; const AToZone: String): TDateTime; overload;
    function GMTToLocalTime(const ADateTime: TDateTime; const AToZone: String; out ATimeZoneSubFix: String): TDateTime; overload;
    function LocalTimeToGMT(const ADateTime: TDateTime; const AFromZone: String): TDateTime;
    function TimeZoneToTimeZone(const ADateTime: TDateTime; const AFromZone, AToZone: String): TDateTime; overload;
    function TimeZoneToTimeZone(const ADateTime: TDateTime; const AFromZone, AToZone: String; out ATimeZoneSubFix: String): TDateTime; overload;
    function ParseDatabaseFromFile(const AFileName: String): Boolean;
    function ParseDatabaseFromFiles(const AFileNames: array of String): Boolean;
    function ParseDatabaseFromStream(const AStream: TStream): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  RtlConsts, DateUtils, uPascalTZ_Tools;

{ TPascalTZ }

function TPascalTZ.FindRuleName(const AName: AsciiString): Integer;
var
  j: integer;
begin
  Result:=-1;
  for j := 0 to FRules.Count-1 do begin
    if FRules[j].Name=AName then begin
      Result:=j;
      break;
    end;
  end;
end;

function TPascalTZ.FindLinkName(const AZoneLinkTo: AsciiString): integer;
var
  j: integer;
begin
  Result:=-1;
  for j := 0 to FLinks.Count-1 do begin
    if FLinks[j].LinkTo=AZoneLinkTo then begin
      Result:=j;
      Break;
    end;
  end;
end;

function TPascalTZ.FindZoneName(const AZone: AsciiString; const AIncludeLinks: Boolean): integer;
var
  LinkIndex: Integer;
  TargetZone: AsciiString;
  j: integer;
begin
  Result:=-1;
  TargetZone := AZone;
  if AIncludeLinks then
  begin
    LinkIndex := FindLinkName(AZone);
    if LinkIndex >= 0 then
      TargetZone := FLinks[LinkIndex].LinkFrom;
  end;
  for j := 0 to FZones.Count-1 do begin
    if FZones[j].Name=TargetZone then begin
      Result:=j;
      Break;
    end;
  end;
end;

function TPascalTZ.FindZoneForDate(const ZoneIndexStart: integer;const ADateTime: TTZDateTime): integer;
var
  AZone: AsciiString;
  j: integer;
begin
  AZone:=FZones[ZoneIndexStart].Name;
  j:=ZoneIndexStart;
  Result:=-1;
  while (j<=FZones.Count-1) and (FZones[j].Name=AZone) do begin
    if CompareDates(FZones[j].RuleValidUntil, ADateTime)=1 then begin
      Result:=j;
      break;
    end;
    inc(j);
  end;
end;

function SortCompareRule(const Item1, Item2: TTZRule): Integer;
const
  eSortCompareBigger = 1;
  eSortCompareLesser = -1;
  eSortCompareEqual  = 0;
begin
  if Item1.Name>Item2.Name Then begin
    Exit(eSortCompareBigger);
  end else if Item1.Name<Item2.Name Then begin
    Exit(eSortCompareLesser);
  end;
  if Item1.FromYear>Item2.FromYear then begin
    Exit(eSortCompareBigger);
  end else if Item1.FromYear<Item2.FromYear Then begin
    Exit(eSortCompareLesser);
  end;
  if Item1.ToYear>Item2.ToYear then begin
    Exit(eSortCompareBigger);
  end else if Item1.ToYear<Item2.ToYear Then begin
    Exit(eSortCompareLesser);
  end;
  if Item1.InMonth>Item2.InMonth then begin
    Exit(eSortCompareBigger);
  end else if Item1.InMonth<Item2.InMonth Then begin
    Exit(eSortCompareLesser);
  end;
  //This should not happend
//  Raise TTZException.CreateFmt('Invalid rule sorting',[]);
  Result:=eSortCompareEqual;
end;

// TODO: Investigate whether SortRules is actually needed for correct operation.
procedure TPascalTZ.SortRules;
begin
  FRules.Sort(@SortCompareRule);
end;

procedure TPascalTZ.ParseLine(const ALineNumber: Integer;
  const ALine: AsciiString; const AParseSequence: TParseSequence);
begin
  try
    BareParseLine(ALine, AParseSequence);
  except on E: Exception do
    raise TTZException.CreateFmt('Parse error at line %s: "%s" [%s]',
      [IntToStr(ALineNumber), ALine, E.Message]);
  end;
end;

procedure TPascalTZ.BareParseLine(const ALine: AsciiString; const AParseSequence: TParseSequence);
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
    Parser:=TTZLineIterate.Create(PreParseLine);
    try
      ZoneContinue:=false;
      if (PreParseLine[1]=#32) or (PreParseLine[1]=#9) then begin
        //Its a continuation
        if ParseStatusTAG<>'Zone' then begin
          raise TTZException.CreateFmt('Continuation for line tag "%s" is not allowed', [ParseStatusTAG]);
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
          BareParseZone(Parser,ParseStatusPreviousZone);
        end;
      end else if (ParseStatusTAG='Rule') then begin
        if (AParseSequence=TTzParseRule) then begin
          BareParseRule(Parser);
        end;
      end else if ParseStatusTAG='Link' then begin
        if (AParseSequence=TTzParseLink) then begin
          BareParseLink(Parser);
        end;
      end else begin
        raise TTZException.CreateFmt('Unknown line tag "%s"', [ParseStatusTAG]);
      end;
    finally
      Parser.Free;
    end;
  end;
end;

procedure TPascalTZ.BareParseZone(const AIterator: TTZLineIterate;
  const AZone: AsciiString);
var
  Index: integer;
  RuleName: AsciiString;
  RuleTmpIndex: integer;
  TmpWord: AsciiString;
  NewZone: TTZZone;
begin
  NewZone := TTZZone.Create;
  Index := FZones.Add(NewZone); // list owns the new item

  begin
    //First is the zone name
    if Length(AZone)>TZ_ZONENAME_SIZE then begin
      FZones.Delete(Index); //Remove put information
      Raise TTZException.CreateFmt('Name on Zone line "%s" too long. (Increase source code TZ_ZONENAME_SIZE)',[AIterator.CurrentLine]);
    end;
    NewZone.Name:=AZone;

    //Now check the offset
    TmpWord:=AIterator.GetNextWord; //Offset
    NewZone.Offset:=TimeToSeconds(TmpWord);

    //Now check the rules...
    RuleName:=AIterator.GetNextWord;
    if RuleName='' Then begin
      FZones.Delete(Index); //Remove put information
      Raise TTZException.CreateFmt('Rule on Zone line "%s" empty.',[AIterator.CurrentLine]);
    end;
    if RuleName='-' then begin
      //Standard time (Local time)
      NewZone.RuleFixedOffset:=0;
      NewZone.RuleName:='';
    end else if RuleName[1] in ['0'..'9'] then begin
      //Fixed offset time to get standard time (Local time)
      NewZone.RuleFixedOffset:=TimeToSeconds(RuleName);
      NewZone.RuleName:='';
    end else begin
      RuleTmpIndex:=FindRuleName(RuleName);
      if RuleTmpIndex<0 then begin
        FZones.Delete(Index); //Remove put information
        Raise TTZException.CreateFmt('Rule on Zone line "%s" not found.',[AIterator.CurrentLine]);
      end else begin
        NewZone.RuleName:=RuleName;
        NewZone.RuleFixedOffset:=0; //Nonsense value.
      end;
    end;

    //Now its time for the format (GMT, BST, ...)
    TmpWord:=AIterator.GetNextWord;
    if Length(TmpWord)>TZ_TIMEZONELETTERS_SIZE Then begin
      FZones.Delete(Index); //Remove put information
      Raise TTZException.CreateFmt('Format on Zone line "%s" too long. (Increase source code TZ_TIMEZONELETTERS_SIZE)',[AIterator.CurrentLine]);
    end;
    NewZone.TimeZoneLetters:=TmpWord;

    //And finally the UNTIL field which format is optional fields from
    //left to right: year month day hour[s]
    //defaults:      YEAR Jan   1   0:00:00
    NewZone.RuleValidUntil:=ParseUntilFields(AIterator,NewZone.RuleValidUntilGMT);
  end;
end;

procedure TPascalTZ.BareParseRule(const AIterator: TTZLineIterate);
var
  Index: integer;
  TmpWord: AsciiString;
  StandardTimeFlag: char;
  NewRule: TTZRule;
begin
  NewRule := TTZRule.Create;
  Index := FRules.Add(NewRule); // list owns the new item

  begin
    TmpWord:=AIterator.GetNextWord;
    if Length(TmpWord)>TZ_RULENAME_SIZE then begin
      FRules.Delete(Index); //Remove put information
      Raise TTZException.CreateFmt('Name on Rule line "%s" too long. (Increase source code TZ_RULENAME_SIZE)',[AIterator.CurrentLine]);
    end;
    NewRule.Name:=TmpWord;
    //Begin year...
    TmpWord:=AIterator.GetNextWord;
    NewRule.FromYear:=StrToInt(TmpWord);
    //End year...
    TmpWord:=AIterator.GetNextWord;
    if TmpWord='only' then begin
      NewRule.ToYear:=NewRule.FromYear;
    end else if TmpWord='max' then begin
      //max year, so in this case 9999
      NewRule.ToYear:=9999;
    end else begin
      NewRule.ToYear:=StrToInt(TmpWord);
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
    NewRule.InMonth:=MonthNumberFromShortName(TmpWord);
    //On Rule...
    TmpWord:=AIterator.GetNextWord;
    if Length(TmpWord)>TZ_ONRULE_SIZE then begin
      Raise TTZException.CreateFmt('ON Rule condition at "%s" too long. (Increase source code TZ_ONRULE_SIZE)',[AIterator.CurrentLine]);
    end;
    NewRule.OnRule:=TmpWord;
    //AT field
    TmpWord:=AIterator.GetNextWord;
    StandardTimeFlag:=TmpWord[Length(TmpWord)];
    if StandardTimeFlag in ['s','u','g'] Then begin
      if StandardTimeFlag='s' then begin
        NewRule.AtHourGMT:=false;
      end else begin
        NewRule.AtHourGMT:=true;
      end;
      TmpWord:=Copy(TmpWord,1,Length(TmpWord)-1); //remove the standard time flag
    end;
    NewRule.AtHourTime:=TimeToSeconds(TmpWord);
    //SAVE field
    TmpWord:=AIterator.GetNextWord;
    NewRule.SaveTime:=TimeToSeconds(TmpWord);
    //LETTERS field
    NewRule.TimeZoneLetters:=AIterator.GetNextWord;
    if NewRule.TimeZoneLetters='-' Then
      NewRule.TimeZoneLetters:='';
  end;
end;

procedure TPascalTZ.BareParseLink(const AIterator: TTZLineIterate);
var
  NewLink: TTZLink;
  Index: Integer;
  TmpWord: AsciiString;
begin
  NewLink := TTZLink.Create;
  Index := FLinks.Add(NewLink); // list owns the new item
  try
    // "FROM" zone
    TmpWord := AIterator.GetNextWord;
    if Length(TmpWord) > TZ_ZONENAME_SIZE then
      raise TTZException.CreateFmt('Zone link FROM name "%s" is too long. (Increase source code TZ_ZONENAME_SIZE)', [TmpWord]);
    NewLink.LinkFrom := TmpWord;

    // "TO" zone
    TmpWord := AIterator.GetNextWord;
    if Length(TmpWord) > TZ_ZONENAME_SIZE then
      raise TTZException.CreateFmt('Zone link TO name "%s" is too long. (Increase source code TZ_ZONENAME_SIZE)', [TmpWord]);
    NewLink.LinkTo := TmpWord;

    // Check existance of "FROM" zone
    if FindZoneName(NewLink.LinkFrom, False) < 0 then
      raise TTZException.CreateFmt('Zone info not found for link FROM "%s" TO "%s".', [NewLink.LinkFrom, NewLink.LinkTo]);
  except
    FLinks.Delete(Index);
    raise;
  end;
end;

function TPascalTZ.GetCountZones: Integer;
begin
  Result := FZones.Count;
end;

function TPascalTZ.GetCountRules: Integer;
begin
  Result := FRules.Count;
end;

procedure TPascalTZ.GetTimeZoneNames(const AZones: TStringList;
  const AOnlyGeoZones: Boolean);
var
  j: integer;
  LT: AsciiString;
begin
  AZones.Clear;
  LT:='';
  for j := 0 to FZones.Count-1 do begin
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

function TPascalTZ.TimeZoneExists(const AZone: String): Boolean;
var
  j: Integer;
begin
  Result := False;
  for j := 0 to FZones.Count-1 do
  begin
    if FZones[j].Name = AZone then
      Exit(True);
  end;
end;

function TPascalTZ.Convert(const ADateTime: TTZDateTime;
  const AZone: String; const AConvertToZone: Boolean): TTZDateTime;
var
  ATimeZoneName: String; // dummy
begin
  Result := Convert(ADateTime, AZone, AConvertToZone, ATimeZoneName);
end;

function TPascalTZ.Convert(const ADateTime: TTZDateTime;
  const AZone: String; const AConvertToZone: Boolean;
  out ATimeZoneName: String): TTZDateTime;
var
  j: integer;
  ZoneIndex, RuleIndex: Integer;
  ApplyRuleName: AsciiString;
  RuleBeginDate,RuleEndDate,UsedRuleBeginDate: TTZDateTime;
  SaveTime: integer;
  RuleLetters: AsciiString;
  ZoneNameCut: integer;
  UsedRuleIndex: Integer;
begin
  //Find zone matching target...
  ZoneIndex:=FindZoneName(AZone, True);
  if ZoneIndex<0 then begin
    raise TTZException.CreateFmt('Zone not found [%s]', [AZone]);
  end;

  // Now check which zone configuration line matches the given date.
  ZoneIndex:=FindZoneForDate(ZoneIndex,ADateTime);
  if ZoneIndex<0 then begin
    raise TTZException.CreateFmt('No valid conversion rule for Zone [%s]', [AZone]);
  end;
  RuleIndex:=-1;
  if Length(FZones[ZoneIndex].RuleName) > 0 then
    RuleIndex:=FindRuleName(FZones[ZoneIndex].RuleName);

  if RuleIndex<0 then begin
    //No rule is applied, so use the zone fixed offset
    Result:=ADateTime;
    if AConvertToZone then
      Inc(Result.SecsInDay,FZones[ZoneIndex].RuleFixedOffset+FZones[ZoneIndex].Offset)
    else
      Dec(Result.SecsInDay,FZones[ZoneIndex].RuleFixedOffset+FZones[ZoneIndex].Offset);
    ATimeZoneName:=FZones[ZoneIndex].TimeZoneLetters;
    FixUpTime(Result);
    exit;
  end;

  //Now we have the valid rule index...
  ApplyRuleName:=FRules[RuleIndex].Name;
  j:=RuleIndex;
  SaveTime:=0;
  UsedRuleIndex:=-1;
  while (j<=FRules.Count-1) and (FRules[j].Name=ApplyRuleName) do begin
    if (ADateTime.Year>=FRules[j].FromYear) and (ADateTime.Year<=FRules[j].ToYear) then begin
      //The year is in the rule range, so discard year information...
      RuleBeginDate.Year:=ADateTime.Year;
      RuleBeginDate.Month:=FRules[j].InMonth;
      MacroSolver(RuleBeginDate,FRules[j].OnRule);
      RuleBeginDate.SecsInDay:=FRules[j].AtHourTime;
      if not AConvertToZone then
        Inc(RuleBeginDate.SecsInDay, FZones[ZoneIndex].Offset);

      RuleEndDate.Year:=ADateTime.Year;
      RuleEndDate.Month:=12;
      RuleEndDate.Day:=31;
      RuleEndDate.SecsInDay:=SecsPerDay;

      // Ensure that we use the latest applicable rule, once it has passed the initial range.
      // When rules are sorted by year and then by month, it doesn't guarantee that
      // all applicable rules for the year in question are also in ascending order by month.
      // For example: Rule1 = 2000-2010 Oct; Rule2 = 2005-2010 Mar; for years 2005-2010
      // both rules are applicable but they are not in ascending order by month (Oct, Mar),
      // so simply using the last rule in the list will not work!
      if (CompareDates(ADateTime,RuleBeginDate)>=0) and
         (CompareDates(ADateTime,RuleEndDate)<=0) then
      begin
        // "UsedRuleBeginDate" not initialized is ok by design!
        if (UsedRuleIndex < 0) or (CompareDates(RuleBeginDate, UsedRuleBeginDate)>=0) then
        begin
          SaveTime:=FRules[j].SaveTime;
          RuleLetters:=FRules[j].TimeZoneLetters;
          UsedRuleIndex := j;
          UsedRuleBeginDate := RuleBeginDate;
        end;
      end;
    end;
    inc(j);
  end;

  Result:=ADateTime;
  if AConvertToZone then
    Inc(Result.SecsInDay,SaveTime+FZones[ZoneIndex].Offset)
  else
    Dec(Result.SecsInDay,SaveTime+FZones[ZoneIndex].Offset);
  FixUpTime(Result);

  if not AConvertToZone then
  if FDetectInvalidLocalTimes then begin
    if CompareDates(ADateTime,Convert(Result,AZone,not AConvertToZone))<>0 then begin
      Raise TTZException.CreateFmt('The time %s does not exists in %s',[DateTimeToStr(ADateTime),AZone]);
    end;
  end;

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
end;

function TPascalTZ.GMTToLocalTime(const ADateTime: TDateTime; const AToZone: String): TDateTime;
var
  ATimeZoneSubFix: String; // dummy
begin
  Result := GMTToLocalTime(ADateTime, AToZone, ATimeZoneSubFix);
end;

function TPascalTZ.GMTToLocalTime(const ADateTime: TDateTime;
  const AToZone: String; out ATimeZoneSubFix: String): TDateTime;
var
  MilliSeconds: integer;
begin
  MilliSeconds:=MilliSecondOfTheSecond(ADateTime);
  Result:=TZDateToPascalDate(GMTToLocalTime(PascalDateToTZDate(ADateTime),AToZone,ATimeZoneSubFix));
  Result:=IncMilliSecond(Result,MilliSeconds);
end;

function TPascalTZ.GMTToLocalTime(const ADateTime: TTZDateTime;
  const AToZone: String; out ATimeZoneName: String): TTZDateTime;
begin
  Result := Convert(ADateTime, AToZone, True, ATimeZoneName);
end;

function TPascalTZ.LocalTimeToGMT(const ADateTime: TDateTime;
  const AFromZone: String): TDateTime;
var
  MilliSeconds: integer;
begin
  MilliSeconds:=MilliSecondOfTheSecond(ADateTime);
  Result:=TZDateToPascalDate(LocalTimeToGMT(PascalDateToTZDate(ADateTime),AFromZone));
  Result:=IncMilliSecond(Result,MilliSeconds);
end;

function TPascalTZ.LocalTimeToGMT(const ADateTime: TTZDateTime;
  const AFromZone: String): TTZDateTime;
begin
  Result := Convert(ADateTime, AFromZone, False);
end;

function TPascalTZ.TimeZoneToTimeZone(const ADateTime: TDateTime; const AFromZone, AToZone: String): TDateTime;
var
  ATimeZoneSubFix: String; // dummy
begin
  Result := TimeZoneToTimeZone(ADateTime, AFromZone, AToZone, ATimeZoneSubFix);
end;

function TPascalTZ.TimeZoneToTimeZone(const ADateTime: TDateTime;
  const AFromZone, AToZone: String; out ATimeZoneSubFix: String
  ): TDateTime;
var
  Tmp: TTZDateTime;
begin
  Tmp:=PascalDateToTZDate(ADateTime);
  Tmp:=LocalTimeToGMT(Tmp,AFromZone);
  Tmp:=GMTToLocalTime(Tmp,AToZone,ATimeZoneSubFix);
  Result:=TZDateToPascalDate(Tmp);
end;

function TPascalTZ.ParseDatabaseFromFile(const AFileName: String): Boolean;
var
  FileStream: TFileStream;
begin
  CheckCanLoadDatabase;
  FileStream:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    Result:=ParseDatabaseFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TPascalTZ.ParseDatabaseFromFiles(const AFileNames: array of String): Boolean;
var
  ADatabaseStream: TStringStream;
  AFileStream: TFileStream;
  AFileName: String;
begin
  CheckCanLoadDatabase;
  ADatabaseStream := TStringStream.Create('');
  try
    for AFileName in AFileNames do
    begin
      AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
      try
        ADatabaseStream.CopyFrom(AFileStream, AFileStream.Size);
        ADatabaseStream.WriteString(LineEnding + LineEnding);
      finally
        AFileStream.Free;
      end;
    end;
    ADatabaseStream.Position := 0;
    Result := ParseDatabaseFromStream(ADatabaseStream);
  finally
    ADatabaseStream.Free;
  end;
end;

procedure TPascalTZ.CheckCanLoadDatabase;
begin
  // TPascalTZ class was not designed to load more than one database file.
  // Loading more than once messes up evaluation/application of timezones,
  // so forbid it until this problem is fixed.
  if FDatabaseLoaded then
    raise TTZException.Create('Cannot load timezone database, it is already loaded');
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
  CheckCanLoadDatabase;
  FDatabaseLoaded := True;

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
          ParseLine(FLineCounter, ThisLine, ParseSequence);
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
        ParseLine(FLineCounter, ThisLine, ParseSequence);
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

constructor TPascalTZ.Create;
begin
  FDetectInvalidLocalTimes := True;
  FDatabaseLoaded := False;
  FLinks := TTZLinkList.Create(True); // FreeObjects = True
  FZones := TTZZoneList.Create(True); // FreeObjects = True
  FRules := TTZRuleList.Create(True); // FreeObjects = True
end;

destructor TPascalTZ.Destroy;
begin
  FreeAndNil(FLinks);
  FreeAndNil(FZones);
  FreeAndNil(FRules);
end;

end.

