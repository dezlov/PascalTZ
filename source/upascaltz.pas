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
  Classes, SysUtils, uPascalTZ_Types;

type
  TPascalTZ = class(TComponent)
  private
    ParseStatusTAG: AsciiString;
    ParseStatusPreviousZone: AsciiString;
    FDetectInvalidLocalTimes: Boolean;
    FRuleGroups: TTZRuleGroupList;
    FZoneGroups: TTZZoneGroupList;
    FLinks: TTZLinkList;
    FDatabasePath: String;
    procedure DatabaseChanged;
    function GetCountZones: Integer;
    function GetCountRules: Integer;
    function GetCountLinks: Integer;
    function GetCountTimeZoneNames: Integer;
    procedure ParseLine(const ALineNumber: Integer; const ALine: AsciiString; const AParseSequence: TParseSequence);
    procedure BareParseLine(const ALine: AsciiString; const AParseSequence: TParseSequence);
    procedure BareParseZone(const AIterator: TTZLineIterate; const AZone: AsciiString);
    procedure BareParseRule(const AIterator: TTZLineIterate);
    procedure BareParseLink(const AIterator: TTZLineIterate);
    procedure SetDatabasePath(const APath: String);
    procedure ReloadDatabasePath;
    function TryReloadDatabasePath: Boolean;
  protected
    procedure Loaded; override;
  // Keep interesting methods in "protected" section so they could be accessed by sub-classes.
  protected
    function FindRuleForDate(const ARuleList: TTZRuleList; const AZone: TTZZone;
      const ADateTime: TTZDateTime; const ATimeForm: TTZTimeForm): TTZRule;
    function FindZoneForDate(const AZoneList: TTZZoneList; const ADateTime: TTZDateTime;
      const ATimeForm: TTZTimeForm): TTZZone;
    function FindZoneGroup(const AZone: AsciiString; const AIncludeLinks: Boolean): TTZZoneGroup;
    function FindRuleGroup(const AName: AsciiString): TTZRuleGroup;
    function FindLink(const ALinkName: AsciiString): TTZLink;
    function FindZoneSaveTimeOffset(const AZone: TTZZone;
      const ADateTime: TTZDateTime; const ATimeForm: TTZTimeForm): Integer; overload;
    function FindZoneSaveTimeOffset(const AZone: TTZZone;
      const ADateTime: TTZDateTime; const ATimeForm: TTZTimeForm;
      out ATimeZoneAbbreviation: String): Integer; overload;
    function Convert(const ADateTime: TTZDateTime; const AZone: String;
      const ASourceTimeForm, ATargetTimeForm: TTZTimeForm): TTZDateTime; overload;
    function Convert(const ADateTime: TTZDateTime; const AZone: String;
      const ASourceTimeForm, ATargetTimeForm: TTZTimeForm;
      out ATimeZoneAbbreviation: String): TTZDateTime; overload;
  public
    property CountZones: Integer read GetCountZones;
    property CountRules: Integer read GetCountRules;
    property CountLinks: Integer read GetCountLinks;
    property CountTimeZoneNames: Integer read GetCountTimeZoneNames;
    procedure GetTimeZoneNames(const AZones: TStrings; const AIncludeLinks: Boolean = True);
    function TimeZoneExists(const AZone: String; const AIncludeLinks: Boolean = True): Boolean;
    function Convert(const ADateTime: TTZDateTime; const AFromZone, AToZone: String): TTZDateTime; overload;
    function Convert(const ADateTime: TDateTime; const AFromZone, AToZone: String): TDateTime; overload;
    function UnixToLocalTime(const ATimestamp: Int64; const AToZone: String): TDateTime; overload;
    function LocalTimeToUnix(const ADateTime: TDateTime; const AFromZone: String): Int64; overload;
    function GMTToLocalTime(const ADateTime: TTZDateTime; const AToZone: String): TTZDateTime; overload;
    function GMTToLocalTime(const ADateTime: TTZDateTime; const AToZone: String; out ATimeZoneAbbreviation: String): TTZDateTime; overload;
    function GMTToLocalTime(const ADateTime: TDateTime; const AToZone: String): TDateTime; overload;
    function GMTToLocalTime(const ADateTime: TDateTime; const AToZone: String; out ATimeZoneAbbreviation: String): TDateTime; overload;
    function LocalTimeToGMT(const ADateTime: TTZDateTime; const AFromZone: String): TTZDateTime;
    function LocalTimeToGMT(const ADateTime: TDateTime; const AFromZone: String): TDateTime;
    function TimeZoneToTimeZone(const ADateTime: TDateTime; const AFromZone, AToZone: String): TDateTime; overload;
    function TimeZoneToTimeZone(const ADateTime: TDateTime; const AFromZone, AToZone: String; out ATimeZoneAbbreviation: String): TDateTime; overload;
    procedure ParseDatabaseFromDirectory(const ADirectory: String);
    procedure ParseDatabaseFromFile(const AFileName: String);
    procedure ParseDatabaseFromFiles(const AFileNames: array of String; const AFilePathPrefix: String = '');
    procedure ParseDatabaseFromString(const AString: String);
    procedure ParseDatabaseFromStream(const AStream: TStream);
    procedure ParseDatabaseFromMemory(const AData: Pointer; const ADataSize: Integer);
    procedure ClearDatabase;
    constructor Create; overload;
    constructor Create(AOwner: TComponent); override; overload;
    destructor Destroy; override;
  public
    class function LocalTime: TDateTime;
    class function UniversalTime: TDateTime;
    class function UnixTime: Int64;
  published
    property DatabasePath: String read FDatabasePath write SetDatabasePath;
    property DetectInvalidLocalTimes: Boolean read FDetectInvalidLocalTimes write FDetectInvalidLocalTimes;
  end;

implementation

uses
  RtlConsts, DateUtils, Math, uPascalTZ_Tools;

class function TPascalTZ.LocalTime: TDateTime;
begin
  Result := Now;
end;

class function TPascalTZ.UniversalTime: TDateTime;
begin
  // LocalTimeToUniversal(Now) is a crude way of getting current UTC time
  // because Now gets us local time and then LocalTimeToUniversal adjusts it
  // by the currently active local time offset (suffers from a race condition).
  // It would be better to use something like LazUTF8SysUtils.NowUTC,
  // but that would add LazUtils dependency. Perhaps NowUTC can be moved to FPC?
  Result := LocalTimeToUniversal(Now);
end;

class function TPascalTZ.UnixTime: Int64;
begin
  Result := DateTimeToUnix(UniversalTime);
end;

function TPascalTZ.FindRuleGroup(const AName: AsciiString): TTZRuleGroup;
var
  Group: TTZRuleGroup;
begin
  Result := nil;
  for Group in FRuleGroups do
  begin
    if Group.Name = AName then
      Exit(Group);
  end;
end;

function TPascalTZ.FindLink(const ALinkName: AsciiString): TTZLink;
var
  Link: TTZLink;
begin
  Result := nil;
  for Link in FLinks do
  begin
    if Link.LinkName = ALinkName then
      Exit(Link);
  end;
end;

function TPascalTZ.FindZoneGroup(const AZone: AsciiString; const AIncludeLinks: Boolean): TTZZoneGroup;
var
  Link: TTZLink;
  ZoneGroup: TTZZoneGroup;
  TargetZone: AsciiString;
begin
  Result := nil;
  TargetZone := AZone;
  if AIncludeLinks then
  begin
    Link := FindLink(AZone);
    if Link <> nil then
      TargetZone := Link.LinkTarget;
  end;
  for ZoneGroup in FZoneGroups do
  begin
    if ZoneGroup.Name = TargetZone then
      Exit(ZoneGroup);
  end;
end;

function TPascalTZ.FindZoneForDate(const AZoneList: TTZZoneList; const ADateTime: TTZDateTime;
  const ATimeForm: TTZTimeForm): TTZZone;
var
  Zone: TTZZone;
  ZoneValidUntilInTargetTimeForm: TTZDateTime;
begin
  Result := nil;
  // Assumes that zone list is sorted by ZONE UNTIL date
  for Zone in AZoneList do
  begin
    // Convert ZONE UNTIL date to the same time form as the date in question
    ZoneValidUntilInTargetTimeForm := ConvertToTimeForm(Zone.ValidUntil, Zone.Offset,
      Zone.ValidUntilSaveTime, Zone.ValidUntilForm, ATimeForm);
    // Select first zone which is valid until some date in future past our date
    if ZoneValidUntilInTargetTimeForm > ADateTime then
    begin
      Result := Zone;
      Break;
    end;
  end;
end;

function TPascalTZ.FindRuleForDate(const ARuleList: TTZRuleList; const AZone: TTZZone;
  const ADateTime: TTZDateTime; const ATimeForm: TTZTimeForm): TTZRule;
var
  TargetDatePreviousSaveUTC: TTZDateTime;
  RuleBeginPreviousSaveUTC: TTZDateTime;
  ApplyYear, ApplyYearMax, ApplyYearMin: Integer;
  Rule: TTZRule;
  RuleBegin: TTZDateListItem;
  RuleBeginList: TTZDateList;
  PreviousSaveTimeOffset: Integer;
  IsApplicableRule, RuleBeginsWithZone: Boolean;
begin
  Result := nil;
  RuleBeginList := TTZDateList.Create(True); // FreeObjects = True
  try
    // Generate rule activation dates for the most recent and previous year.
    for Rule in ARuleList do
    begin
      ApplyYearMax := Min(ADateTime.Year, Rule.ToYear); // current year (bound to rule last year)
      ApplyYearMin := Max(ApplyYearMax - 1, Rule.FromYear); // previous year (bound to rule first year)
      for ApplyYear := ApplyYearMin to ApplyYearMax do
      begin
        RuleBegin := TTZDateListItem.Create(Rule,
          Rule.GetBeginDate(ApplyYear), Rule.AtHourTimeForm);
        RuleBeginList.Add(RuleBegin); // list owns the new item
      end;
    end;

    // Sort by rule activation date in ascending order.
    RuleBeginList.SortByDate;

    // Select the most recently activated rule by comparing target date
    // and rule begin date in UTC forms using previous save time offset.
    // When rule activation time is defined in wall clock form, it is by
    // design a wall clock time at a moment when previous rule was active.
    // Convert both dates to UTC and use the same save time offset (of previous rule)
    // to have a consistent and transparent operation. Other time zone libraries
    // (e.g. PHP, Howard Hinnant TZ) seem to be using a similar approach.
    PreviousSaveTimeOffset := 0;
    for RuleBegin in RuleBeginList do
    begin
      TargetDatePreviousSaveUTC := ConvertToTimeForm(ADateTime,
        AZone.Offset, PreviousSaveTimeOffset, // use previous save time
        ATimeForm, tztfUniversal);

      RuleBeginPreviousSaveUTC := ConvertToTimeForm(RuleBegin.Date,
        AZone.Offset, PreviousSaveTimeOffset, // use previous save time
        RuleBegin.TimeForm, tztfUniversal);

      // Handle special cases when rule begin date is same as zone begin date
      // (a.k.a. previous zone end date), meaning that rule begin date in UTC
      // should be calculated based on previous zone offset and save time.
      // For example: "1982-03-31 18:00" UTC to "Asia/Aqtau" time zone.
      //   Zone 5:00    -   SHET    1981 Oct  1 # Shevchenko Time
      //   Zone 6:00    -   SHET    1982 Apr  1
      //   Rule RussiaAsia 1981    1984    -   Apr 1    0:00   1:00    S
      //   Rule RussiaAsia 1981    1983    -   Oct 1    0:00   0   -
      // UTC Time            Offset  Save Time   Local Time
      // 1982-03-31 17:00    6:00    0:00        1982-03-31 23:00
      // 1982-03-31 17:30    6:00    0:00        1982-03-31 23:30
      // 1982-03-31 18:00    6:00    0:00        1982-04-01 00:00  <- old zone and no rules!
      // 1982-03-31 18:00    5:00    1:00        1982-04-01 00:00  <- new zone and new rule!
      // 1982-03-31 18:30    5:00    1:00        1982-04-01 00:30
      // 1982-03-31 19:00    5:00    1:00        1982-04-01 01:00
      if Assigned(AZone.PreviousZone) then
      begin
        RuleBeginsWithZone :=
          (AZone.PreviousZone.ValidUntil = RuleBegin.Date) and // same date
          (AZone.PreviousZone.ValidUntilForm = RuleBegin.TimeForm); // same time form
        if RuleBeginsWithZone then
        begin
          RuleBeginPreviousSaveUTC := ConvertToTimeForm(RuleBegin.Date,
            AZone.PreviousZone.Offset, AZone.PreviousZone.ValidUntilSaveTime,
            RuleBegin.TimeForm, tztfUniversal);
        end;
      end;

      IsApplicableRule := (RuleBeginPreviousSaveUTC <= TargetDatePreviousSaveUTC);
      if IsApplicableRule then
        Result := RuleBegin.Rule;

      PreviousSaveTimeOffset := RuleBegin.Rule.SaveTime;
    end;
  finally
    RuleBeginList.Free;
  end;
end;

function TPascalTZ.FindZoneSaveTimeOffset(const AZone: TTZZone;
  const ADateTime: TTZDateTime; const ATimeForm: TTZTimeForm): Integer;
var
  ATimeZoneAbbreviation: String; // dummy
begin
  Result := FindZoneSaveTimeOffset(AZone, ADateTime, ATimeForm, ATimeZoneAbbreviation);
end;

function TPascalTZ.FindZoneSaveTimeOffset(const AZone: TTZZone;
  const ADateTime: TTZDateTime; const ATimeForm: TTZTimeForm;
  out ATimeZoneAbbreviation: String): Integer;
var
  Rule: TTZRule;
  RuleGroup: TTZRuleGroup;
  RuleLetters: AsciiString;
begin
  Result := 0;
  RuleLetters := '';

  // Find zone rules
  RuleGroup := nil;
  if Length(AZone.RuleName) > 0 then
    RuleGroup := FindRuleGroup(AZone.RuleName);

  // No rule is applied, use fixed save time offset
  if RuleGroup = nil then
    Result := AZone.FixedSaveTime
  // Found list of rules, now find an applicable rule
  else
  begin
    Rule := FindRuleForDate(RuleGroup.List, AZone, ADateTime, ATimeForm);
    if Rule <> nil then
    begin
      Result := Rule.SaveTime;
      RuleLetters := Rule.TimeZoneLetters;
    end;
  end;

  // Format time zone abbreviation
  ATimeZoneAbbreviation := ResolveTimeZoneAbbreviation(AZone.TimeZoneLetters, RuleLetters, Result <> 0);
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
  PreParseLine := ALine;
  j := Pos('#',PreParseLine);
  if j > 0 then
    PreParseLine := Copy(PreParseLine,1,j-1);
  spCount := 0;
  for j := 1 to Length(PreParseLine) do
  begin
    if PreParseLine[j] = #9 then
      PreParseLine[j] := #32;
    if PreParseLine[j] = #32 then
      inc(spCount);
  end;
  if spCount = Length(PreParseLine) then
    PreParseLine := ''; // all spaces in line
  if Length(PreParseLine) > 0 then
  begin
    Parser := TTZLineIterate.Create(PreParseLine);
    try
      ZoneContinue := false;
      if (PreParseLine[1] = #32) or (PreParseLine[1] = #9) then
      begin
        // It is a continuation
        if ParseStatusTAG <> 'Zone' then
          raise TTZException.CreateFmt('Continuation for line tag "%s" is not allowed', [ParseStatusTAG]);
        ZoneContinue := true;
      end
      else
        ParseStatusTAG := Parser.GetNextWord;
      if (ParseStatusTAG = 'Zone') then
      begin
        if (AParseSequence = TTzParseZone) then
        begin
          if not ZoneContinue then
            ParseStatusPreviousZone := Parser.GetNextWord;
          BareParseZone(Parser,ParseStatusPreviousZone);
        end;
      end
      else if (ParseStatusTAG = 'Rule') then
      begin
        if (AParseSequence = TTzParseRule) then
          BareParseRule(Parser);
      end
      else if ParseStatusTAG = 'Link' then
      begin
        if (AParseSequence = TTzParseLink) then
          BareParseLink(Parser);
      end
      else
        raise TTZException.CreateFmt('Unknown line tag "%s"', [ParseStatusTAG]);
    finally
      Parser.Free;
    end;
  end;
end;

procedure TPascalTZ.BareParseZone(const AIterator: TTZLineIterate;
  const AZone: AsciiString);
var
  RuleName: AsciiString;
  TmpWord: AsciiString;
  NewZone: TTZZone;
  ZoneGroup: TTZZoneGroup;
begin
  NewZone := TTZZone.Create;
  try
    // First is the zone name
    if Length(AZone) > TZ_ZONENAME_SIZE then
      raise TTZException.CreateFmt('Name on Zone line "%s" too long. (TZ_ZONENAME_SIZE)', [AIterator.CurrentLine]);
    NewZone.Name := AZone;

    // Now check the offset
    TmpWord := AIterator.GetNextWord; // Offset
    NewZone.Offset := TimeToSeconds(TmpWord);

    // Now check the rules...
    RuleName := AIterator.GetNextWord;
    if RuleName = '' then
      raise TTZException.CreateFmt('Rule on Zone line "%s" empty.', [AIterator.CurrentLine]);
    if RuleName = '-' then
    begin
      // Always standard time
      NewZone.FixedSaveTime := 0;
      NewZone.RuleName := '';
    end
    else if RuleName[1] in ['0'..'9'] then
    begin
      // Fixed save time offset
      NewZone.FixedSaveTime := TimeToSeconds(RuleName);
      NewZone.RuleName := '';
    end
    else
    begin
      // Dynamic save time offset as defined by rules
      if FindRuleGroup(RuleName) = nil then
        raise TTZException.CreateFmt('Rule on Zone line "%s" not found.', [AIterator.CurrentLine]);
      NewZone.RuleName := RuleName;
      NewZone.FixedSaveTime := 0;
    end;

    // Now its time for the format (GMT, BST, ...)
    TmpWord := AIterator.GetNextWord;
    if Length(TmpWord) > TZ_TIMEZONELETTERS_SIZE then
      raise TTZException.CreateFmt('Format on Zone line "%s" too long. (TZ_TIMEZONELETTERS_SIZE)', [AIterator.CurrentLine]);
    NewZone.TimeZoneLetters := TmpWord;

    // And finally the UNTIL field which format is optional fields from
    // left to right: year month day hour[s]
    // defaults:      YEAR Jan   1   0:00:00

    // ZIC man page:
    // ZONE UNTIL field uses the same format as the IN, ON, and AT fields of a rule.
    // Hence, in the absence of an indicator, wall clock time is assumed.
    NewZone.ValidUntil := ParseUntilFields(AIterator, NewZone.ValidUntilForm, TZ_TIME_FORM_DEFAULT);
  except
    FreeAndNil(NewZone);
    raise;
  end;

  // Add new zone group if needed
  ZoneGroup := FindZoneGroup(NewZone.Name, False);
  if ZoneGroup = nil then
  begin
    ZoneGroup := TTZZoneGroup.Create(NewZone.Name);
    FZoneGroups.Add(ZoneGroup); // list owns the new item
  end;

  // Add new zone definition
  ZoneGroup.List.Add(NewZone); // list owns the new item
end;

procedure TPascalTZ.BareParseRule(const AIterator: TTZLineIterate);
var
  TmpWord: AsciiString;
  NewRule: TTZRule;
  RuleGroup: TTZRuleGroup;
begin
  NewRule := TTZRule.Create;
  try
    TmpWord := AIterator.GetNextWord;
    if Length(TmpWord) > TZ_RULENAME_SIZE then
      raise TTZException.CreateFmt('Name on Rule line "%s" too long. (TZ_RULENAME_SIZE)', [AIterator.CurrentLine]);
    NewRule.Name := TmpWord;

    // Begin year...
    TmpWord := AIterator.GetNextWord;
    NewRule.FromYear := StrToInt(TmpWord);

    // End year...
    TmpWord := AIterator.GetNextWord;
    if TmpWord = 'only' then
      NewRule.ToYear := NewRule.FromYear
    else if TmpWord = 'max' then
      NewRule.ToYear := TZ_YEAR_MAX
    else
      NewRule.ToYear := StrToInt(TmpWord);

    // Year type (macro)
    TmpWord := AIterator.GetNextWord;
    if TmpWord = '-' then
    begin
      // No year type, so all years.
    end
    else
    begin
      // Special year... check macro...
      // No one defined by now, so raise an exception if found.
      raise TTZException.CreateFmt('Year type not supported in line "%s"', [AIterator.CurrentLine]);
    end;

    // In month...
    TmpWord := AIterator.GetNextWord;
    NewRule.InMonth := MonthNumberFromShortName(TmpWord);

    // On Rule...
    TmpWord := AIterator.GetNextWord;
    if Length(TmpWord) > TZ_ONRULE_SIZE then
      raise TTZException.CreateFmt('ON Rule condition at "%s" too long. (TZ_ONRULE_SIZE)', [AIterator.CurrentLine]);
    NewRule.OnRule := TmpWord;

    // AT field
    TmpWord := AIterator.GetNextWord;
    // ZIC man page:
    // In RULE AT field, in the absence of an indicator, wall clock time is assumed.
    NewRule.AtHourTimeForm := ExtractTimeFormDefault(TmpWord, TZ_TIME_FORM_DEFAULT);
    NewRule.AtHourTime := TimeToSeconds(TmpWord);

    // SAVE field
    TmpWord := AIterator.GetNextWord;
    NewRule.SaveTime := TimeToSeconds(TmpWord);

    // LETTERS field
    NewRule.TimeZoneLetters := AIterator.GetNextWord;
    if NewRule.TimeZoneLetters = '-' then
      NewRule.TimeZoneLetters := '';
  except
    FreeAndNil(NewRule);
    raise;
  end;

  // Add new rule group if needed
  RuleGroup := FindRuleGroup(NewRule.Name);
  if RuleGroup = nil then
  begin
    RuleGroup := TTZRuleGroup.Create(NewRule.Name);
    FRuleGroups.Add(RuleGroup); // list owns the new item
  end;

  // Add new zone definition
  RuleGroup.List.Add(NewRule); // list owns the new item
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
    // "FROM" zone (link target)
    TmpWord := AIterator.GetNextWord;
    if Length(TmpWord) > TZ_ZONENAME_SIZE then
      raise TTZException.CreateFmt('Zone link FROM name "%s" is too long. (TZ_ZONENAME_SIZE)', [TmpWord]);
    NewLink.LinkTarget := TmpWord;

    // "TO" zone (link name)
    TmpWord := AIterator.GetNextWord;
    if Length(TmpWord) > TZ_ZONENAME_SIZE then
      raise TTZException.CreateFmt('Zone link TO name "%s" is too long. (TZ_ZONENAME_SIZE)', [TmpWord]);
    NewLink.LinkName := TmpWord;

    // Check existance of "FROM" zone
    if FindZoneGroup(NewLink.LinkTarget, False) = nil then
      raise TTZException.CreateFmt('Zone info not found for link FROM "%s" TO "%s".', [NewLink.LinkTarget, NewLink.LinkName]);
  except
    FLinks.Delete(Index);
    raise;
  end;
end;

function TPascalTZ.GetCountZones: Integer;
var
  Group: TTZZoneGroup;
begin
  Result := 0;
  for Group in FZoneGroups do
    Inc(Result, Group.List.Count);
end;

function TPascalTZ.GetCountRules: Integer;
var
  Group: TTZRuleGroup;
begin
  Result := 0;
  for Group in FRuleGroups do
    Inc(Result, Group.List.Count);
end;

function TPascalTZ.GetCountLinks: Integer;
begin
  Result := FLinks.Count;
end;

function TPascalTZ.GetCountTimeZoneNames: Integer;
var
  Names: TStringList;
begin
  Names := TStringList.Create;
  try
    GetTimeZoneNames(Names);
    Result := Names.Count;
  finally
    Names.Free;
  end;
end;

procedure TPascalTZ.GetTimeZoneNames(const AZones: TStrings;
  const AIncludeLinks: Boolean = True);
var
  I: Integer;
  ZoneName: AsciiString;
begin
  AZones.Clear;
  for I := 0 to FZoneGroups.Count-1 do
  begin
    ZoneName := FZoneGroups[I].Name;
    if AZones.IndexOf(ZoneName) < 0 then
      AZones.Add(ZoneName);
  end;
  if AIncludeLinks then
  begin
    for I := 0 to FLinks.Count-1 do
    begin
      ZoneName := FLinks[I].LinkName;
      if AZones.IndexOf(ZoneName) < 0 then
        AZones.Add(ZoneName);
    end;
  end;
end;

function TPascalTZ.TimeZoneExists(const AZone: String; const AIncludeLinks: Boolean = True): Boolean;
begin
  Result := (FindZoneGroup(AZone, AIncludeLinks) <> nil);
end;

function TPascalTZ.Convert(const ADateTime: TTZDateTime; const AZone: String;
  const ASourceTimeForm, ATargetTimeForm: TTZTimeForm): TTZDateTime;
var
  ATimeZoneAbbreviation: String; // dummy
begin
  Result := Convert(ADateTime, AZone, ASourceTimeForm, ATargetTimeForm, ATimeZoneAbbreviation);
end;

function TPascalTZ.Convert(const ADateTime: TTZDateTime; const AZone: String;
  const ASourceTimeForm, ATargetTimeForm: TTZTimeForm;
  out ATimeZoneAbbreviation: String): TTZDateTime;
var
  Zone: TTZZone;
  ZoneGroup: TTZZoneGroup;
  SaveTimeOffset: Integer;
begin
  Result := ADateTime;

  // Same source and target time forms, exit
  if ASourceTimeForm = ATargetTimeForm then
    Exit;

  // Find zone group by name
  ZoneGroup := FindZoneGroup(AZone, True);
  if ZoneGroup = nil then
    raise TTZException.CreateFmt('Zone not found [%s]', [AZone]);

  // Find appropriate zone from the group
  Zone := FindZoneForDate(ZoneGroup.List, ADateTime, ASourceTimeForm);
  if Zone = nil then
    raise TTZException.CreateFmt('No valid conversion rule for Zone [%s]', [AZone]);

  // Source and target time forms are not dependant on rules for save time offset
  if (ASourceTimeForm <> tztfWallClock) and (ATargetTimeForm <> tztfWallClock) then
  begin
    SaveTimeOffset := 0;
    ATimeZoneAbbreviation := ResolveTimeZoneAbbreviation(Zone.TimeZoneLetters, '', False);
  end
  // Find appropriate save time offset
  else
    SaveTimeOffset := FindZoneSaveTimeOffset(Zone, ADateTime, ASourceTimeForm, ATimeZoneAbbreviation);

  // Convert to the target time form
  Result := ConvertToTimeForm(ADateTime, Zone.Offset, SaveTimeOffset, ASourceTimeForm, ATargetTimeForm);

  // Check for invalid local time (supplied local time may not exist due to DST change)
  if FDetectInvalidLocalTimes and (ASourceTimeForm = tztfWallClock) then
    if ADateTime <> Convert(Result, AZone, ATargetTimeForm, ASourceTimeForm) then
      raise TTZException.CreateFmt('The time %s does not exist in %s', [DateTimeToStr(ADateTime), AZone]);
end;

function TPascalTZ.Convert(const ADateTime: TTZDateTime; const AFromZone, AToZone: String): TTZDateTime;
var
  DateTimeUTC: TTZDateTime;
begin
  DateTimeUTC := LocalTimeToGMT(ADateTime, AFromZone);
  Result := GMTToLocalTime(DateTimeUTC, AToZone);
end;

function TPascalTZ.Convert(const ADateTime: TDateTime; const AFromZone, AToZone: String): TDateTime;
var
  DateTimeUTC: TDateTime;
begin
  DateTimeUTC := LocalTimeToGMT(ADateTime, AFromZone);
  Result := GMTToLocalTime(DateTimeUTC, AToZone);
end;

function TPascalTZ.UnixToLocalTime(const ATimestamp: Int64; const AToZone: String): TDateTime;
begin
  Result := UnixToDateTime(ATimestamp);
  Result := GMTToLocalTime(Result, AToZone);
end;

function TPascalTZ.LocalTimeToUnix(const ADateTime: TDateTime; const AFromZone: String): Int64;
var
  DateTimeUTC: TDateTime;
begin
  DateTimeUTC := LocalTimeToGMT(ADateTime, AFromZone);
  Result := DateTimeToUnix(DateTimeUTC);
end;

function TPascalTZ.GMTToLocalTime(const ADateTime: TDateTime; const AToZone: String): TDateTime;
var
  ATimeZoneAbbreviation: String; // dummy
begin
  Result := GMTToLocalTime(ADateTime, AToZone, ATimeZoneAbbreviation);
end;

function TPascalTZ.GMTToLocalTime(const ADateTime: TDateTime;
  const AToZone: String; out ATimeZoneAbbreviation: String): TDateTime;
var
  MilliSeconds: Integer;
begin
  MilliSeconds := MilliSecondOfTheSecond(ADateTime);
  Result := TZDateToPascalDate(GMTToLocalTime(PascalDateToTZDate(ADateTime), AToZone, ATimeZoneAbbreviation));
  Result := IncMilliSecond(Result, MilliSeconds);
end;

function TPascalTZ.GMTToLocalTime(const ADateTime: TTZDateTime;
  const AToZone: String; out ATimeZoneAbbreviation: String): TTZDateTime;
begin
  Result := Convert(ADateTime, AToZone, tztfUniversal, tztfWallClock, ATimeZoneAbbreviation);
end;

function TPascalTZ.GMTToLocalTime(const ADateTime: TTZDateTime;
  const AToZone: String): TTZDateTime;
var
  ATimeZoneAbbreviation: String;
begin
  Result := GMTToLocalTime(ADateTime, AToZone, ATimeZoneAbbreviation);
end;

function TPascalTZ.LocalTimeToGMT(const ADateTime: TDateTime;
  const AFromZone: String): TDateTime;
var
  MilliSeconds: Integer;
begin
  MilliSeconds := MilliSecondOfTheSecond(ADateTime);
  Result := TZDateToPascalDate(LocalTimeToGMT(PascalDateToTZDate(ADateTime), AFromZone));
  Result := IncMilliSecond(Result, MilliSeconds);
end;

function TPascalTZ.LocalTimeToGMT(const ADateTime: TTZDateTime;
  const AFromZone: String): TTZDateTime;
begin
  Result := Convert(ADateTime, AFromZone, tztfWallClock, tztfUniversal);
end;

function TPascalTZ.TimeZoneToTimeZone(const ADateTime: TDateTime;
  const AFromZone, AToZone: String): TDateTime;
var
  ATimeZoneAbbreviation: String; // dummy
begin
  Result := TimeZoneToTimeZone(ADateTime, AFromZone, AToZone, ATimeZoneAbbreviation);
end;

function TPascalTZ.TimeZoneToTimeZone(const ADateTime: TDateTime;
  const AFromZone, AToZone: String; out ATimeZoneAbbreviation: String): TDateTime;
var
  Tmp: TTZDateTime;
begin
  Tmp := PascalDateToTZDate(ADateTime);
  Tmp := LocalTimeToGMT(Tmp, AFromZone);
  Tmp := GMTToLocalTime(Tmp, AToZone, ATimeZoneAbbreviation);
  Result := TZDateToPascalDate(Tmp);
end;

procedure TPascalTZ.ParseDatabaseFromDirectory(const ADirectory: String);
var
  TargetDirPath: String;
begin
  TargetDirPath := ADirectory;
  if Length(TargetDirPath) > 0 then
    TargetDirPath := IncludeTrailingPathDelimiter(TargetDirPath);
  ParseDatabaseFromFiles(TZ_FILES_STANDARD, TargetDirPath);
end;

procedure TPascalTZ.ParseDatabaseFromFile(const AFileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    ParseDatabaseFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPascalTZ.ParseDatabaseFromFiles(const AFileNames: array of String;
  const AFilePathPrefix: String = '');
var
  ADataStream: TStringStream;
  AFileStream: TFileStream;
  AFileName: String;
begin
  ADataStream := TStringStream.Create('');
  try
    for AFileName in AFileNames do
    begin
      AFileStream := TFileStream.Create(AFilePathPrefix + AFileName,
        fmOpenRead or fmShareDenyWrite);
      try
        ADataStream.CopyFrom(AFileStream, AFileStream.Size);
        ADataStream.WriteString(LineEnding + LineEnding);
      finally
        AFileStream.Free;
      end;
    end;
    ADataStream.Position := 0;
    ParseDatabaseFromStream(ADataStream);
  finally
    ADataStream.Free;
  end;
end;

procedure TPascalTZ.ParseDatabaseFromString(const AString: String);
var
  AStream: TStringStream;
begin
  AStream := TStringStream.Create(AString);
  try
    ParseDatabaseFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TPascalTZ.ParseDatabaseFromStream(const AStream: TStream);
var
  Buffer: Pointer;
  BufferSize: Integer;
begin
  BufferSize := AStream.Size - AStream.Position;
  Buffer := GetMem(BufferSize);
  // GetMem does not throw exception when "ReturnNilIfGrowHeapFails" is TRUE.
  if not Assigned(Buffer) then
    raise EOutOfMemory.Create(SErrOutOfMemory);
  try
    AStream.ReadBuffer(Buffer^, BufferSize);
    ParseDatabaseFromMemory(Buffer, BufferSize);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TPascalTZ.ParseDatabaseFromMemory(const AData: Pointer; const ADataSize: Integer);
var
  Buffer: PAsciiChar;
  ThisLine: AsciiString;
  LineBegin, LineSize, LineCounter: Integer;
  ParseSequence: TParseSequence;
begin
  try
    Buffer := AData;
    ParseStatusTAG := '';
    ParseStatusPreviousZone := '';
    ParseSequence := TTzParseRule;
    while ParseSequence < TTzParseFinish do
    begin
      LineCounter := 1;
      LineBegin := 0;
      LineSize := 0;
      while LineBegin < ADataSize do
      begin
        if (Buffer[LineBegin+LineSize] = #13) or (Buffer[LineBegin+LineSize] = #10) then
        begin
          SetLength(ThisLine,LineSize);
          Move(Buffer[LineBegin],ThisLine[1],LineSize);
          ParseLine(LineCounter, ThisLine, ParseSequence);
          inc(LineBegin,LineSize);
          LineSize := 0;
          while (LineBegin < ADataSize) and ((Buffer[LineBegin] = #13) or (Buffer[LineBegin] = #10)) do
          begin
            if Buffer[LineBegin] = #10 then
            begin
              inc(LineCounter);
            end;
            inc(LineBegin);
          end;
        end
        else
        begin
          inc(LineSize);
        end;
      end;
      if LineSize > 0 then
      begin
        inc(LineCounter);
        SetLength(ThisLine,LineSize);
        Move(Buffer[LineBegin],ThisLine[1],LineSize);
        ParseLine(LineCounter, ThisLine, ParseSequence);
      end;
      ParseSequence := Succ(ParseSequence);
    end;
  finally
    DatabaseChanged;
  end;
end;

procedure TPascalTZ.DatabaseChanged;
var
  Zone, PreviousZone: TTZZone;
  ZoneGroup: TTZZoneGroup;
begin
  for ZoneGroup in FZoneGroups do
  begin
    // Sort by ZONE UNTIL date (regardless of time form, we assume it won't matter)
    ZoneGroup.List.SortByValidUntil;

    // Identify previous zone for each zone in group
    PreviousZone := nil;
    for Zone in ZoneGroup.List do
    begin
      Zone.PreviousZone := PreviousZone;
      PreviousZone := Zone;
    end;

    // Calculate appropriate save time offset for ZONE UNTIL date
    for Zone in ZoneGroup.List do
    begin
      Zone.ValidUntilSaveTime := 0;
      if Zone.ValidUntil.Year <> TZ_YEAR_MAX then
        Zone.ValidUntilSaveTime := FindZoneSaveTimeOffset(Zone, Zone.ValidUntil, Zone.ValidUntilForm);
    end;
  end;
end;

procedure TPascalTZ.ClearDatabase;
begin
  FLinks.Clear;
  FZoneGroups.Clear;
  FRuleGroups.Clear;
end;

procedure TPascalTZ.ReloadDatabasePath;
var
  DrExsts: Boolean;
begin
  ClearDatabase;
  if Length(FDatabasePath) > 0 then
  begin
    DrExsts:=DirectoryExists(FDatabasePath);
    if not DrExsts and FileExists(FDatabasePath) then
      ParseDatabaseFromFile(FDatabasePath)
    else if DrExsts then 
      ParseDatabaseFromDirectory(FDatabasePath)
    else
      raise TTZException.Create(Format('Time zone database path does not exist: %s', [FDatabasePath]));
  end;
end;

function TPascalTZ.TryReloadDatabasePath: Boolean;
begin
  Result := True;
  try
    ReloadDatabasePath;
  except
    Result := False;
    ClearDatabase;
  end;
end;

procedure TPascalTZ.SetDatabasePath(const APath: String);
begin
  FDatabasePath := APath;
  // Do not load database at design time or while component is loading.
  if [csDesigning, csLoading] * ComponentState <> [] then Exit;
  ReloadDatabasePath;
end;

procedure TPascalTZ.Loaded;
begin
  inherited;
  // Do not load database at design time.
  if csDesigning in ComponentState then Exit;
  // Exceptions here would crash the application before it can handle them.
  // It is better to silently fail, rather than kill the entire application.
  TryReloadDatabasePath;
end;

// Keep the default constructor for backward compatibility.
constructor TPascalTZ.Create;
begin
  Create(nil);
end;

constructor TPascalTZ.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatabasePath := '';
  FDetectInvalidLocalTimes := True;
  FLinks := TTZLinkList.Create(True); // FreeObjects = True
  FZoneGroups := TTZZoneGroupList.Create(True); // FreeObjects = True
  FRuleGroups := TTZRuleGroupList.Create(True); // FreeObjects = True
end;

destructor TPascalTZ.Destroy;
begin
  FreeAndNil(FLinks);
  FreeAndNil(FZoneGroups);
  FreeAndNil(FRuleGroups);
  inherited Destroy;
end;

end.

