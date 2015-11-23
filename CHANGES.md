# Changes in PascalTZ

This change log contains the highlights of major changes. For details on all finer changes please check the commits history.

## Latest development

- Use time form of ZONE UNTIL field when determining applicable zone definition.
- Sort zone definitions by ZONE UNTIL date. Ensures correct operation of the zone selection algorithm when original input is not sorted.
- Ability to load standard files from "tzdata" package via ParseDatabaseFromStandardFiles method.
- Recoded algorithm that searches for applicable rule. Select the most recently activated rule by comparing target date and rule begin date in UTC forms using previous save time offset. A more consistent and transparent implementation.
- Moved LocalTimeToGMT and GMTToLocalTime operating on TTZDateTime type to public section. This should allow to overcome alleged limitations of TDateTime type.
- Added ParseDatabaseFromString method.
- Added ClearDatabase method.
- Significant speedup in performing time zone conversions due to smart storage of zones and rules (grouped by their name).
- Removed unused return value from all ParseDatabaseFromXXX functions.
- Added ParseDatabaseFromMemory function.
- Removed AOnlyGeoZones parameter from GetTimeZoneNames function, as it is inaccurate and infeasible.
- Removed public ProcessedLines property, in favor of CountZones, CountRules, CountLinks properties.
- Removed limitation for allowing to parse only one database stream. It no longer silently corrupts internal data.
- Removed rule sorting code. Sorting rules after parsing is no longer necessary.
- Fixed incorrect interpretation of time string in shortest form, i.e. time in hours.
- Fixed inappropriately adjusted rule begin dates according to RULE AT field's time form (universal, standard, wall clock).
- Fixed a problem with incorrectly adjusted dates when correcting overlapping seconds of day. Calculation of Days Since AD (DSAD) was replaced with Julian Day Number (JDN).
- Fixed a problem of not applying rules inherited from the past years (prior to the year of the date in question), by finding the most recently activated rule out of all rules.
- Correctly interpret all official forms of time (wall clock, standard, universal) in ZONE and RULE definitions.
- Fixed incorrect resolution of negative seconds in FixUpTime.
- Implemented parsing and use of LINK zone tag for resolving zone aliases.
- Eliminated dependency on custom sorting classes, replaced with built-in list sort functionality.
- Fixed incorrect selection of appropriate DST rule, by ensuring that only the latest applicable rule is used.
- Fixed mishandling of parameters and wrong use of MacroFirstWeekDay and MacroLastWeekDay functions, resulting in wrong values for non first or last week days (e.g. 2nd or 3rd week day).
- Fixed incorrect parsing of conditional parts in MacroSolver function (e.g. "Sun>=15").
- Fixed double freeing of internal objects in ParseLine.
- Increased the maximum allowed length of zone name from 30 to 32 characters, to support 'backward' zones.
- Added TimeZoneExists function to check for existence of a zone.
- Added ParseDatabaseFromFiles function for loading multiple database files at once by combining them into a single stream.
- Added CountZones, CountRules, CountLinks, CountTimeZoneNames properties.
- Do not break official zone names at parsing stage by replacing '_' (underscore) with ' ' (space) in zone names.
- Major code refactoring and cleanup.

## 1.0 (2009-11-10)

**Warning:** Major data parsing and time zone conversions problems are present in PascalTZ 1.0. You are strongly advised to use a newer version.

- Original version published by *José Mejuto*.
