# Using PascalTZ

PascalTZ package is **currently under heavy maintenance** and no up-to-date usage instructions available yet.

Below are usage instructions for PascalTZ 1.0 from the original author.

### Dependencies / Requirements

- [Time Zone Database](https://www.iana.org/time-zones) - use the latest `tzdata*.tar.gz`.

## PascalTZ 1.0

**Warning:** Major data parsing and time zone conversions problems have been discovered in PascalTZ 1.0. You are strongly advised to use the latest development version until further notice.

### Installation

1. Add `upascaltz` unit to the uses clause of your project.
2. Load a database in a new instance of `TPascalTZ` class.

The database is presented in different files, so you can load one of them using `ParseDatabaseFromFile` or concatenate the interested ones in a single file or stream. Beware, once the database is loaded calling `ParseDatabaseFromXXXX` again will corrupt the internal time zone information.

### Known Issues

`TDateTime` in FPC seems to have problems for dates before *30 Dec 1899*, so the time operations before such date could be wrong. If you need to operate with early dates you can derive a new class from `TPascalTZ` and expose the `TTZDateTime` to operate with.

### Function Reference

#### `ProcessedLines`

Amount of read lines in database.

#### `DetectInvalidLocalTimes` [True|False] (default true)

When converting it will check if the given time is impossible (does not
exists). This happends, in example, when a local time changes from 2:00
to 3:00, if the time to be converted is 2:01 at the change date, that
time does not exists as it is not linear.

#### `GetTimeZoneNames`

Returns a TStringList with the time zones available in the database. This
names must be used for local times to perform conversions. It is not a
country list, as many countries have several time zones. AOnlyGeoZones
removes from the list the usual "GMT, BST, PST" from the list.

#### `GMTToLocalTime`

Converts a GMT/UTC/Zulu time to a time zone (AToZone). ATimeZoneSubFix
returns the subfix for that zone like "GMT, BST, ...".

#### `LocalTimeToGMT`

Converts a local time at time zone "AFromZone" to GMT/UTC/Zulu time.

#### `TimeZoneToTimeZone`

Converts time across zones. Basically this performs a local time to
GMT and GTM to the new local time.

#### `ParseDatabaseFromFile`

Reads the database from a file.

#### `ParseDatabaseFromStream`

Reads the database from a stream.
