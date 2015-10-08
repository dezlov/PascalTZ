# PascalTZ

PascalTZ stands for "Pascal Time Zone". It allows you to convert local times to [GMT](http://en.wikipedia.org/wiki/Gmt)/[UTC](http://en.wikipedia.org/wiki/Utc)/[Zulu](http://en.wikipedia.org/wiki/Coordinated_Universal_Time) and across [time zones](http://en.wikipedia.org/wiki/Time_zone) taking into account the different rules for this change on every time zone.

PascalTZ uses the [Time Zone Database](https://www.iana.org/time-zones) (often called `tz` or `zoneinfo`) to know how to change time for any past time (rules changed over the years) and it is almost 100% reliable for times in 20th and 21th century, and quite precise for prior times as some rules in 19th century and before were never properly documented. Time zone changes for the future times are not reliable as the rules can change sometimes suddenly like Venezuela 2007 change proposed a bunch of days before the change, or the 2009 Argentina ones.

It is designed to:

- Convert a given time/date in local time to/from GMT time.
- Convert a given time/date across two zone times.
- Detect invalid times in the conversion process.
- Be updateable simply by upgrading the [Time Zone Database](https://www.iana.org/time-zones).

This code is based on PascalTZ Version 1.0 2009.11.10 originally created by José Mejuto.

### Authors

- 2009 - José Mejuto
- 2015 - Denis Kozlov

### License

[Modified](COPYING.modifiedLGPL)
[LGPL](COPYING.LGPL) (same as the [FPC RTL](http://wiki.freepascal.org/FPC_modified_LGPL) and the Lazarus LCL).

### Dependencies / Requirements

- [Time Zone Database](https://www.iana.org/time-zones) - use the latest `tzdata*.tar.gz`.

### Installation

1. Just add the `upascaltz` to the uses clause of your project.
2. Load a database in a new instance of the class.

### Known Issues

`TDateTime` in FPC seems to have problems for dates before *30 Dec 1899*, so the time operations before such date could be wrong. If you need to operate with early dates you can derive a new class from `TPascalTZ` and expose the `TTZDateTime` to operate with.

### See Also

- [PascalTZ article on FPC Wiki](http://wiki.freepascal.org/PascalTZ)
- [PascalTZ repository in Lazarus CCR on SourceForge](http://sourceforge.net/projects/lazarus-ccr/files/PascalTZ/)
