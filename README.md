# PascalTZ

PascalTZ stands for "Pascal Time Zone". It allows you to convert local times to [GMT](http://en.wikipedia.org/wiki/Gmt)/[UTC](http://en.wikipedia.org/wiki/Utc)/[Zulu](http://en.wikipedia.org/wiki/Coordinated_Universal_Time) and across [time zones](http://en.wikipedia.org/wiki/Time_zone) taking into account the different rules for this change on every time zone.

PascalTZ uses the [Time Zone Database](https://www.iana.org/time-zones) (often called `tz` or `zoneinfo`) to know how to change time for any past time (rules changed over the years) and it is almost 100% reliable for times in 20th and 21th century, and quite precise for prior times as some rules in 19th century and before were never properly documented. Time zone changes for the future times are not reliable as the rules can change sometimes suddenly like Venezuela 2007 change proposed a bunch of days before the change, or the 2009 Argentina ones.

It is designed to:

- Convert a given time/date in local time to/from GMT time.
- Convert a given time/date across two zone times.
- Detect invalid times in the conversion process.
- Be updateable simply by upgrading the [Time Zone Database](https://www.iana.org/time-zones).

This code is based on PascalTZ Version 1.0 2009.11.10 originally created by *José Mejuto*.

The maintenance of this package has been taken up by *Denis Kozlov* in 2015 with key goals:

1. Identify and fix all data parsing and time zone conversion problems (yes, plenty).
2. Refactor the existing code to make debugging and maintenance easier.
3. Expand functionality for more user-friendly workflows.
4. Add test cases and a test framework.

Please see [USAGE.md](USAGE.md) for general usage instructions and known issues.

Please see [CHANGES.md](CHANGES.md) for a list of major changes between versions.

### Authors

- 2009 - José Mejuto
- 2015 - Denis Kozlov

### License

[Modified](COPYING.modifiedLGPL.txt)
[LGPL](COPYING.LGPL.txt) (same as the [FPC RTL](http://wiki.freepascal.org/FPC_modified_LGPL) and the Lazarus LCL).

### See Also

- [PascalTZ article on FPC Wiki](http://wiki.freepascal.org/PascalTZ)
- [PascalTZ repository in Lazarus CCR on SourceForge](http://sourceforge.net/projects/lazarus-ccr/files/PascalTZ/)
