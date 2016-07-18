<?php

/*******************************************************************************
This file is a part of PascalTZ package:
  https://github.com/dezlov/pascaltz

License:
  GNU Library General Public License (LGPL) with a special exception.
  Read accompanying README and COPYING files for more details.

Authors:
  2016 - Denis Kozlov
*******************************************************************************/

########## Get command line options ##########

$options = getopt('hvglc', array(
	'help', 'verbose', 'generate', 'length', 'convert',
	'zones::', 'first::', 'last::', 'dpy::', 'zpd::', 'date::', 'from::', 'to::',
	'mysql', 'host::', 'user::', 'pass::'
));

########## Short options ##########

$option_help       = isset($options['h']) || isset($options['help']);
$option_verbose    = isset($options['v']) || isset($options['verbose']);
$option_generate   = isset($options['g']) || isset($options['generate']);
$option_length     = isset($options['l']) || isset($options['length']);
$option_convert    = isset($options['c']) || isset($options['convert']);

########## Long options ##########

$option_zones_file     = isset($options['zones']) ? $options['zones'] : null;
$option_first          = isset($options['first']) ? $options['first'] : null;
$option_last           = isset($options['last']) ? $options['last'] : null;
$option_dates_per_year = isset($options['dpy']) ? $options['dpy'] : null;
$option_zones_per_date = isset($options['zpd']) ? $options['zpd'] : null;
$option_date           = isset($options['date']) ? $options['date'] : null;
$option_zone_from      = isset($options['from']) ? $options['from'] : null;
$option_zone_to        = isset($options['to']) ? $options['to'] : null;
$option_mysql          = isset($options['mysql']);
$option_hostname       = isset($options['host']) ? $options['host'] : null;
$option_username       = isset($options['user']) ? $options['user'] : null;
$option_password       = isset($options['pass']) ? $options['pass'] : null;

########## Help message ##########

if ($option_help || empty($options))
{
	echo 'Usage: php '.basename(__FILE__).' [command] [options]'.PHP_EOL;
	echo PHP_EOL;
	echo 'Commands:'.PHP_EOL;
	echo '  -h, --help       Display this help message.'.PHP_EOL;
	echo '  -v, --verbose    Verbose output, display configuration.'.PHP_EOL;
	echo '  -g, --generate   Generate and output test vectors.'.PHP_EOL;
	echo '  -l, --length     Analyze the length of zone names.'.PHP_EOL;
	echo '  -c, --convert    Convert date between time zones.'.PHP_EOL;
	echo PHP_EOL;
	echo 'Options for "generate" and "length" commands:'.PHP_EOL;
	echo '  --zones=<FILE>   Use a custom list of zones from a file (one per line).'.PHP_EOL;
	echo '  --first=<YEAR>   First year (Default is 1910).'.PHP_EOL;
	echo '  --last=<YEAR>    Last year (Default is current year).'.PHP_EOL;
	echo '  --dpy=<N>        Number of dates per year (Default is 3).'.PHP_EOL;
	echo '  --zpd=<N>        Number of zones per date (Default is 3).'.PHP_EOL;
	echo PHP_EOL;
	echo 'Options for "convert" command:'.PHP_EOL;
	echo '  --date=<DATE>    Date in YYYY-MM-DD HH:MM:SS format (Default is "now").'.PHP_EOL;
	echo '  --from=<ZONE>    Convert from zone name (Default is UTC).'.PHP_EOL;
	echo '  --to=<ZONE>      Convert to zone name (Default is UTC).'.PHP_EOL;
	echo '  --mysql          Test timezone conversion using MySQL also.'.PHP_EOL;
	echo '  --host=<HOST>    Hostname for MySQL connection (Default is "localhost").'.PHP_EOL;
	echo '  --user=<USER>    Username for MySQL connection (Default is "root").'.PHP_EOL;
	echo '  --pass=<PASS>    Password for MySQL connection (Default is empty).'.PHP_EOL;
	exit(1);
}

########## Main configuration and defaults ##########

$output_value_delimiter = ',';
$output_line_delimiter = PHP_EOL;
$date_time_format = 'Y-m-d H:i:s';
$number_of_dates_per_year = (empty($option_dates_per_year) ? 3 : (int)$option_dates_per_year);
$number_of_zones_per_date = (empty($option_zones_per_date) ? 3 : (int)$option_zones_per_date);
$range_year_first = (empty($option_first) ? '1910' : (int)$option_first);
$range_year_last = (empty($option_last) ? date('Y') : (int)$option_last);
$count_test_vectors = ($range_year_last - $range_year_first + 1) *
	$number_of_dates_per_year * $number_of_zones_per_date;

if (empty($option_hostname))
	$option_hostname = '127.0.0.1';
if (empty($option_username))
	$option_username = 'root';
if (empty($option_password))
	$option_password = '';

########## Prepare list of zones ##########

if (strlen($option_zones_file) > 0)
{
	$zones = file($option_zones_file);
	array_walk($zones, function (&$value) {
		$value = trim($value);
	});
	$zones = array_filter($zones, function($value) {
		return (strlen($value) > 0); // remove empty lines!
	});
}
else
{
	$zones = DateTimeZone::listIdentifiers();
	$zones = array_values(array_filter($zones, function($value) {
		return ($value != 'UTC'); // remove 'UTC' zone built into PHP, not official zone name!
	}));
}

########## Verbose output ##########

if ($option_verbose)
{
	echo 'PHP timezone version: '.timezone_version_get().PHP_EOL;
	if (is_linux())
		echo 'System tzdata version: '.get_apt_package_version('tzdata').PHP_EOL;
	echo 'Number of known zones: '.number_format(count($zones)).PHP_EOL;
	echo 'Number of dates per year: '.number_format($number_of_dates_per_year).PHP_EOL;
	echo 'Number of zones per date: '.number_format($number_of_zones_per_date).PHP_EOL;
	echo 'Years range: '.$range_year_first.'-'.$range_year_last.PHP_EOL;
	echo 'Total number of test vectors: '.number_format($count_test_vectors).PHP_EOL;
	if ($option_mysql)
	{
		echo 'MySQL hostname: '.$option_hostname.PHP_EOL;
		echo 'MySQL username: '.$option_username.PHP_EOL;
		echo 'MySQL password: '.$option_password.PHP_EOL;
	}
}

########## Convert action ##########

if ($option_convert)
{
	if (empty($option_zone_from))
		$option_zone_from = 'UTC';
	if (empty($option_zone_to))
		$option_zone_to = 'UTC';
	$tz_from = new DateTimeZone($option_zone_from);
	$tz_to = new DateTimeZone($option_zone_to);

	if (empty($option_date))
		$dt_source = new DateTime(null, $tz_from); // current date
	else
	{
		$dt_source = DateTime::createFromFormat($date_time_format, $option_date, $tz_from);
		if ($dt_source === false)
			throw new Exception('Failed to parse date: '.$option_date);
	}
	$dt_target = change_time_zone_lose_utc_link($dt_source, $tz_to);
	$dt_revert = change_time_zone_lose_utc_link($dt_target, $tz_from);

	echo ($option_mysql ? 'PHP source' : 'Source').' date: '.
		date_time_format($dt_source).' ['.date_time_zone_name($dt_source).']'.PHP_EOL;	
	echo ($option_mysql ? 'PHP target' : 'Target').' date: '.
		date_time_format($dt_target).' ['.date_time_zone_name($dt_target).']'.PHP_EOL;	
	echo ($option_mysql ? 'PHP revert' : 'Revert').' date: '.
		date_time_format($dt_revert).' ['.date_time_zone_name($dt_revert).']'.PHP_EOL;

	if ($option_mysql)
	{
		$mysqli = new mysqli($option_hostname, $option_username, $option_password);
		if ($mysqli->connect_error)
			throw new Exception('Failed to connect to MySQL host: '.$mysqli->connect_error);

		echo PHP_EOL;
		$prior_to_1970 = ($dt_source->format('Y') < 1970);
		$date_source = date_time_format($dt_source);
		$timezone_source = date_time_zone_name($dt_source);
		$timezone_target = $option_zone_to;

		$sql = vsprintf("SELECT CONVERT_TZ('%s', '%s', '%s')", array(
				$mysqli->escape_string($date_source),
				$mysqli->escape_string($timezone_source),
				$mysqli->escape_string($timezone_target)));
		$result = $mysqli->query($sql);
		if ($result === false)
			throw new Exception('MySQL query error: '.$mysqli->error);
		$row = $result->fetch_row();
		$date_target = reset($row);
		if ($date_target === null)
			throw new Exception('MySQL CONVERT_TZ returned NULL. Check time zone tables.');
		$result->close();

		$sql = vsprintf("SELECT CONVERT_TZ('%s', '%s', '%s')", array(
				$mysqli->escape_string($date_target),
				$mysqli->escape_string($timezone_target),
				$mysqli->escape_string($timezone_source)));
		$result = $mysqli->query($sql);
		if ($result === false)
			throw new Exception('MySQL query error: '.$mysqli->error);
		$row = $result->fetch_row();
		$date_revert = reset($row);
		if ($date_revert === null)
			throw new Exception('MySQL CONVERT_TZ returned NULL. Check time zone tables.');
		$result->close();

		echo 'SQL source date: '.$date_source.' ['.$timezone_source.']'.PHP_EOL;
		echo 'SQL target date: '.$date_target.' ['.$timezone_target.']'.PHP_EOL;
		echo 'SQL revert date: '.$date_revert.' ['.$timezone_source.']'.PHP_EOL;
		if ($prior_to_1970)
			echo PHP_EOL.'WARNING: MySQL cannot convert time zone on dates prior to year 1970.'.PHP_EOL;
	}
}

########## Length actions ##########

if ($option_length)
{
	$lengths = array();
	foreach ($zones as $zone)
		$lengths[strlen($zone)][] = $zone;
	ksort($lengths, SORT_NUMERIC);
	foreach ($lengths as $length => $names)
	{
		echo '['.$length.'] characters:'.PHP_EOL;
		foreach ($names as $name)
			echo '    '.$name.PHP_EOL;
	}
}

########## Generate actions ##########

if ($option_generate)
{
	$zone_index_first = 0;
	$zone_index_last = count($zones)-1;
	for ($year = $range_year_first; $year <= $range_year_last; $year++)
	{
		// hour, minute, second, month, day, year
		$ts_year_start = gmmktime(0, 0, 0, 1, 1, $year); // first second of the year
		$ts_year_end = gmmktime(0, 0, 0, 1, 1, $year+1)-1; // last second of the year
		
		for ($i=1; $i<=$number_of_dates_per_year; $i++)
		{
			$ts_random = mt_rand($ts_year_start, $ts_year_end);
			
			for ($k=1; $k<=$number_of_zones_per_date; $k++)
			{
				$zone_index_random = mt_rand($zone_index_first, $zone_index_last);
				$zone_random = $zones[$zone_index_random];		
				$vector = new TestVector($ts_random, $zone_random);
				$values = $vector->getOutputValues();
				echo implode($output_value_delimiter, $values).$output_line_delimiter;
			}
		}
	}
}

########## TestVector class ##########

class TestVector
{
	public $date_universal;
	public $date_local_from_universal;
	public $date_universal_from_local;
	public $timezone;
	
	public function __construct($timestamp_utc, $timezone)
	{
		$this->timezone = $timezone;

		$dt = new DateTime('@'.$timestamp_utc); // UTC date
		$this->date_universal = date_time_format($dt);

		$dt = change_time_zone_lose_utc_link($dt, new DateTimeZone($timezone));
		$this->date_local_from_universal = date_time_format($dt);

		$dt = change_time_zone_lose_utc_link($dt, new DateTimeZone('UTC'));
		$this->date_universal_from_local = date_time_format($dt);
	}
	
	public function getOutputValues()
	{
		$values = array(
			$this->date_universal,
			$this->date_local_from_universal,
			$this->timezone,
		);
		if ($this->date_universal_from_local != $this->date_universal)
			$values[] = $this->date_universal_from_local;
		return $values;
	}
}

########## Helper functions ##########

function date_time_zone_name(DateTime $dt)
{
	return $dt->getTimezone()->getName();
}

function date_time_format(DateTime $dt)
{
	global $date_time_format;
	return $dt->format($date_time_format);
}

function change_time_zone_lose_utc_link(DateTime $dt, DateTimeZone $tz_target)
{
	global $date_time_format;
	// Changing timezone of existing date object is not the same as recreating it,
	// because if the date object is internally stored as UTC then there would be
	// no ambiguity when changing time zones. We want ambiguity!
	$new = DateTime::createFromFormat($date_time_format,
		$dt->format($date_time_format), $dt->getTimezone());
	$new->setTimezone($tz_target);
	return $new;
}

function is_linux()
{
	return (strtoupper(PHP_OS) == 'LINUX');
}

function get_apt_package_version($package)
{
	$version_line_pattern = '#^Version:\s*(.+)$#';
	// dpkg -s <package>  -- shows the current status and current version
	// apt-cache show <package>  -- can show multiple version of the package
	$command = 'dpkg -s '.escapeshellarg($package);
	$output = $return_var = null;
	exec($command, $output, $return_var);
	if (($return_var == 0) && is_array($output))
	{
		foreach ($output as $line)
		{
			$matches = array();
			if (preg_match($version_line_pattern, $line, $matches))
				return $matches[1];
		}
	}
	return false;
}
