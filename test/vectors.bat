@ECHO OFF
SET PHP=php.exe
%PHP% --version
echo Generating test vectors....
%PHP% tz.php --generate --first=2000 --last=2016 --dpy=10 --zpd=10 > vectors-php-2000-2016-10dpy-10zpd.txt
%PHP% tz.php --generate --first=1910 --last=2016 --dpy=3 --zpd=3 > vectors-php-1910-2016-3dpy-3zpd.txt
echo Finished!
