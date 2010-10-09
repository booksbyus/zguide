#
#   sfldate.pl - SFL date functions
#                               
#   Copyright (c) 1991-2009 iMatix Corporation
#
#   Implements the complete set of macros and functions in the SFL date
#   package (sfldate.c).  Macros are defined in lower-case: &get_month.
#
#   ------------------ GPL Licensed Source Code ------------------
#   iMatix makes this software available under the GNU General
#   Public License (GPL) license for open source projects.  For
#   details of the GPL license please see www.gnu.org or read the
#   file license.gpl provided in this package.
#   
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License as
#   published by the Free Software Foundation; either version 2 of
#   the License, or (at your option) any later version.
#   
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public
#   License along with this program in the file 'license.gpl'; if
#   not, write to the Free Software Foundation, Inc., 59 Temple
#   Place - Suite 330, Boston, MA 02111-1307, USA.
#   
#   You can also license this software under iMatix's General Terms
#   of Business (GTB) for commercial projects.  If you have not
#   explicitly licensed this software under the iMatix GTB you may
#   only use it under the terms of the GNU General Public License.
#   
#   For more information, send an email to info@imatix.com.
#   --------------------------------------------------------------

package sfldate;

CONFIG: {
    #   Julian date calculation: days before 1st of each month in year
    @julian_base = ( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334);

    #   Number of days in each month, in a leap year
    @month_days = ( 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );

    #   The timezone calculation was plucked from the timelocal.pl module.
    local (@epoch) = localtime (0);
    $'daylight = (localtime) [8];       #   Is daylight savings time?
    $'timezone = $epoch [2] * 60 * 60 + $epoch [1] * 60;
    if ($'timezone > 0) {               #   Seconds west of GMT
        $'timezone = 24 * 60 * 60 - $'timezone;
        $'timezone -= 24 * 60 * 60
            if $epoch [5] == 70;        #   Account for the date line
    }
    #   Interval values, specified in centiseconds
    $'INTERVAL_CENTI = 1;
    $'INTERVAL_SEC   = 100;
    $'INTERVAL_MIN   = 6000;
    $'INTERVAL_HOUR  = 360000;
    $'INTERVAL_DAY   = 8640000;
}

#
#   Implement the date/time access macros from sfldate.h

sub 'get_century {
    local ($date) = @_;
    return (int ($date / 1000000));
}

sub 'get_ccyear {
    local ($date) = @_;
    return (int ($date / 10000));
}

sub 'get_year {
    local ($date) = @_;
    return (int (($date % 1000000) / 10000));
}

sub 'get_month {
    local ($date) = @_;
    return (int (($date % 10000) / 100));
}

sub 'get_day {
    local ($date) = @_;
    return ($date % 100);
}

sub 'get_hour {
    local ($time) = @_;
    return (int ($time / 1000000));
}

sub 'get_minute {
    local ($time) = @_;
    return (int (($time % 1000000) / 10000));
}

sub 'get_second {
    local ($time) = @_;
    return (int (($time % 10000) / 100));
}

sub 'get_centi {
    local ($time) = @_;
    return ($time % 100);
}

sub 'make_date {
    local ($century, $year, $month, $day) = @_;
    return ($century * 1000000 + $year * 10000 + $month * 100 + $day);
}

sub 'make_time {
    local ($hour, $minute, $second, $centi) = @_;
    return ($hour * 1000000 + $minute * 10000 + $second * 100 + $centi);
}

sub 'timeeq {
    local ($date1, $time1, $date2, $time2) = @_;
    return ($date1 == $date2 && $time1 == $time2);
}

sub 'timeneq {
    local ($date1,$time1,$date2,$time2) = @_;
    return ($date1 != $date2 || $time1 != $time2);
}

sub 'timelt {
    local ($date1,$time1,$date2,$time2) = @_;
    return ($date1 < $date2 || ($date1 == $date2 && $time1 < $time2));
}

sub 'timele {
    local ($date1,$time1,$date2,$time2) = @_;
    return ($date1 < $date2 || ($date1 == $date2 && $time1 <= $time2));
}

sub 'timegt {
    local ($date1,$time1,$date2,$time2) = @_;
    return ($date1 > $date2 || ($date1 == $date2 && $time1 > $time2));
}

sub 'timege {
    local ($date1,$time1,$date2,$time2) = @_;
    return ($date1 > $date2 || ($date1 == $date2 && $time1 >= $time2));
}


#   -------------------------------------------------------------------------
#   &date_now
#
#   Synopsis: Returns the current date as a long value (CCYYMMDD).  Since
#   most system clocks do not return a century, this function assumes that
#   all years 80 and above are in the 20th century, and all years 00 to 79
#   are in the 21st century.  For best results, consume before 1 Jan 2080.
#
#   Selftest:
#   &date_now > 19970524
#   &date_now == &'timer_to_date (date_to_timer (&date_now, &time_now))
#   -------------------------------------------------------------------------

sub 'date_now {
    local ($day, $month, $year) = (localtime) [3..5];
    return (&'make_date (0, $year + 1900, $month + 1, $day));
}


#   -------------------------------------------------------------------------
#   &time_now
#
#   Synopsis: Returns the current time as a long value (HHMMSSCC).  If the
#   system clock does not return centiseconds, these are set to zero.
#
#   Selftest:
#   &time_now == &'timer_to_time (date_to_timer (&date_now, &time_now))
#   -------------------------------------------------------------------------

sub 'time_now {
    local ($sec, $min, $hour) = (localtime) [0..2];
    return (&'make_time ($hour, $min, $sec, 0));
}


#   -------------------------------------------------------------------------
#   &leap_year (year_value)
#
#   Synopsis: Returns 1 if the year is a leap year. You must supply a
#   4-digit value for the year: 90 is taken to mean 90 ad.  Handles leap
#   centuries correctly.
#
#   Selftest:
#   &leap_year (1984) == 1
#   &leap_year (1985) == 0
#   &leap_year (1900) == 0
#   &leap_year (2000) == 1
#   -------------------------------------------------------------------------

sub 'leap_year {
    local ($year) = @_;                 #   Get subroutine arguments

    return (($year % 4 == 0 && $year % 100 != 0) || $year % 400 == 0);
}


#   -------------------------------------------------------------------------
#   &julian_date (date_value)
#
#   Synopsis: Returns the number of days since 31 December last year.  The
#   Julian date of 1 January is 1.
#
#   Selftest:
#   &julian_date (19970101) == 1
#   &julian_date (19970102) == 2
#   &julian_date (19970201) == 32
#   &julian_date (19971231) == 365
#   -------------------------------------------------------------------------

sub 'julian_date {
    local ($date) = @_;                 #   Get subroutine arguments

    local ($julian);
    $julian = $julian_base [&'get_month ($date) - 1]
            + &'get_day ($date);
    if (&'get_month ($date) > 2
    &&  &'leap_year (&'get_year ($date))) {
        $julian++;
    }
    return ($julian);
}


#   -------------------------------------------------------------------------
#   &day_of_week (date_value)
#
#   Synopsis: Returns the day of the week where 0 is Sunday, 1 is Monday,
#   ... 6 is Saturday.  Uses Zeller's Congurence algorithm.
#
#   Selftest:
#   &day_of_week (19961203) == 2
#   &day_of_week (19970525) == 0
#   --------------------------------------------------------------------------

sub 'day_of_week {
    local ($date) = @_;                 #   Get subroutine arguments

    local ($year)  = &'get_ccyear ($date);
    local ($month) = &'get_month  ($date);
    local ($day)   = &'get_day    ($date);
    if ($month > 2) {
        $month -= 2;
    }
    else {
        $month += 10;
        $year--;
    }
    $day = (int ((13 * $month - 1) / 5) + $day + ($year % 100) +
            int (($year % 100) / 4) + int (int ($year / 100) / 4) - 2 *
            int  ($year / 100) + 77);

    return ($day - 7 * int ($day / 7));
}


#   -------------------------------------------------------------------------
#   &week_of_year (date_value)
#
#   Synopsis: Returns the week of the year, where 1 is the first full week.
#   Week 0 may or may not exist in any year.  Uses a Lillian date algorithm
#   to calculate the week of the year.  The week starts on Sunday.
#
#   Selftest:
#   &week_of_year (19970524) == 20
#   &week_of_year (19970526) == 21
#   --------------------------------------------------------------------------

sub 'week_of_year {
    local ($date) = @_;                 #   Get subroutine arguments

    local ($year) = &'get_ccyear ($date) - 1501;
    local ($day)  = $year * 365 + int ($year / 4) - 29872 + 1
                  - int ($year / 100) + int (($year - 300) / 400);

    return (int ((&'julian_date ($date) + int (($day + 4) % 7)) / 7));
}


#   -------------------------------------------------------------------------
#   &year_quarter (date_value)
#
#   Synopsis: Returns the year quarter, 1 to 4, depending on the month
#   specified.
#
#   Selftest:
#   &year_quarter (19970331) == 1
#   &year_quarter (19970401) == 2
#   &year_quarter (19971231) == 4
#   --------------------------------------------------------------------------

sub 'year_quarter {
    local ($date) = @_;                 #   Get subroutine arguments
    return (int ((&'get_month ($date) - 1) / 3 + 1));
}


#   -------------------------------------------------------------------------
#   &default_century (date_value)
#
#   Synopsis: Supplies a default century for the year if necessary.  If
#   the year is 51 to 99, the century is set to 19.  If the year is 0 to
#   50, the century is set to 20.  Returns the adjusted date.
#
#   Selftest:
#   &default_century (19970525) == 19970525
#   &default_century (20070525) == 20070525
#   &default_century (970525)   == 19970525
#   &default_century ( 70525)   == 20070525
#   --------------------------------------------------------------------------

sub 'default_century {
    local ($date) = @_;                 #   Get subroutine arguments

    if (&'get_century ($date) == 0) {
        $date += &'get_year ($date) > 50? 19000000: 20000000;
    }
    return ($date);
}


#   -------------------------------------------------------------------------
#   &pack_date (date_value)
#
#   Synopsis: Packs the date into a single unsigned short word.  Use this
#   function to store dates when memory space is at a premium.  The packed
#   date can be used correctly in comparisons.  Returns the packed date.
#   The date must be later than 31 December 1979.
#
#   Selftest:
#   &pack_date (19800101) == 33
#   &pack_date (19970525) == 8889
#   &unpack_date (&pack_date ($date = &date_now ())) == $date
#   --------------------------------------------------------------------------

sub 'pack_date {
    local ($date) = @_;                 #   Get subroutine arguments

    return (((&'get_ccyear ($date) - 1980) << 9) +
             (&'get_month  ($date) << 5) +
              &'get_day    ($date));
}


#   -------------------------------------------------------------------------
#   &pack_time (time_value)
#
#   Synopsis: Packs the time into a single unsigned short word.  Use this
#   function to store times when memory space is at a premium.  The packed
#   time can be used correctly in comparisons.  Returns the packed time.
#   Seconds are stored with 2-second accuracy and centiseconds are lost.
#
#   Selftest:
#   &pack_time (12590000) == 26464;
#   &unpack_time (&pack_time ($time = &time_now ())) == int ($time / 200) * 200
#   --------------------------------------------------------------------------


sub 'pack_time {
    local ($time) = @_;                 #   Get subroutine arguments

    return ((&'get_hour   ($time) << 11) +
            (&'get_minute ($time) << 5)  +
            (&'get_second ($time) >> 1));
}


#   -------------------------------------------------------------------------
#   &unpack_date (packed_date)
#
#   Synopsis: Converts a packed date back into a long value.
#
#   Selftest:
#   &unpack_date (33)   == 19800101
#   &unpack_date (8889) == 19970525
#   &unpack_date (&pack_date ($date = &date_now ())) == $date
#   -------------------------------------------------------------------------

sub 'unpack_date
{
    local ($packdate) = @_;             #   Get subroutine arguments

    local ($year);
    $year = (($packdate & 0xfe00) >> 9) + 1980;

    return (&'make_date (0, $year,
                        ($packdate & 0x01e0) >> 5,
                        ($packdate & 0x001f)));
}


#   -------------------------------------------------------------------------
#   &unpack_time (packed_time)
#
#   Synopsis: Converts a packed time back into a long value.
#
#   Selftest:
#   &unpack_time (26464) == 12590000;
#   &unpack_time (&pack_time ($time = &time_now ())) == int ($time / 200) * 200
#   --------------------------------------------------------------------------

sub 'unpack_time {
    local ($packtime) = @_;             #   Get subroutine arguments

    return (&'make_time (($packtime & 0xf800) >> 11,
                         ($packtime & 0x07e0) >> 5,
                         ($packtime & 0x001f) << 1, 0));
}


#   -------------------------------------------------------------------------
#   &date_to_days (date_value)
#
#   Synopsis: Converts the date into a number of days since a distant but
#   unspecified epoch.  You can use this function to calculate differences
#   between dates, and forward dates.  Use &days_to_date to calculate the
#   reverse function.  Author: Robert G. Tantzen, translated from the Algol
#   original in Collected Algorithms of the CACM (algorithm 199).  Original
#   translation into C by Nat Howard, posted to Usenet 5 Jul 1985.  Perl'd
#   By Pieter.(tm)
#
#   Selftest:
#   &date_to_days (19970525) == 2450594;
#   &days_to_date (&date_to_days ($date = &date_now)) == $date
#   --------------------------------------------------------------------------

sub 'date_to_days {
    local ($date) = @_;                 #   Get subroutine arguments

    local ($year)    = &'get_year    ($date),
    local ($century) = &'get_century ($date),
    local ($month)   = &'get_month   ($date),
    local ($day)     = &'get_day     ($date);
    if ($month > 2) {
        $month -= 3;
    }
    else {
        $month += 9;
        if ($year) {
            $year--;
        }
        else {
            $year = 99;
            $century--;
        }
    }
    return (int ((146097 * $century)   / 4) +
            int ((1461   * $year)      / 4) +
            int ((153    * $month + 2) / 5) +
                           $day + 1721119);
}


#   -------------------------------------------------------------------------
#   &days_to_date (number_of_days)
#
#   Synopsis: Converts a number of days since some distant but unspecified
#   epoch into a date.  You can use this function to calculate differences
#   between dates, and forward dates.  Use &date_to_days to calculate the
#   reverse function.  Author: Robert G. Tantzen, translated from the Algol
#   original in Collected Algorithms of the CACM (algorithm 199).  Original
#   translation into C by Nat Howard, posted to Usenet 5 Jul 1985.
#
#   Selftest:
#   &date_to_days (19970525) == 2450594;
#   &days_to_date (&date_to_days ($date = &date_now)) == $date
#   --------------------------------------------------------------------------

sub 'days_to_date {
    local ($days) = @_;                 #   Get subroutine arguments

    local ($century, $year, $month, $day);
    $days   -= 1721119;
    $century = int ((4 * $days - 1) / 146097);
    $days    =       4 * $days - 1  - 146097 * $century;
    $day     = int ($days / 4);

    $year    = int ((4 * $day + 3) / 1461);
    $day     =       4 * $day + 3  - 1461 * $year;
    $day     = int (($day + 4) / 4);

    $month   = int ((5 * $day - 3) / 153);
    $day     =       5 * $day - 3  - 153 * $month;
    $day     = int (($day + 5) / 5);

    if ($month < 10) {
        $month += 3;
    }
    else {
        $month -= 9;
        $year++;                        #   $year may overflow to 100
    }
    return (&'make_date ($century, $year, $month, $day));
}


#   ---------------------------------------------------------------------[<]-
#   $timer = &date_to_timer (date_value, time_value)
#
#   Synopsis: Converts the supplied date and time into a timer value, which
#   is the number of non-leap seconds since 00:00:00 UTC January 1, 1970.
#
#   Selftest:
#   &date_now == &timer_to_date (date_to_timer (&date_now, &time_now))
#   ---------------------------------------------------------------------[>]-*/

sub 'date_to_timer {
    local ($date, $time) = @_;          #   Get subroutine arguments

    #   Get number of days since 1 January, 1970
    local ($days) = &'date_to_days ($date) - 2440588;
    local ($seconds) = ((  $days * 24
                         + &'get_hour   ($time) - $'daylight) * 60
                         + &'get_minute ($time)) * 60
                         + &'get_second ($time);
    return ($seconds + $'timezone);
}


#   ---------------------------------------------------------------------[<]-
#   &timer_to_date
#
#   Synopsis: Converts the supplied timer value into a long date value.
#   Dates are stored as long values: CCYYMMDD.  If the supplied value is
#   zero, returns zero.
#
#   Selftest:
#   &date_now == &timer_to_date (date_to_timer (&date_now, &time_now))
#   ---------------------------------------------------------------------[>]-*/

sub 'timer_to_date {
    local ($time_secs) = @_;            #   Get subroutine arguments

    if ($time_secs == 0) {
        return (0);
    }
    else {
        #   Convert into a long value CCYYMMDD
        local ($day, $month, $year) = (localtime ($time_secs)) [3..5];
        return (&'make_date (0, $year + 1900, $month + 1, $day));
    }
}


#   ---------------------------------------------------------------------[<]-
#   &timer_to_time
#
#   Synopsis: Converts the supplied timer value into a long time value.
#   Times are stored as long values: HHMMSS00.  Since the timer value does
#   not hold centiseconds, these are set to zero.  If the supplied value
#   was zero, returns zero.
#
#   Selftest:
#   &time_now == &timer_to_time (date_to_timer (&date_now, &time_now))
#   ---------------------------------------------------------------------[>]-*/

sub 'timer_to_time {
    local ($time_secs) = @_;            #   Get subroutine arguments

    if ($time_secs == 0) {
        return (0);
    }
    else {
        local ($sec, $min, $hour) = (localtime ($time_secs)) [0..2];
        return (&'make_time ($hour, $min, $sec, 0));
    }
}


#   ---------------------------------------------------------------------[<]-
#   &timer_to_gmdate
#
#   Synopsis: Converts the supplied timer value into a long date value in
#   Greenwich Mean Time (GMT).  Dates are stored as long values: CCYYMMDD.
#   If the supplied value is zero, returns zero.
#
#   Selftest:
#   &timer_to_gmdate (100000) == &timer_to_date (100000 + timezone)
#   ---------------------------------------------------------------------[>]-*/

sub 'timer_to_gmdate {
    local ($time_secs) = @_;            #   Get subroutine arguments

    if ($time_secs == 0) {
        return (0);
    }
    else {
        #   Convert into a long value CCYYMMDD
        local ($day, $month, $year) = (gmtime ($time_secs)) [3..5];
        return (&'make_date (0, $year + 1900, $month + 1, $day));
    }
}


#   ---------------------------------------------------------------------[<]-
#   &timer_to_gmtime
#
#   Synopsis: Converts the supplied timer value into a long time value in
#   Greenwich Mean Time (GMT).  Times are stored as long values: HHMMSS00.
#   On most systems the clock does not return centiseconds, so these are
#   set to zero.  If the supplied value is zero, returns zero.
#
#   Selftest:
#   &timer_to_gmtime (100000) == &timer_to_time (100000 + $timezone)
#   ---------------------------------------------------------------------[>]-*/

sub 'timer_to_gmtime {
    local ($time_secs) = @_;            #   Get subroutine arguments

    if ($time_secs == 0) {
        return (0);
    }
    else {
        local ($sec, $min, $hour) = (gmtime ($time_secs)) [0..2];
        return (&'make_time ($hour, $min, $sec, 0));
    }
}


#   -------------------------------------------------------------------------
#   &time_to_csecs (time_value)
#
#   Synopsis: Converts a time (HHMMSSCC) into a number of centiseconds.
#
#   Selftest:
#   &csecs_to_time (&time_to_csecs ($time = &time_now)) == $time
#   --------------------------------------------------------------------------

sub 'time_to_csecs {
    local ($time) = @_;                 #   Get subroutine arguments

    return (&'get_hour   ($time) * $'INTERVAL_HOUR
          + &'get_minute ($time) * $'INTERVAL_MIN
          + &'get_second ($time) * $'INTERVAL_SEC
          + &'get_centi  ($time));
}


#   -------------------------------------------------------------------------
#   &csecs_to_time (centiseconds)
#
#   Synopsis: Converts a number of centiseconds (< INTERVAL_DAY) into a
#   time value (HHMMSSCC).
#
#   Selftest:
#   &csecs_to_time (&time_to_csecs ($time = &time_now)) == $time
#   --------------------------------------------------------------------------

sub 'csecs_to_time {
    local ($csecs) = @_;                #   Get subroutine arguments

    local ($hour, $min, $sec);
    $hour  = int ($csecs / $'INTERVAL_HOUR);
    $csecs =      $csecs % $'INTERVAL_HOUR;
    $min   = int ($csecs / $'INTERVAL_MIN);
    $csecs =      $csecs % $'INTERVAL_MIN;
    $sec   = int ($csecs / $'INTERVAL_SEC);
    $csecs =      $csecs % $'INTERVAL_SEC;

    return (&'make_time ($hour, $min, $sec, $csecs));
}


#   -------------------------------------------------------------------------
#   ($date, $time) = &future_date ($date, $time, days, csecs)
#
#   Synopsis: Calculates a future date and time from the date and time
#   specified, plus an interval specified in days and 1/100th seconds.
#   The date can be any date since some distant epoch (around 1600).
#   If the date and time arguments are both zero, the current date and
#   time are used.
#
#   Selftest:
#   &future_date (19970525, 12000000, 1, 100) == (19970526, 12000100)
#   &future_date (19970531, 23595900, 0, 100) == (19970601,        0)
#   --------------------------------------------------------------------------

sub 'future_date {
    local ($date, $time, $days, $csecs) = @_;

    #   Set date and time to NOW if necessary
    if ($date == 0 && $time == 0) {
        $date = &'date_now;
        $time = &'time_now;
    }
    #   Get future date in days and centiseconds
    $days  = &'date_to_days  ($date) + $days;
    $csecs = &'time_to_csecs ($time) + $csecs;

    #   Normalise overflow in centiseconds
    while ($csecs >= $'INTERVAL_DAY) {
        $days++;
        $csecs -= $'INTERVAL_DAY;
    }
    #   Convert date and time back into organised values
    $date = &'days_to_date  ($days);
    $time = &'csecs_to_time ($csecs);

    return ($date, $time);
}


#   -------------------------------------------------------------------------
#   ($date, $time) = &past_date ($date, $time, days, csecs)
#
#   Synopsis: Calculates a past date and time from the date and time
#   specified, minus an interval specified in days and 1/100th seconds.
#   The date can be any date since some distant epoch (around 1600).
#   If the date and time arguments are both zero, the current date and
#   time are used.
#
#   Selftest:
#   &past_date (19970526, 12000100, 1, 100) == (19970525, 12000000)
#   &past_date (19970601,        0, 0, 100) == (19970531, 23595900)
#   --------------------------------------------------------------------------

sub 'past_date {
    local ($date, $time, $days, $csecs) = @_;

    #   Set date and time to NOW if necessary
    if ($date == 0 && $time == 0) {
        $date = &'date_now;
        $time = &'time_now;
    }
    #   Get past date in days and centiseconds
    $days  = &'date_to_days  ($date) - $days;
    $csecs = &'time_to_csecs ($time) - $csecs;

    #   Normalise underflow in centiseconds
    while ($csecs < 0) {
        $days--;
        $csecs += $'INTERVAL_DAY;
    }
    #   Convert date and time back into organised values
    $date = &'days_to_date  ($days);
    $time = &'csecs_to_time ($csecs);

    return ($date, $time);
}


#   -------------------------------------------------------------------------
#   ($days, $csecs) = &date_diff ($date1, $time1, $date2, $time2)
#
#   Synopsis: Calculates the difference between two date/time values, and
#   returns the difference as a number of days and a number of centiseconds.
#   The date can be any date since some distant epoch (around 1600).  The
#   calculation is date1:time1 - date2:time2.  The returned values may be
#   negative.
#
#   Selftest:
#   &date_diff (19970526, 12000100, 19970525, 12000000) == (1, 100)
#   --------------------------------------------------------------------------

sub 'date_diff {
    local ($date1, $time1, $date2, $time2) = @_;

    local ($days, $csecs);
    $days  = &'date_to_days  ($date1) - &'date_to_days  ($date2);
    $csecs = &'time_to_csecs ($time1) - &'time_to_csecs ($time2);
    return ($days, $csecs);
}


#   -------------------------------------------------------------------------
#   &valid_date (date_value)
#
#   Synopsis: Returns 1 if the date is valid or zero; returns 0 if the date
#   is not valid.
#
#   Selftest:
#   &valid_date (19970526) == 1
#   &valid_date (19970532) == 0
#   &valid_date (19970229) == 0
#   &valid_date (20000229) == 1
#   --------------------------------------------------------------------------

sub 'valid_date {
    local ($date) = @_;                 #   Get subroutine arguments

    local ($month, $day, $feedback);
    $month = &'get_month ($date);
    $day   = &'get_day   ($date);

    if ($date == 0) {
        $feedback = 1;                  #  Zero date is okay
    }
    elsif ($month < 1 || $month > 12) {
        $feedback = 0;                  #   Month out of range
    }
    elsif (($day < 1 || $day > $month_days [$month - 1])
    ||     ($month == 2 && $day == 29
        && !&'leap_year (&'get_year ($date)))) {
        $feedback = 0;                  #   Day out of range
    }
    else {
        $feedback = 1;                  #   Zero date is okay
    }
    return ($feedback);
}


#   -------------------------------------------------------------------------
#   &valid_time (time_value)
#
#   Synopsis: Returns TRUE if the time is valid or zero; returns FALSE if
#   the time is not valid.
#
#   Selftest:
#   &valid_time (19596000) == 0
#   &valid_time (19595999) == 1
#   &valid_time (24000000) == 0
#   &valid_time (       0) == 1
#   --------------------------------------------------------------------------

sub 'valid_time {
    local ($time) = @_;                 #   Get subroutine arguments

    return (&'get_second ($time) < 60
        &&  &'get_minute ($time) < 60
        &&  &'get_hour   ($time) < 24);
}


#   -------------------------------------------------------------------------
#   &date_is_future (date_value, time_value)
#
#   Synopsis: Returns TRUE if the specified date and time are in the future.
#   Returns FALSE if the date and time are in the past, or the present (which
#   will be the past by the time you've read this).  Date is specified as a
#   YYYYMMDD value; time as HHMMSSCC.
#
#   Selftest:
#   &date_is_future (&future_date (&date_now, &time_now, 0, 1000)) == 1
#   &date_is_future (&future_date (&date_now, &time_now, 0, 0))    == 0
#   --------------------------------------------------------------------------

sub 'date_is_future {
    local ($date, $time) = @_;          #   Get subroutine arguments

    return ($date  > &'date_now
        ||  $date == &'date_now && $time > &'time_now);
}


#   -------------------------------------------------------------------------
#   &date_is_past (date_value, time_value)
#
#   Synopsis: Returns TRUE if the specified date and time are in the past.
#   Returns FALSE if the date and time are in the future or present (which
#   despite any assertion to the contrary, is not the past.  Although that
#   may change soon).  Date is specified as YYYYMMDD; time as HHMMSSCC.
#
#   Selftest:
#   &date_is_past (&past_date (&date_now, &time_now, 0, 1000)) == 1
#   &date_is_past (&past_date (&date_now, &time_now, 0, 0))    == 0
#   --------------------------------------------------------------------------

sub 'date_is_past {
    local ($date, $time) = @_;          #   Get subroutine arguments

    return ($date  < &'date_now
        ||  $date == &'date_now && $time < &'time_now);
}
