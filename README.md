# Calendar

Print simple text calendars in a format similar to Unix cal. The only
exported function, CALENDAR, can be controlled with keyword arguments.

The month can be specified as either an integer in the interval [1,12]
or a keyword representing the name of the month, eg :SEPTEMBER. Years
should be given as integers.

If neither a month or year is specified, the calendar for the current
month will be printed. If a month and no year is given, a calendar
will be printed for that month of the current year. If a year and no
month is given, a calendar will be printed for that entire year.

The calendar to use can be specified as either :GREGORIAN or
:JULIAN. By default, Julian will be used for years before 1752,
and Gregorian otherwise.

When printing a calendar, as many months as fit will be printed in
each row. The number of columns in a row is given in SCREEN-WIDTH,
which defaults to 80. An error will be signalled if CALENDAR is called
with a SCREEN-WIDTH of less than 20.
