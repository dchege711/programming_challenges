---
draft: true
----

# [205 (Easy) Friendly Date Ranges](https://www.reddit.com/r/dailyprogrammer/comments/2ygsxs/20150309_challenge_205_easy_friendly_date_ranges/)

For the original [r/dailyprogrammer](https://www.reddit.com/r/dailyprogrammer/) post and discussion, click the link in the title.

#  (Easy): Friendly Date Ranges
(#EasyIcon)
The goal of this challenge is to implement a way of converting two dates into a more friendly date range that could be presented to a user. It must not show any redundant information in the date range. For example, if the year and month are the same in the start and end dates, then only the day range should be displayed. Secondly, if the starting year is the current year, and the ending year can be inferred by the reader, the year should be omitted also (see below for examples).

# Formal Inputs and Outputs
## Input Description
The input will be two dates in the YYYY-MM-DD format, such as:


```
YYYY-MM-DD
```

```
2015-07-01 2015-07-04
```

```
2015-12-01 2016-02-03
```

```
2015-12-01 2017-02-03
```

```
2016-03-01 2016-05-05
```

```
2017-01-01 2017-01-01
```

```
2022-09-05 2023-09-04
```
## Output Description
The program must turn this into a human readable date in the Month Day, Year format (omitting the year where possible). These outputs correspond to the above inputs:


```
Month Day, Year
```

```
July 1st - 4th
```

```
December 1st - February 3rd
```

```
December 1st, 2015 - February 3rd, 2017
```

```
March 1st - May 5th, 2016
```

```
January 1st, 2017
```

```
September 5th, 2022 - September 4th, 2023
```
## Edge Case 1
If the starting year is the current year, but the ending year isn't and the dates are at least a year apart, then specify the year in both. For example, this input:


```
2015-04-01 2020-09-10
```
Must not omit the 2015, so it should output April 1st, 2015 - September 10th, 2020, and NOT April 1st - September 10th, 2020, which would otherwise be ambiguous.


```
April 1st, 2015 - September 10th, 2020
```

```
April 1st - September 10th, 2020
```
Of course if the dates are less than a year apart, as in the case of 2015-12-01 2016-02-03, then you can safely omit the years (December 1st - February 3rd), as that makes it clear that it's the February next year.


```
2015-12-01 2016-02-03
```

```
December 1st - February 3rd
```
## Edge Case 2
Similarly, if the starting year is the current year, but the two dates are exactly one year apart, also specify the year in both. For example, this input:


```
2015-12-11 2016-12-11
```
Must specify both years, i.e. December 11th, 2015 - December 11th, 2016.


```
December 11th, 2015 - December 11th, 2016
```
# Bonus (Intermediate)
Of course, not all users will want to read a Month Day, Year format. To fix this, allow your program to receive hints on how to format the dates, by accepting a date format as a third parameter, for example:


```
Month Day, Year
```

```
2015-07-01 2015-07-04 DMY
```

```
2016-03-01 2016-05-05 YDM
```

```
2022-09-05 2023-09-04 YMD
```
would produce:


```
1st - 4th July
```

```
2016, 1st March - 5th May
```

```
2022, September 5th - 2023, September 4th
```
You only need to handle date format strings DMY, MDY, YMD and YDM.


```
DMY
```

```
MDY
```

```
YMD
```

```
YDM
```
# Special Thanks
Special thanks to /u/pogotc for creating this challenge in /r/DailyProgrammer_Ideas! If you have your own idea for a challenge, submit it there, and there's a good chance we'll post it.

(/u/pogotc)
(/r/DailyProgrammer_Ideas)

----
## **DISCLAIMER**
This prompt has been adapted from [205 [Easy] Friendly Date Ranges](https://www.reddit.com/r/dailyprogrammer/comments/2ygsxs/20150309_challenge_205_easy_friendly_date_ranges/
)
