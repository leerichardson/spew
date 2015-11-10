README
SCHOOLS
MIDAS
AUGUST 5, 2014

School data is from NCES.  Please see http://nces.ed.gov/ccd/elsi/tableGenerator.aspx.

There are currently two files containing school data.

ELSI_2010_private.csv
-This data is for the 2009-2010 school year due to availability
-variables:  Private.School.Name, State.Name, State.Code, School.ID, County.Code (5 digits), School.Level, Mailing.Address, City, ZIP, Total.Students

#Contents:  Private school data for the entire US+DC for 2009-2010.
#READING the File:  Skip the first 6 lines.  The last 5 lines contain metadata.
#NOTE:  This file includes NA values; may include '=' in the state/county codes
#The private schools have mailing addresses but no latitude and longitude

ELSI_2011_public.csv
-This data is for the 2009-2010 school year due to availability
-variables:  School.Name, State.Name, School.ID, County.Number (5 digits), State.Code, Latitude, Longitude, School.Level, Total.Students

#Contents:  Public school data for the entire US+DC for 2010-2011 school year.
#READING the File:  Skip the first 6 lines.  The last 5 lines contain metadata.
#NOTE:  This file includes NA values; may include '=' in the state/county codes

