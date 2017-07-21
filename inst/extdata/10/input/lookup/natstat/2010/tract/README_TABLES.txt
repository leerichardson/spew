README
TABLES
MIDAS
JULY 24, 2014

The geographies incorporated in the U.S. Census can be complicated and not always nested within one another.  See http://factfinder2.census.gov/faces/nav/jsf/pages/using_factfinder5.xhtml

Each geographical unit has a number attached to it.  The geographical delineations change from 2000 to 2010, but the changes are not implemented in the ACS files until 2012.  We use two hierarchies.

1.  State-County-Tract
11 digits
If using the FIPS codes, this will uniquely identify any tract in the US.
e.g.  The tract CMU is located in is 42003982200 (42-PA, 003- Allegheny County, 982200-tract CMU is in)

2.  State-PUMA
7 digits
This will uniquely identify a PUMA.  (NOTE:  PUMAs change drastically from 2000 to 2010)
e.g.  The PUMA CMU is located in is 4201701.

FILES
lookup00_v3.csv
This is a table of the 2000 defined PUMAs and counties. 
VARIABLES: st (state FIPS), co ( county FIPS), puma (PUMA number), st.names (Full state name, uppercase), stco (5 digit FIPS Code for State then County), co.names (county names), and Abbrevation (state abbreviation)
Both counties and PUMAs can be listed multiple times.

lookup10.csv
This is a table of 2010 defined PUMAS, states, counties, and tracts.  There is a listing for each tract in the US.
VARIABLES: X.1 and X (extra indices), STATEFP (State Fips Number), COUNTYFP (County FIPS Number), TRACTCE (Tract number), PUMA5CE (PUMA number), STATEAB (State Abbrev.), county.name (county name), state.name, (state name), GEOID (11 digit 2 digit state, 3 digit county, 6 digit tract numbers), and STCO (5 digits 2 state, 3 county)

states.csv
Table of U.S. state names (and DC) and two letter abbreviation

tracts00.csv
11 digit Tract GEOID for each tract in the US.
state-county-tract

tracts00_df.csv
Data frame with a spit GEOID for each tract
VARIABLES:  st (state FIPS), co (county FIPS), tract.no (6 digit tract number)

