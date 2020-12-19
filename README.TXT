FROM GEOGRAPHY CURO RESEARCH ASSISTANCESHIP FALL 2020 WITH UNITED WAY

## NOTES ON HOW TO READ DATAFRAMES PULLED FROM RTM ##

We pulled data from Refernet, uploaded to Google Sheets as CSVs, then read those files into R using the package googlesheets4.
We had four tables of raw data, and joined then filtered each to contain the data we found pertinent:

AGENCY KEY is for each agency (can be duplicated, may have an address listed).
  Some agencies have a site and some do not.
Name (of agency)
SITE KEY is for each site (if an agency has multiple locations).
  Site key is not a unique value; it just tells you how many sites there are and which one it is.
  All agencies are their own site.
LOCATION ID is a unique value for each site and agency.
AREA SERVED is delineated by agency, not by site.
SearchTerms.code includes prefixes and numbers used to categorize services offered.  
  These prefixes are specific to United Way.  The broader categories in these prefixes were what we used to categorize services.  More info can be found here: https://docs.google.com/spreadsheets/d/13XXozF_klUAUwlhdpaY5LAoiW4MsbutyVqfQpKaQbL0/edit#gid=181505920

We chose to measure coverage by areas served by each agency.  To create our final tables, we first created dummy variables for both counties served and services offered by agencies.  Once our dummy tables were filled, we created a summary table of how many agencies of each category of services were offered in each county.  We took that data to Excel and manually calculated our rates which we then graphed and represented spatially.  Final results can be found at: https://docs.google.com/spreadsheets/d/1tzPAbbcksHpdOZVSbeu8d92_KzYpR2qhtqY9Ygaprr0/edit?usp=sharing