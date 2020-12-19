###FOR THE COMMUNITY MAPPING LAB'S RESEARCH WITH UNITED WAY###

# equals an error
## equals notes for reader

library(janitor)
library(googlesheets4)
library(tidyverse)
library(knitr)
library(readr)

googleDriveAgency <- read_sheet("https://docs.google.com/spreadsheets/d/1PMpG8bZCxVb2Ajx4NuORsGJJ_CF1XCrN0H-bIBEebRs/edit#gid=1545683915", sheet = "Agency")
googleDriveSite <- read_sheet("https://docs.google.com/spreadsheets/d/1PMpG8bZCxVb2Ajx4NuORsGJJ_CF1XCrN0H-bIBEebRs/edit#gid=275719177", sheet = "Site")
googleDriveServiceGroup <- read_sheet("https://docs.google.com/spreadsheets/d/1PMpG8bZCxVb2Ajx4NuORsGJJ_CF1XCrN0H-bIBEebRs/edit#gid=110835911", sheet = "ServiceGroup")
googleDriveServiceSite <- read_sheet("https://docs.google.com/spreadsheets/d/1PMpG8bZCxVb2Ajx4NuORsGJJ_CF1XCrN0H-bIBEebRs/edit#gid=1173655008", sheet = "ServiceSite")


##agency_key is for each non-profit
##location_ID is specific to each cite of each agency
##site key indicates a new site for each agency (NOT A UNIQUE VALUE)

googleDriveSiteCleaned <- googleDriveSite %>%
  select(Location_ID, Agency_Key)
joinedSiteServiceSite = full_join(googleDriveSiteCleaned, googleDriveServiceSite) %>%
  group_by(SearchTerms.code, Agency_Key) %>%
  summarise(count = n()) %>%
  left_join(googleDriveAgency %>% 
              select(Agency_Key,Location_ID))

joinedSiteServiceSiteArea<-full_join(joinedSiteServiceSite,googleDriveServiceGroup %>%
  select(Location_ID,`AREA SERVED`) %>%
  rename(area_served=`AREA SERVED`))

joinedSiteServiceSiteAgency = full_join(joinedSiteServiceSiteArea, googleDriveAgency)
joinedSiteServiceSiteAgencyServiceGroup = full_join(joinedSiteServiceSiteAgency, googleDriveServiceGroup)
cleanedJoins = joinedSiteServiceSiteAgency %>%
  filter(Active == TRUE) %>%
  drop_na(area_served) %>%
  filter(area_served != "Georgia|Nation")
  select(Location_ID, Agency_Key, SearchTerms.code, Name, Address1, City, State, ZIPCode, area_served)


cleanedJoins <- cleanedJoins %>%
  mutate(cty_Banks=if_else(str_count(area_served, "Banks") == 1,1,0),
         cty_Barrow=if_else(str_count(area_served, "Barrow") == 1,1,0),
         cty_Clarke=if_else(str_count(area_served, "Clarke") == 1,1,0),
         cty_Elbert=if_else(str_count(area_served, "Elbert") == 1,1,0),
         cty_Franklin=if_else(str_count(area_served, "Franklin") == 1,1,0),
         cty_Greene=if_else(str_count(area_served, "Greene") == 1,1,0),
         cty_Hart=if_else(str_count(area_served, "Hart") == 1,1,0),
         cty_Jackson=if_else(str_count(area_served, "Jackson") == 1,1,0),
         cty_Madison=if_else(str_count(area_served, "Madison") == 1,1,0),
         cty_Morgan=if_else(str_count(area_served, "Morgan") == 1,1,0),
         cty_Oconee=if_else(str_count(area_served, "Oconee") == 1,1,0),
         cty_Oglethorpe=if_else(str_count(area_served, "Oglethorpe") == 1,1,0))

##now onto the service codes
##categories taken from https://docs.google.com/spreadsheets/d/13XXozF_klUAUwlhdpaY5LAoiW4MsbutyVqfQpKaQbL0/edit#gid=181505920
##categories listed in order from statewide reporting subfield categories

cleanedJoins <- cleanedJoins %>%
  mutate(service_Clothing_Personal_and_Household=if_else(str_count(SearchTerms.code, "BM-6500.1500") == 1,1,0),
         service_Disaster_Services=if_else(str_count(SearchTerms.code, "TH") == 1,1,0),
         service_Community_Shelters=if_else(str_count(SearchTerms.code, "BH-0500.7000|BH-1800.1500-080|BH-1800.1500-100|BH-1800.1500-140|BH-1800.1500-140|BH-1800.1500-200|BH-1800.1500-308|BH-1800.1500-330|BH-1800.1500-700|BH-1800.1500-800|BH-1800.8500-150|BH-1800.8500-300") == 1,1,0),
         service_Education=if_else(str_count(SearchTerms.code, "HD|HH|HL") == 1,1,0),
         service_Employment=if_else(str_count(SearchTerms.code, "ND") == 1,1,0),
         service_Food_and_Meals=if_else(str_count(SearchTerms.code, "BD-1800.1000|BD-1800.1900|BD-1800.2000|BD-1800.2250|BD-1800.5000|BD-1800.6400|BD-1800.8000|BD-1800.8200-500|BD-1800.8200-600|BD-5000.1470|BD.500.1500|BD-5000.3500|BD-5000.4500|BD-5000.5000|BD-5000.8300") == 1,1,0),
         service_Health_Care=if_else(str_count(SearchTerms.code, "LD|LE|LN") == 1,1,0),
         service_Public_Health=if_else(str_count(SearchTerms.code, "JP") == 1,1,0),
         service_Public_Safety=if_else(str_count(SearchTerms.code, "JR") == 1,1,0),
         service_Financial_Assistance=if_else(str_count(SearchTerms.code, "BH-3800.5000|BH-3800.5100|BH-3800.7200|BH-3800.7250|BH-3800.8000|BV-8900.9300|BV") == 1,1,0),
         service_Individual_Family_Community_Support=if_else(str_count(SearchTerms.code, "PB|PD") == 1,1,0),
         service_Resources_for_Families_with_Children=if_else(str_count(SearchTerms.code, "PH") == 1,1,0),
         service_Information_Services=if_else(str_count(SearchTerms.code, "TJ") == 1,1,0),
         service_Legal_Consumer_and_Public_Safety=if_else(str_count(SearchTerms.code, "BH-3500.3400-450|FC|FN|FP|FT") == 1,1,0),
         service_Mental_and_Behavioral_Health=if_else(str_count(SearchTerms.code, "RD|RF|RM|RP|RR") == 1,1,0),
         service_Behavioral_Health=if_else(str_count(SearchTerms.code, "RX") == 1,1,0),
         service_Government=if_else(str_count(SearchTerms.code, "NL|TD-0300.1200|TD-0300.1300|TD-0300.2000|TD-0300.6000|TD-0300.8000|TD-0350|TE") == 1,1,0),
         service_Social_Insurance_Programs=if_else(str_count(SearchTerms.code, "NS") == 1,1,0),
         service_Support_Groups=if_else(str_count(SearchTerms.code, "PN") == 1,1,0),
         service_Transportation=if_else(str_count(SearchTerms.code, "BT-4500.4500|BT-4500.4700|BT4500.6500|BT-4800|BT-8300|BT-8400|BT-8500.1000") == 1,1,0)
  )

cleanedJoins_agg<-cleanedJoins %>%
  pivot_longer(cty_Banks:service_Transportation,names_to="var",values_to="dummy") %>%
  group_by(Agency_Key, Name, var) %>%
             summarise(dummy=max(dummy)) %>%
  pivot_wider(names_from=var,values_from=dummy)

cleaned_Joins_agg1 <- cleanedJoins_agg %>%
  pivot_longer(cty_Banks:cty_Oglethorpe,names_to="county",values_to="dummy")  %>%
  filter(dummy == 1) %>%
  group_by(county) %>%
  summarise(service_Clothing_Personal_and_Household=sum(service_Clothing_Personal_and_Household, na.rm = TRUE),
            service_Disaster_Services=sum(service_Disaster_Services, na.rm = TRUE),
            service_Community_Shelters=sum(service_Community_Shelters, na.rm = TRUE),
            service_Education=sum(service_Education, na.rm = TRUE),
            service_Employment=sum(service_Employment, na.rm = TRUE),
            service_Food_and_Meals=sum(service_Food_and_Meals, na.rm = TRUE),
            service_Health_Care=sum(service_Health_Care, na.rm = TRUE),
            service_Public_Health=sum(service_Public_Health, na.rm = TRUE),
            service_Public_Safety=sum(service_Public_Safety, na.rm = TRUE),
            service_Financial_Assistance=sum(service_Financial_Assistance, na.rm = TRUE),
            service_Individual_Family_Community_Support=sum(service_Individual_Family_Community_Support, na.rm = TRUE),
            service_Resources_for_Families_with_Children=sum(service_Resources_for_Families_with_Children, na.rm = TRUE),
            service_Information_Services=sum(service_Information_Services, na.rm = TRUE),
            service_Legal_Consumer_and_Public_Safety=sum(service_Legal_Consumer_and_Public_Safety, na.rm = TRUE),
            service_Mental_and_Behavioral_Health=sum(service_Mental_and_Behavioral_Health, na.rm = TRUE),
            service_Government=sum(service_Government, na.rm = TRUE),
            service_Social_Insurance_Programs=sum(service_Social_Insurance_Programs, na.rm = TRUE),
            service_Support_Groups=sum(service_Support_Groups, na.rm = TRUE),
            service_Transportation=sum(service_Transportation, na.rm = TRUE)
            )

write_csv(cleaned_Joins_agg1, "final_table_11_17_20.csv")
write_csv(cleanedJoins_agg, "agencies_reference_11_18_20.csv")
