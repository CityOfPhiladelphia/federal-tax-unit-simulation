rm(list = ls())
setwd("~/Americorp VISTA")

##### Load Data #####

library(tidyverse)
library(haven)
library(scales)

data <- read.csv("~/Americorp VISTA/Joined Data for SPM Tax Analysis.csv")
data_tax <-read.csv("~/Americorp VISTA/data_tax_nums.csv")
data_reform <- read.csv("~/Americorp VISTA/data_tax_nums-18-#-ARP-#.csv")
data_baseline <- read.csv("~/Americorp VISTA/data_tax_nums-18-#-#-#.csv")

colnames(data_baseline) <- paste("Base",colnames(data_baseline), sep = "_") ## Rename baseline numbers
data_baseline <- data_baseline %>% rename(RECID = Base_RECID)


data_tax_reform <- left_join(data_tax,data_reform,by = "RECID")
data_tax_reform <- left_join(data_tax_reform,data_baseline,by = "RECID")
data_joined_reform <- left_join(data,data_tax_reform,by = "TAXID")

data_joined_reform <- data_joined_reform %>%
  group_by(SPM_ID) %>%
  mutate(reform_hh_tax = sum(unique(combined)),
         base_hh_tax = sum(unique(Base_combined)))

data_joined_reform <- data_joined_reform %>%
  rowwise() %>% 
  mutate(reform_resources = sum(c(SPM_Totval,-reform_hh_tax,SPM_SnapSub,SPM_CapHouseSub,SPM_SchLunch,SPM_EngVal,SPM_WICval,-SPM_StTax,-SPM_CapWkCCXpns,-SPM_MedXpns),na.rm = TRUE),
         base_resources = sum(c(SPM_Totval,-base_hh_tax,SPM_SnapSub,SPM_CapHouseSub,SPM_SchLunch,SPM_EngVal,SPM_WICval,-SPM_StTax,-SPM_CapWkCCXpns,-SPM_MedXpns),na.rm = TRUE),
         reform_pov = if_else(reform_resources<SPM_PovThreshold,1,0),
         reform_deeppov =if_else(reform_resources<(.5*SPM_PovThreshold),1,0),
         base_pov = if_else(base_resources<SPM_PovThreshold,1,0),
         base_deeppov =if_else(base_resources<(.5*SPM_PovThreshold),1,0),
         SPM_deeppov = if_else(SPM_Resources<(.5*SPM_PovThreshold),1,0),
         local_base_resources = sum(c(SPM_Totval*.97,-base_hh_tax,SPM_SnapSub,SPM_CapHouseSub,SPM_SchLunch,SPM_EngVal,SPM_WICval,-SPM_StTax,-SPM_CapWkCCXpns,-SPM_MedXpns),na.rm = TRUE),
         local_reform_pov = if_else(local_base_resources<SPM_PovThreshold,1,0),
        )

data_joined_reform <- data_joined_reform %>%
  mutate(race_recode = case_when(
    race == 1 & hispanic == 0 ~ "White (Non-Hispanic)",
    race == 2 & hispanic == 0 ~ "Black (Non-Hispanic)",
    race == 3 & hispanic == 0 ~ "Asian (Non-Hispanic)",
    race == 4 & hispanic == 0 ~ "Other (Non-Hispanic)",
    hispanic == 1 ~ "Hispanic (Any Race)"
  ))

data_joined_reform <- data_joined_reform %>%
  mutate(age_recode = case_when(
    age < 18 ~ "Child",
    age > 65 ~ "Senior",
    TRUE ~ "Adult"
    ))

hh_data <- data_joined_reform %>%
  filter(puma %in% c(4203201:4203211)) %>%
  group_by(SPM_ID) %>%
summarise(base_resources = base_resources,
          reform_resources = reform_resources,
          closedgap_resources = if_else(reform_pov == 1,SPM_Resources,reform_resources),
          wt = sum(wt.x)) %>%
  ggplot(aes(x=base_resources))






write_csv(data_joined_reform,"Final Joined Tax and Reform Data")

data_joined_reform %>%
  group_by(TAXID) %>%
  mutate(kids_here = if_else(n)) %>%
  summarise(n=sum(PERWT*kids_here)) %>%
  count(n)



data_joined_reform  %>%
  filter(puma %in% c(4203201:4203211)) %>%
  group_by(race_recode,base_pov) %>%
  summarise_at(vars(PWGTP1:PWGTP80),sum) %>%
  ungroup(base_pov) %>%
  mutate_at(vars(PWGTP1:PWGTP80),function(x) {x / sum(x)}) %>%
  rowwise() %>%
  mutate(ci_low = quantile(c_across(PWGTP1:PWGTP80),.025),
         est = mean(c_across(PWGTP1:PWGTP80)),
         ci_high = quantile(c_across(PWGTP1:PWGTP80),probs = .975)) %>%
  select(race_recode,base_pov,ci_low,est,ci_high) %>%
  filter(base_pov == 1)

  
  ?percent
  

install.packages("scales")



data_joined_reform %>%
  filter(puma %in% c(4203201:4203211)) %>%
  summarise(x = sum(SPM_PovThreshold),
            y = sum(reform_resources),
            z = sum(base_resources)) %>%
  summarise(thresh = sum(x),
            reform = sum(y),
            base = sum(z)) %>%
  summarise(Reform_Gap = thresh - reform, 
            Base_Gap = thresh - base, 
            Difference = Base_Gap - Reform_Gap
            )
  


data_joined_reform %>%
  filter(puma %in% c(4203201:4203211)) %>%
  group_by(SPM_ID) %>%
  mutate(spm_hh_wt=sum(wt.x)) %>%
  summarise(base = sum(base_resources),
            reform = sum(reform_resources),
            wt = spm_hh_wt) %>%
  summarise(base = sum(base*wt),
            reform = sum(reform*wt),
            difference = reform-base) %>%
  summarise(base=sum(base),
            reform = sum(reform),
            difference = sum(difference))


data_joined_reform <- data_joined_reform %>%
  mutate(inc_groups = case_when(
    SPM_Totval %in% c(0:4999) ~ "$1 - $4,999",
    SPM_Totval %in% c(5000:9999) ~ "$5,000 - $9,999",
    SPM_Totval %in% c(10000:14999) ~"10,000 - $14,999",
    SPM_Totval %in% c(15000:19999) ~ "$15,000 - $19,999",
    SPM_Totval > 19999 ~ "$20,000 or more"
  ))


data_joined_reform %>%
  filter(puma %in% c(4203201:4203211)) %>%
  filter(SPM_CapHouseSub > 0) %>%
  group_by(inc_groups) %>%
  summarise(mean = paste0("$",weighted.mean(.$SPM_CapHouseSub,.$wt.x)/12),
            income = weighted.mean(.$SPM_Totval,.$wt.x),
            count = sum(wt.x)) %>%
  mutate(   percent = percent(count/sum(count),accuracy = 1),
            HUD_Records = percent(c(9,36,18,12,25)/100,accuracy = 2)) %>%
  arrange(.,c(1,4,5,2,3)) -> results
  


data_joined_reform %>% 
  ggplot(aes(x=SPMresources-reform_resources)) + geom_histogram()
