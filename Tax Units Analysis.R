rm(list = ls())
setwd("~/Americorp VISTA")

##### Load Data #####

library(tidyverse)
library(haven)
library(ipumsr)

options(scipen=999)

`%notin%` <- Negate(`%in%`)

spm_pu_2018 <- read_sas("spm_pu_2018.sas7bdat", 
                        NULL)

spm_pa <- filter(spm_pu_2018,st == 42)
psam_p42 <- read.csv("~/Americorp VISTA/psam_p42.csv")
psam_p42$SERIAL_recode <- str_sub(psam_p42$SERIALNO,-7,-1)

## Add IPUMS Data
ddi <- read_ipums_ddi("usa_00014.xml")
IPUMS <- read_ipums_micro(ddi)
IPUMS$SERIAL_recode <- str_sub(IPUMS$CBSERIAL,-7,-1)

data_joined <- bind_cols(psam_p42,IPUMS)

## Filter out Group Quarters
data_joined <- filter(data_joined,grepl("HU",SERIALNO))

## Join Brian's data and ACS data
data_joined <- bind_cols(spm_pa,data_joined)


##### Tax Units ####
  ## create Tax IDs ##


      # pad subfamilies and famunits to be width of two

  data_joined <- data_joined %>% 
  mutate(SUBFAM_pad = str_pad(SUBFAM,2,side = "left",pad = "0"),
         FAMUNIT_pad = str_pad(FAMUNIT,2,side = "left",pad = "0"),
         PERNUM_pad = str_pad(PERNUM,2,side = "left",pad = "0"))
  
  
    # create TAXIDs with serial number, family unit, and subfamily unit

    data_joined <- data_joined %>% 
      mutate(TAXID = as.numeric(paste0(SERIAL,0,FAMUNIT_pad,SUBFAM_pad)))

    ## Create column for Tax Unit Size for diagnostic purposes
    
    data_joined <- data_joined %>%
      group_by(TAXID) %>% 
      mutate(TAXSIZE = n())
  
    
  ## Remove all people whose SUBFAM == 0 and who are NOT married spouses, kids under 18, college kids, people w disabilities who don't work, non-married spouses who don't work
      # create flag for people who should remain in the O subfamily (dependents, spouses, and children)

    data_joined <- data_joined %>% 
      mutate(dependent_flag = case_when(
        (SUBFAM == 0 & RELATE == 1) |
        (SUBFAM == 0 & age < 18) |
        (SUBFAM == 0 & SPLOC != 0 & MAR == 1) |
        (SUBFAM == 0 & SPLOC != 0 & PINCP %in% c(0:4300) & MAR %in% c(2:5)) |
        (SUBFAM == 0 & age %in% c(18:23) & SCH %in% c(2:3)) |
        (SUBFAM == 0 & DIS ==  1 & is.na(WKHP)) ~ 1
      ))

    ## Send people who are not in the dependent status to have a different TAXID, based off of the person identifier (PERNUM)
    
    data_joined <- data_joined %>%
      mutate(TAXID = case_when(
        SUBFAM == 0 & dependent_flag == 1 ~ as.numeric(paste0(SERIAL,0,FAMUNIT_pad,SUBFAM_pad)),
        SUBFAM == 0 & is.na(dependent_flag) ~ as.numeric(paste0(SERIAL,1,FAMUNIT_pad,PERNUM_pad)),
        SUBFAM != 0 ~ as.numeric(paste0(SERIAL,0,FAMUNIT_pad,SUBFAM_pad))))

  
## Add people who are have MOMLOC or POPLOC =/= 0 and who are between 18 and 23 and add them to the SUBFAM of their parent
   ## create flag for children to be in their parent's subfamily if they are in college
         
    data_joined <- data_joined %>% mutate(MOM_flag = if_else(MOMLOC != 0 & age %in% c(18:23) & SCH %in% c(2:3),1,0))
    data_joined <- data_joined %>% mutate(POP_flag = if_else(POPLOC != 0 & age %in% c(18:23) & SCH %in% c(2:3),1,0))
      
   ## Add them to their parent's SUBFAM
    
    data_joined <- data_joined %>%
      group_by(SERIAL) %>%
      mutate(SUBFAM = ifelse(
        POP_flag == 1,
        SUBFAM[PERNUM == POPLOC],
        SUBFAM))
    
    data_joined <- data_joined %>%
      group_by(SERIAL) %>%
      mutate(SUBFAM = ifelse(
        MOM_flag == 1,
        SUBFAM[PERNUM == MOMLOC],
        SUBFAM))
    
    # pad subfamilies and famunits to be width of two (repeat because of the college student stuff)
    
    data_joined <- data_joined %>% 
      mutate(SUBFAM_pad = str_pad(SUBFAM,2,side = "left",pad = "0"),
             FAMUNIT_pad = str_pad(FAMUNIT,2,side = "left",pad = "0"),
             PERNUM_pad = str_pad(PERNUM,2,side = "left",pad = "0"))
  
    ## Create a new TaxID for these college students
    
    data_joined <- data_joined %>% 
      mutate(TAXID = if_else(MOM_flag == 1 | POP_flag == 1,as.numeric(paste0(SERIAL,0,FAMUNIT_pad,SUBFAM_pad))))

    table(substr(data_joined$TAXID,8,12))
    
    table(data_joined$SUBFAM)

## Create flag for dependents
    
    data_joined <- data_joined %>% 
      mutate(dependent_flag2 = case_when(
        age < 18  ~ 1, 
        age %in% c(18:23) & SCH %in% c(2:3) ~ 1,
        DIS ==  1 & is.na(WKHP) & age > 17 ~ 1,
        SPLOC != 0 & PINCP %in% c(0:4300) & MAR %in% c(2:5) ~ 1,
        TRUE ~ 0
    ))
    
    ## create payer
  
    data_joined <- data_joined %>% 
      mutate(payer = case_when(
        dependent_flag2 == 0 ~ 1, 
        TRUE ~ 0
    ))
    
    ## Create Flag to Find which houesholds are Just dependents
  
    data_joined <- data_joined %>% 
      group_by(TAXID) %>%
      mutate(DepHH = case_when(n() - sum(dependent_flag2) == 0 ~ 1, 
                               TRUE ~ 0))

    
    # Create Payer and Spouse variable
    
    data_joined <-  data_joined %>%
      group_by(TAXID) %>%
      arrange(desc(payer)) %>%
      mutate(
        payer_id = 1:n(),
        payer_id = case_when(
          payer_id == 1 & payer ~ "Payer",
          payer_id == 2 & payer ~ "Spouse",
          ! payer ~ "Non-Payer",
        )
      )
    
    stopifnot(data_joined$payer_id %in% c("Payer", "Spouse", "Non-Payer"))

    ## Code in the Rules for how people end up as dependents for diagnosing why so many households of non-payers show up
    
    data_joined <- data_joined %>% 
      mutate(dependent_rule = case_when(
        RELATE == 1 ~ "Householder",
        age < 18 ~ "Under 18",
        SPLOC != 0 & MAR == 1 ~ "Married Spouse", 
        SPLOC != 0 & PINCP %in% c(0:4300) & MAR %in% c(2:5) ~ "Dependent Unmarried Spouse",
        age %in% c(18:23) & SCH %in% c(2:3) ~ "College Student",
        DIS ==  1 & is.na(WKHP) ~ "Person with Disability"
      ))
    
    
    ## Create flags for income and age for diagnostic purposes
    
    data_joined <- data_joined %>% 
      mutate(flag_18 = if_else(age>17,"Over 18","Under 18"),
             flag_inc = if_else(PINCP > 4300,"Income Earned","No Income"))
    
    
    
    ## set college students who are in all dependent households to be their own tax units either as dependents who will be claimed by someone else
    
    data_joined <- data_joined %>% mutate(
      TAXID = if_else(
        DepHH == 1 & dependent_rule == "College Student" & TAXSIZE == 1,as.numeric(paste0(SERIAL,1,FAMUNIT_pad,PERNUM_pad)),TAXID),
      payer_id = if_else(
        DepHH == 1 & dependent_rule == "College Student" & TAXSIZE == 1,"Claimed Dependent",payer_id),
      DSI_eligible =  if_else(
        DepHH == 1 & dependent_rule == "College Student" & TAXSIZE == 1,1,0),
    )
    
    ## If you are still in a non-payer only tax unit then we will manually for householders into payers and married spouses into spouses
    
    data_joined <- data_joined %>%
      mutate(
        payer_id = case_when(
          DepHH == 1 & dependent_rule == "Householder" ~ "Payer",
          DepHH == 1 & dependent_rule == "Married Spouse" ~ "Spouse",
          TRUE ~ payer_id
        )
      )
    
    ## create a flag for Earned Income Credit, Child Tax Credit, and Vision Difficulty
    
    data_joined <- data_joined %>%
      mutate(EIC_eligible = if_else(
        age <= 23 & payer_id == "Non-Payer",1,0),
        CTC_eligible = if_else(
          age <= 17 & payer_id == "Non-Payer",1,0),
        Blind = case_when(DEYE == 2 ~ 0,
                          DEYE == 1 ~ 1))
    

###### Diagnostic Code to Help Identify Where Households of Just Dependents Were ######
    
    data_joined %>%
      group_by(payer_id) %>%
      summarise(n=MAR) %>%
      count(n)
    
    data_joined %>%
      filter(payer_id == "Spouse" & MAR %in% c(2:5)) %>%
      summarise(n=RELATE) %>%
      count(n)
    
    
    
    data_joined %>%
      group_by(TAXID) %>%
      summarise(n = n() - sum(payer_id == "Non-payer")) %>%
      count(n)
    
  data_joined %>%
    group_by(TAXID) %>%
    filter(n() - sum(dependent_flag2) == 0) %>%
    ungroup(TAXID) %>%
    summarise(n=dependent_rule) %>%
    count(n)


    data_joined %>%
    group_by(TAXID) %>%
    filter(n() - sum(dependent_flag2) == 0) %>%
    filter(dependent_rule == "Householder") %>%
    ggplot(aes(x=TAXSIZE)) + geom_histogram()
    
    

    
    data_joined <- data_joined %>%
      mutate(TAXID
    )
    
    
    
    filter(FAMSIZE != 1 & SPLOC == 0) %>%
    group_by(FAMSIZE) %>%
    summarise(n=n(),m=sum(PERWT))
     ggplot(aes(x=SUBFAM)) + geom_histogram() 
    
    
    filter(SPLOC == 0) %>%
    
    filter(FAMSIZE != 1) %>%
    glimpse()
    
    
    ungroup() %>%
    summarise(n= n(), m=sum(PERWT))
             
    
    
    glimpse()
                      
                      
                      ) %>%
    ungroup() %>%
    summarise(n=sum(PERWT))
              
              
    glimpse() %>%
    ggplot(aes(x=age)) + geom_histogram()
  
    
    
##### Create income categories #####
    
  ## add all the elements into a data frame grouped by TAX ID
    
    ## create data frame of just the spouse info to join onto the big data frame
    
    data_spouse <- data_joined %>%
      filter(payer_id == "Spouse") %>%
      select(age_spouse = age,
             blind_spouse = Blind,
             e00200s = WAGP,
             e00900s = SEMP,
             wt_s = PERWT
             )
    
    data_tax <- data_joined %>%
      group_by(TAXID) %>%
    filter(sum(payer_id == "Spouse") <= 1 & ! all(payer_id == "Non-Payer")) %>% ## filter out households with more than one spouse and that are all dependents
      summarise(
             FLPDYR = 2018, ## Calendar year for when the taxes are calculated
             MAR_p = MAR[payer_id == "Payer"], ## marital status of payer
             widow_p = MARHW[payer_id == "Payer"], ## widow in last 12 months status of payer
             wt_p = PERWT[payer_id == "Payer"],
             age_head = age[payer_id == "Payer"],## age of head
             blind_head = Blind[payer_id == "Payer"], ## flag for whether the payer is blind
             e00200p_working = WAGP[payer_id == "Payer"], ## wage earnings of payer
             e00900p_working = SEMP[payer_id == "Payer"], ## self employment earnings of payer
             e00200 = sum(WAGP,na.rm = TRUE), ## wage earnings across the whole Tax Unit
             e00900 = sum(SEMP,na.rm = TRUE), ## self employment earnings across the whole tax unit
             e00300 = sum(INTP,na.rm = TRUE), ## interest, rental, dividends
             e02400 = sum(SSP,na.rm = TRUE), ## Social Security income
             e01500 = sum(RETP,na.rm = TRUE), ## Retirement income
             ssi_ben = sum(SSIP,na.rm = TRUE), ## SSI amount
             tanf_ben = sum(PAP,na.rm = TRUE), ## TANF amount
             f2441 = SPM_ChildCareXpns[payer_id == "Payer"], ## Childcare expenses
             elderly_dependents = sum(age>65 & payer_id == "Non-Payer"), ## number of people over 65
             EIC_working = sum(EIC_eligible), ## count eligible EIC 
             n24 = sum(CTC_eligible), ## number of CTC eligble children
             nu06 = sum(age < 6), ## number of people under 6
             nu13 = sum(age < 13), ## number of people under 13
             nu1820 = sum(age %in% c(18:20)), ## number of people between 18 and 20
             n21 = sum(age >= 21), ## number of people over 21
             wt = sum(PERWT,na.rm = TRUE), ## Summed weight
             XTOT = sum(payer_id == "Non-Payer"), ## total number of exemptions
             snap_ben = SPM_SnapSub[payer_id == "Payer"],# SNAP benefits
             housing_ben = SPM_CapHouseSub[payer_id == "Payer"], ## HCV Benefits
             DSI = max(DSI_eligible), ## indicate whether the tax units are claimed by another household
             unit_size = n() ## size of tax units
             ) %>%
      left_join(data_spouse, by = "TAXID") %>%
      mutate(e00200_working = sum(e00200p_working,e00200s,na.rm = TRUE), ## summed wage earnings
             e00900_working = sum(e00900p_working,e00900s,na.rm = TRUE), ## self employment earnings
             EIC = if_else(EIC_working>3,3,EIC_working), ## max EIC at 3 kids 
             AGI = sum(e00200,e00900,e00300,e02400,e01500,ssi_ben,tanf_ben,na.rm = TRUE),
             AGI_working = sum(e00200_working,e00900_working,e00300,e02400,e01500,ssi_ben,tanf_ben,na.rm = TRUE),
             e00200p = sum(e00200,-e00200s,na.rm = TRUE),
             e00900p = sum(e00900,-e00900s,na.rm = TRUE),
             MARS = case_when( ## filing status [1=single, 2=joint, 3=separate, 4=household-head, 5=widow(er)]
               (MAR_p == 2) & (widow_p == 1) & (EIC >= 1) ~ 5, ## widow(er)
               is.na(age_spouse) & (MAR_p %in% c(2:5)) & (XTOT >= 1) ~ 4, ## household-head 
               is.na(age_spouse) ~ 1, ## single
               !is.na(age_spouse) ~ 2 ## married
                ),
             s006 = sum(wt_p,wt_s,na.rm = TRUE) ## weight of the tax unit (just filers and spouses)
             ) %>%
      rename(RECID = TAXID
               )
    
    data_tax_zeros <- data_tax
    data_tax_zeros[is.na(data_tax_zeros)] <- 0
    
    
     write_csv(data_tax,"data_tax") ## write the CSV for tax units
     
     write_csv(data_tax_zeros,"data_tax_zeros")
     
     write_csv(data_joined,"Joined Data for SPM Tax Analysis")

     data_tax_zeros %>%
       rename(TAXID = RECID) %>%
       rowid_to_column("RECID") %>%
       write_csv("data_tax_nums")
     
###### rejoin data and calculate SPM poverty rate #####
     
     data_reform <- read.csv("~/Americorp VISTA/data_tax_nums-18-#-Brown-#.csv")
     
     data_tax_reform <- left_join(data_tax,data_reform,by = "RECID")
     
     data_joined <- data_joined %>% 
       rename(RECID = TAXID)
    
     data_joined_reform <- right_join(data_joined,data_tax_reform,by = "RECID")
     
     data_joined_reform <- data_joined_reform %>%
       mutate(reform_resources = sum(aftertax_income,SPM_SnapSub,SPM_CapHouseSub,SPM_SchLunch,SPM_EngVal,SPM_WICval,-SPM_CapWkCCXpns,-SPM_MedXpns,na.rm = TRUE),
              reform_pov = if_else(reform_resources<SPM_PovThreshold,1,0))
     
     data_joined_reform %>%
       group_by(reform_pov) %>%
       summarise(n=sum(PERWT)) %>%
       count(n) %>%
       summarise(n/sum(n))
     
     
     glimpse(data_reform)
  
     data_joined$SPM_CapWkCCXpns
     
     
  
  
  