

################################################################################################
################################################################################################
#
# This R code shows several analytical steps used in USA TODAY's "Broken Adoptions" project
# Aleszu Bajak, USA TODAY
#
# Datasets required:
#  You will need to agree to terms and download AFCARS datasets for FY2008-FY2020 here:
#  https://www.ndacan.acf.hhs.gov/datasets/datasets-list.cfm
#
# Analysis
#  1) Code to isolate previously adopted children in care
#  2) Code to compare and visualize "everadpt" cohort with overall foster care population
#  3) Code to run Cox regression and visualize hazard curves for post-adoption reentry analysis
#
################################################################################################
################################################################################################


################################################################################################
#  1) Code to isolate previously adopted children in care
################################################################################################

# The AFCARS foster care codebook describes the EVERADPT variable as:
# "Indicates that the child has previously been legally adopted."
# With 1 as Yes, 2 as No and 3 as Unable to determine.
# For more details see:
# https://www.ndacan.acf.hhs.gov/datasets/pdfs_user_guides/afcars-foster-care-file-codebook.pdf

# The analysis below identifies all previously adopted children served by the
# foster care system between 2008 and 2020
# The analysis also removes children who are erroneously flagged as "everadpt"


library(tidyverse)

# Add min and max FY to the df, we'll use these later
afcars_foster_2008_2020 = afcars_foster_2008_2020 %>%
  group_by(StFCID) %>%
  mutate(Max_FY = max(FY),
         Min_FY = min(FY))
glimpse(afcars_foster_2008_2020)

# Count up all 3s in the "EVERADPT" column
afcars_foster_ever3 = afcars_foster_2008_2020 %>%
  filter(EVERADPT == 3) #
afcars_foster_ever3 %>%
  group_by(StFCID) # 314,830 kids

notadopt_list <- afcars_foster_2008_2020 %>%
  filter(!EVERADPT == 1 | is.na(EVERADPT)) %>%
  select(StFCID) %>%
  distinct()
glimpse(notadopt_list) # 3,415,542

# Select only IDs
afcars_foster_2020_ids = afcars_foster_2008_2020 %>%
  dplyr::select(StFCID)  %>%
  ungroup() %>%
  distinct()
glimpse(afcars_foster_2020_ids)

# remove any kid who's ever had a EVERADPT != 1 or EVERADPT == NA
alwaysadopted_list <- afcars_foster_2020_ids %>%
  filter(!StFCID %in% notadopt_list$StFCID) %>%
  distinct()
glimpse(alwaysadopted_list) # 63,679

# Find all kids who weren't adopted their first year in AFCARS
notadopted_firstyear <- afcars_foster_2008_2020 %>%
  filter(FY == Min_FY) %>% # filter for only Min_FY records
  filter(DISREASN != 3) # That aren't adopted that year
glimpse(notadopted_firstyear)

# Find those unique ID numbers
notadopted_firstyear_uniques <- notadopted_firstyear %>%
  select(StFCID) %>%
  ungroup() %>%
  distinct()
glimpse(notadopted_firstyear_uniques) # 3,410,441

# Use those to generate list of kids who were adopted and
# never in foster care before
nopriors_2020 <- alwaysadopted_list %>%
  filter(StFCID %in% notadopted_firstyear_uniques$StFCID)
glimpse(nopriors_2020) # 59,513

# Now find all "return trips" i.e. kids who were in foster care
# and then were adopted out and then returned
adopted_from_foster <- afcars_foster_2008_2020 %>%
  filter(DISREASN == 3) %>%
  filter(InAtEnd == 0) %>%
  filter(Exited == 1) %>%
  filter(DoDFCDt != "") %>%
  filter(TPRDate != "")

# Count them up
adopted_from_foster %>%
  group_by(StFCID) # 690,446

# Then, we use this list to determine last year in care just for kids who were
# adopted out of foster care. We keep only the kids where last year in care is
# greater than the year of adoption.

# This basically removes entries where who only spent one year in foster
# care and had everadpt==1 because we had seen so many nonsensical entries
# whose EVERADPT status flipped between reporting periods so we decided to only
# trust entries with multiple years of consistent EVERADPT.

minadoptyear <- adopted_from_foster %>%
  group_by(StFCID) %>%
  filter(FY == min(FY)) %>%
  rename("Min_Adopted_FY" = "FY") %>%
  select(StFCID, Min_Adopted_FY)

minadoptyear %>%
  group_by(StFCID) # 690,446

afcars_foster_2008_2020_w_Min <- afcars_foster_2008_2020 %>%
  ungroup() %>%
  left_join(minadoptyear, by="StFCID")
glimpse(afcars_foster_2008_2020_w_Min)

failedfosteradopt_list <- afcars_foster_2008_2020_w_Min %>%
  filter(Min_Adopted_FY < Max_FY) %>%
  select(StFCID) %>%
  distinct()
glimpse(failedfosteradopt_list) # 7,129

# Bind the failed foster adoptees and the
# failed adoptees (no priors) and remove duplicates, if any

all_true_everadopts_2008_2020 <- failedfosteradopt_list %>%
  bind_rows(nopriors_2020) %>%
  distinct()
glimpse(all_true_everadopts_2008_2020) # 66,577



################################################################################################
#  2) Code to compare and visualize "everadpt" cohort with rest of foster care population
################################################################################################

library(tidyr)
library(scales)

# To compare the prevalence of demographics, behavioral characteristics and removal reasons
# between previously adopted children and the rest of the foster care population

# First, isolate the earliest AFCARS record for each kid

all_true_everadopts_2008_2020_minFY = afcars_foster_2008_2020 %>%
  filter(StFCID %in% all_true_everadopts_2008_2020$StFCID) %>%
  group_by(StFCID) %>%
  filter(FY == min(FY))
glimpse(all_true_everadopts_2008_2020_minFY)

everadopted_minFY_demos <- all_true_everadopts_2008_2020_minFY %>%
  ungroup() %>%
  dplyr::select(MR, CLINDIS, VISHEAR, EmotDist, OTHERMED, MANREM, CHBEHPRB, PHYDIS,
                CHILDIS, SEX, PHYABUSE, SEXABUSE, NEGLECT, RELINQSH, ABANDMNT, PRTSDIED,
                PRTSJAIL, HOUSING, NOCOPE, AAPARENT, DAPARENT, RaceEthn, DISREASN, CURPLSET) %>%
  dplyr::mutate(Count = n()) %>%
  pivot_longer(
    cols = c(MR, CLINDIS, VISHEAR, EmotDist, OTHERMED, MANREM, CHBEHPRB, PHYDIS,
             CHILDIS, SEX, PHYABUSE, SEXABUSE, NEGLECT, RELINQSH, ABANDMNT, PRTSDIED,
             PRTSJAIL, HOUSING, NOCOPE, AAPARENT, DAPARENT, RaceEthn, DISREASN, CURPLSET),
    names_to = "variable",
    values_to = "number"
  ) %>%
  group_by(variable, Count) %>%
  dplyr::count(number) %>%
  dplyr::mutate(population = "everadopted",
                prop = n/Count)


# Do same for the rest of foster population

afcars_foster_2008_2020_minFY = afcars_foster_2008_2020 %>%
  group_by(StFCID) %>%
  filter(FY == min(FY))

allfoster_minFY_demos <- afcars_foster_2008_2020_minFY %>%
  ungroup() %>%
  # Remove everadpts
  filter(!StFCID %in% all_true_everadopts_2008_2020_minFY$StFCID) %>%
  dplyr::select(MR, CLINDIS, VISHEAR, EmotDist, OTHERMED, MANREM, CHBEHPRB, PHYDIS,
                CHILDIS, SEX, PHYABUSE, SEXABUSE, NEGLECT, RELINQSH, ABANDMNT, PRTSDIED,
                PRTSJAIL, HOUSING, NOCOPE, AAPARENT, DAPARENT, RaceEthn, DISREASN, CURPLSET) %>%
  dplyr::mutate(Count = n()) %>%
  pivot_longer(
    cols = c(MR, CLINDIS, VISHEAR, EmotDist, OTHERMED, MANREM, CHBEHPRB, PHYDIS,
             CHILDIS, SEX, PHYABUSE, SEXABUSE, NEGLECT, RELINQSH, ABANDMNT, PRTSDIED,
             PRTSJAIL, HOUSING, NOCOPE, AAPARENT, DAPARENT, RaceEthn, DISREASN, CURPLSET),
    names_to = "variable",
    values_to = "number"
  ) %>%
  group_by(variable, Count) %>%
  dplyr::count(number) %>%
  dplyr::mutate(population = "allfoster",
                prop = n/Count)

bothpopulations_minFY_demos <- bind_rows(allfoster_minFY_demos, everadopted_minFY_demos)


# Look at behavioral issues in cohorts side-by-side
# with a Cleveland dot plot

bothpopulations_minFY_demos %>%
  filter(!variable %in% c("RaceEthn", "SEX", "DISREASN")) %>%
  filter(number == 1) %>% # diagnosis applies
  ggplot(aes(x=prop, y=reorder(variable, prop))) +
  geom_line(aes(group = variable)) +
  geom_point(aes(color = population)) +
  ylab("") +
  scale_x_continuous("Proportion", labels = percent_format()) +
  theme_minimal()

# Generate table

bothpopulations_minFY_demos %>%
  ungroup() %>%
  filter(variable %in% c("CLINDIS", "EmotDist", "CHBEHPRB", "OTHERMED", "MR", "VISHEAR")) %>%
  filter(number == 1)  %>%
  select(variable, population, prop) %>%
  pivot_wider(names_from = population, values_from = prop) %>%
  mutate(diff=everadopted/allfoster) %>%
  arrange(desc(diff))


################################################################################################
#  3) Code to run Cox regression and visualize hazard curves for post-adoption reentry analysis
################################################################################################

# This reentry analysis is limited to the 16 states with traceable AFCARS IDs, per
# https://www.acf.hhs.gov/sites/default/files/documents/opre/pagitask7designoptionrptfinal_8_17_20508.pdf

# Inspired by 'Predictors of Adoption and Guardianship Dissolution: The Role of Race,
# Age, and Gender Among Children in Foster Care' from 2020 which used survival analysis.
# by Satler and Font https://journals.sagepub.com/doi/abs/10.1177/1077559520952171

# To examine the risk various demographic and behavioral variables have on an adoption failing over time,
# USA TODAY employed a statistical model known as a Cox proportional hazards regression. Using AFCARS, USA TODAY
# identified 60,890 children adopted between 2008 and 2010 in states that researchers note allow children to be
# tracked with a unique identifier pre- and post-adoption: Arizona, Florida, Kansas, Kentucky, Hawaii, Indiana,
# Louisiana, Minnesota, Mississippi, Missouri, Montana, Nebraska, New York, Oregon, Texas and Vermont.
# We then flagged 1,973 children that had returned to the child welfare system through 2020.

# The Cox regression model takes into account the child’s age at adoption, gender, race, an “emotionally disturbed”
# mental health category, and the state where the adoption took place. When children reach 18 years of age, they are
# “censored” out of the model as they’re ineligible to return to foster care. We also do not have data on children
# whose adoptions failed after their parents moved to another state. The hazard ratios calculated
# for age_at_adopt, emotdist=yes and race=black are 1.24, 1.39 and 1.51, respectively. All are statistically significant.

# The risk curves are created using Cox-derived hazard ratios which stay constant over time, an underlying
# assumption of the proportional hazards model. Technically speaking, these curves depict the cumulative risk
# at time t that a child has returned to foster care compared to a child with similar characteristics save for the
# covariate of interest. The cumulative risk is calculated as 1 minus the predicted survival from running a
# Kaplan-Meier survival curve on the Cox model object with a new dataset with reference variables – in this case,
# a female adopted in Texas of varying age, race and “emotionally disturbed” label.

# Thanks to the following for advice: Sarah Font, Andy Barclay, Kristin Turney, Kathryn Ziegler-Graham, Beth Shinn,
# Machelle Wilson, Justin Manjourides, Sarah Sernaker, Michael Dineen, and Jarvis Chen.

library(lubridate)
library(survival)
library(survminer)
library(gtsummary)

# We'll need to select two cohorts,
# Both were adopted out of foster care between 2008 through 2010
# One never re-entered and the other re-entered between 2011 and 2020

Adopted_in_2008_2010 = afcars_foster_2008_2020 %>%
  filter(St %in% c("TX", "KS", "KY", "MO", "OR",
                   "AZ","FL","HI","IN","LA",
                   "MN","MS","MT","NE","NY",
                   "VT")) %>%
  ungroup() %>%
  filter(FY <= 2010) %>% # Select 2008, 2009 and 2010
  filter(DISREASN == 3) %>%
  group_by(StFCID) %>%
  mutate(FY_adopt = ifelse(DISREASN == 3, FY, NA)) # Apply FY of adoption to FY_adopt variable

Adopted_in_2008_2010 %>%
  group_by(StFCID) # 60,890 kids

# Capture all kids who reenter after their FY_adopt
# This can include kids reentering in 2008, 2009 and 2010
Adopted_in_2008_2010_id_and_FYadopt = Adopted_in_2008_2010 %>%
  select(StFCID, FY_adopt)

Reentered_after_2008_10_adoption = afcars_foster_2008_2020 %>%
  ungroup() %>%
  left_join(Adopted_in_2008_2010_id_and_FYadopt, by="StFCID") %>% # merge in FY_adopt
  filter(StFCID %in% Adopted_in_2008_2010$StFCID) %>% # filter just adoptees
  filter(FY > FY_adopt & Entered == 1) %>% # find re-entries
  group_by(StFCID) %>% #
  ungroup() %>%
  dplyr::select(StFCID, FY_adopt) %>% # Isolate ID and FY_adopt
  left_join(afcars_foster_2008_2020, by="StFCID") %>% # Merge in entire records
  mutate(reentered = ifelse(Entered==1 & FY > FY_adopt, 1, 0)) #Apply reentry tag

Reentered_after_2008_10_adoption %>%
  group_by(StFCID)  # 1,973 kids

Stay_adopted_after_2008_10_adoption = Adopted_in_2008_2010 %>%
  ungroup() %>%
  filter(!StFCID %in% Reentered_after_2008_10_adoption$StFCID) %>% # Filter out reentries
  dplyr::select(StFCID) %>%
  left_join(afcars_foster_2008_2020, by="StFCID") %>%
  mutate(reentered = 0)
glimpse(Stay_adopted_after_2008_10_adoption)

Stay_adopted_after_2008_10_adoption %>%
  group_by(StFCID)  #58,917 kids

# Next, we'll combine cohorts

# First, isolate IDs of all kids who reentered so we can tag that cohort later
reentry_ids = Reentered_after_2008_10_adoption %>%
  filter(reentered == 1) %>%
  select(StFCID, reentered) %>%
  distinct() # 1,973 kids

Stay_adopted_after_2008_10_minadoptFY = Stay_adopted_after_2008_10_adoption %>%
  group_by(StFCID) %>%
  # We want to create some new variables for
  # year of entry and year of adoption
  mutate(FY_entry = Min_FY,
         FY_adopt = ifelse(DISREASN == 3, FY, NA),
         FY_adopt = min(FY_adopt, na.rm = TRUE))

Stay_adopted_after_2008_10_minadoptFY %>%
  group_by(StFCID)  # 58,917 kids

Reentered_after_2008_10_minadoptFY = Reentered_after_2008_10_adoption %>%
  group_by(StFCID) %>%
  mutate(FY_entry = Min_FY,
         FY_reentry = ifelse(Entered == 1, FY, NA),
         FY_reentry=max(FY_reentry,na.rm = TRUE),
         FY_adopt = ifelse(DISREASN == 3, FY, NA),
         FY_adopt=min(FY_adopt,na.rm = TRUE)) %>% # For kids this "TX®©¬®¾µþúÿûþü" who were adopted twice, we want to make sure we capture the first adoption
  ungroup() %>%
  select(-reentered) %>% # remove the reentry flag because it was only applied to the child's reentry year.
  left_join(reentry_ids, by="StFCID")  # We want the reentry flag to be on every FY for that child
glimpse(Reentered_after_2008_10_minadoptFY)

Reentered_after_2008_10_minadoptFY %>%
  group_by(StFCID) #  1,973 kids

# Bind and export
two_cohorts_min_adoptFY = bind_rows(Stay_adopted_after_2008_10_minadoptFY,
                                    Reentered_after_2008_10_minadoptFY)

# We want to isolate the behavioral and demographic characteristics of all children
# BEFORE their adoption so we'll filter for the year they were adopted
# If we were to select AFCARS variables from years of reentry we'd have a different picture
# of their health, our earlier exploratory analysis showed.

two_cohorts_min_adoptFY_clean = two_cohorts_min_adoptFY %>%
  group_by(StFCID) %>%
  # Isolate characteristics at the year of adoption
  filter(FY == FY_adopt) %>%
  ungroup() %>%
  # Calculate age at entry, adoption and reentry (if applicable)
  # If not applicable, then calculate age when they'd be ineligible to return to care
  # This is important, because this is how we "censor" those children out
  # Of our observation window
  mutate(DOB = lubridate::ymd(DOB),
         YOB = lubridate::year(DOB),
         age_at_entry = FY_entry-YOB,
         # While AFCARS lists age at adoption, it's not always reliably entered.
         # So we elected to calculate it ourselves.
         age_at_adopt = FY_adopt-YOB,
         age_at_reentry = FY_reentry-YOB,
         year_since_adopt = ifelse(is.na(age_at_reentry), 18-age_at_adopt, age_at_reentry-age_at_adopt),
         SEX=ifelse(SEX==1, "male", "female"),
         race = case_when(
           # Intentionally misspelled so it becomes the reference category. Simpler than releveling.
           RaceEthn == 1 ~ "1white",
           RaceEthn == 2 ~ "black",
           RaceEthn == 7 ~ "latino",
           RaceEthn == 3|4|5|6|99 ~ "other",
         ),
         # We created age groups, too.
         age_group = case_when(
           age_at_adopt <= 2 ~ "0-2",
           age_at_adopt > 2 & age_at_adopt <= 6 ~ "3-6",
           age_at_adopt > 6 & age_at_adopt <= 10 ~ "7-10",
           age_at_adopt > 10 & age_at_adopt <= 13 ~ "11-13",
           age_at_adopt > 13 ~ "14+"
         ),
         # Create state category
         St=substr(StFCID, 1,2)) %>%
  dplyr::select(StFCID, St, SEX:HOUSING, TOTALREM, reentered, race,
                DOB, YOB, age_at_adopt, age_at_reentry, age_group,
                year_since_adopt, FY_entry, FY_adopt, FY_reentry) %>%
  distinct()

glimpse(two_cohorts_min_adoptFY_clean)

# Look at number of reentries tracked per state
two_cohorts_min_adoptFY_clean %>%
  ungroup() %>%
  dplyr::select(St, reentered) %>%
  group_by(St) %>%
  dplyr::count(reentered) %>%
  pivot_wider(names_from = reentered, values_from = n) %>%
  dplyr::rename(adopted = `0`,
                reentered = `1`) %>%
  mutate(adopted = (adopted+reentered),
         reentry_rate = reentered/adopted*100) %>%
  arrange(desc(reentered))

# St    adopted reentered reentry_rate
# 1  TX      14166       660        4.66
# 2  KS       2211       214        9.68
# 3  MO       3083       199        6.45
# 4  KY       2372       167        7.04
# 5  OR       2937       127        4.32
# 6  LA       1808        97        5.37
# 7  IN       4419        95        2.15
# 8  FL      10987        76        0.692
# 9  NY       6977        62        0.889
# 10 MN       2070        54        2.61
# 11 HI        872        53        6.08
# 12 MS        990        45        4.55
# 13 MT        613        36        5.87
# 14 NE       1477        31        2.10
# 15 VT        493        31        6.29
# 16 AZ       5428        28        0.516



###############################
# Final Cox model
###############################

# After consultation with several sources in child welfare and clinical biostats research
# we elected to keep age_at_adopt, EmotDist, SEX, race and St in the model

coxfit_final = coxph(Surv(year_since_adopt, reentered) ~
                       age_at_adopt+EmotDist+SEX+race+St,
                     data = two_cohorts_min_adoptFY_clean)
summary(coxfit_final)

# This is the final model and so we get these exponentiated coefficients
# as our hazard ratios:

# age_at_adopt: 24% higher risk for each year added to the adopted age
# EmotDist: 39% risk if the child has an "EmotDist" diagnosis
# raceblack: Compared to white adopted child, Black child has 51% higher risk of reentry

ggforest(coxfit_final, data = two_cohorts_min_adoptFY_clean)
summary(coxfit_final)

# Fit and export Cox-adjusted K-M curves
# EmotDist

emotdist_df <- with(two_cohorts_min_adoptFY_clean,
                    data.frame(EmotDist = c(0, 1),
                               CLINDIS= c(0, 0),
                               CHBEHPRB = c(0, 0),
                               SEX = c("female", "female"),
                               race = c("1white","1white"),
                               St = c("TX","TX"),
                               age_at_adopt = c(5,5)))

emotdist_df_fit <- survfit(coxfit_final, newdata = emotdist_df, type="kaplan-meier")

ggsurvplot(emotdist_df_fit, data = emotdist_df,
           fun="cumhaz",
           conf.int = F, xlim=c(0,8), ylim=c(0, 0.037),
           legend.labs=c("EmotDist=0", "EmotDist=1"),
           ggtheme = theme_minimal())

# The raw data can be exported as such
emotdist_df_fit_exp = summary(emotdist_df_fit,times=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))
emotdist_df_fit_expdf <- as.data.frame(emotdist_df_fit_exp[c("time", "n.risk", "cumhaz",
                                                             "n.event", "surv")])
emotdist_df_fit_expdf = emotdist_df_fit_expdf %>%
  mutate(rel_risk = cumhaz.2/cumhaz.1,
         cumhaz_new1 = -log(surv.1),
         cumhaz_new2 = -log(surv.2))

# Race

black_df <- with(two_cohorts_min_adoptFY_clean,
                 data.frame(EmotDist = c(0, 0),
                            SEX = c("female", "female"),
                            race = c("1white","black"),
                            St = c("TX","TX"),
                            age_at_adopt = c(5,5)))

black_df_fit <- survfit(coxfit_final, newdata = black_df)

ggsurvplot(black_df_fit,
           data = black_df,
           fun="cumhaz", xlim=c(0,8), ylim=c(0, 0.045),
           conf.int = F, legend.labs=c("White", "Black"),
           ggtheme = theme_minimal())


# Age

age_cox_df <- with(two_cohorts_min_adoptFY_clean,
                   data.frame(EmotDist = c(0, 0),
                              SEX = c("female", "female"),
                              race = c("1white","1white"),
                              St = c("TX","TX"),
                              age_at_adopt = c(1,10)))

age_cox_df_fit <- survfit(coxfit_final, newdata = age_cox_df, type="kaplan-meier")

ggsurvplot(age_cox_df_fit,
           data = age_cox_df,
           fun="cumhaz", xlim=c(0,8), ylim=c(0, 0.11),
           conf.int = F, legend.labs=c("Age 1", "Age 10"),
           ggtheme = theme_minimal())









