source('0_packages.R') # Load necessary packages

##### Load non-BRFSS datasets #####
# Data to be used for peer region identification
ACS.14.S0101 <- read.csv('data/2_2014_ACS_S0101.csv')
CENSUS.POP.CHANGE <- read.csv('data/3_Census_Pop_Changes.csv')
ACS.14.DP04 <- read.csv('data/4_2014_ACS_DP04.csv')
BEA.REAL.PC.PI <- read.csv('data/5_2014_BEA_Real_PC_PI.csv')
FOOD.INSEC.14 <- read.csv('data/6_USDA_Food_Insecurity.csv')
MCAID.PER.ENROLLEE.SPEND.13 <- read.csv('data/7_Medicaid_per_enrollee_spend_residence_2013.csv')
HEART.DISEASE.MORT.14 <- read.csv('data/8_heart_disease.csv')
CANCER.MORT.14 <- read.csv('data/9_cancer_mortality.csv')
HIV.PREVALENCE <- read.csv('data/10_hiv_prevalence.csv')

# Merge non-BRFSS data by state name
NON.BRFSS <- ACS.14.S0101 %>%
  left_join(CENSUS.POP.CHANGE, by='state') %>%
  left_join(ACS.14.DP04, by='state') %>%
  left_join(BEA.REAL.PC.PI, by='state') %>%
  left_join(FOOD.INSEC.14, by='state') %>%
  left_join(MCAID.PER.ENROLLEE.SPEND.13, by='state') %>%
  left_join(HEART.DISEASE.MORT.14, by='state') %>%
  left_join(CANCER.MORT.14, by='state') %>%
  left_join(HIV.PREVALENCE, by='state') %>%
  filter(state != 'District of Columbia')

# FIPS codes & states to assist BRFSS manipulation
FIPS <- read.csv('data/fips.csv')

# FPL data for contiguous US (not AK/HI, which have different values)
## 2014: https://aspe.hhs.gov/2014-poverty-guidelines
fpl.14 <- c(11670, 15730, 19790, 23850, 27910, 31970, 36030, 40090)

## 2022: https://www.federalregister.gov/documents/2022/01/21/2022-01166/annual-update-of-the-hhs-poverty-guidelines
fpl.22 <- c(13590, 18310, 23030, 27750, 32470, 37190, 41910, 46630)

# 2014 Medicaid eligibility limits -- will use to ID study population in each state (>'14 limit & <138% FPL)
## Parent limits from: https://www.kff.org/medicaid/state-indicator/medicaid-income-eligibility-limits-for-parents/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
## Other adult limits from: https://www.kff.org/medicaid/state-indicator/medicaid-income-eligibility-limits-for-other-non-disabled-adults/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
## !!!: using 2013 limits because Jan. 2014 limits were set to 138% for expansion states
MCAID.LIMITS.14 <- read.csv('data/11_2013_Medicaid_Income_Limits.csv')
row.names(MCAID.LIMITS.14) <- MCAID.LIMITS.14$state

##### 2014 BRFSS #####
# Read & transform the base BRFSS dataset
RAW.BRFSS.14 <- foreign::read.xport('data/1_2014_BRFSS.XPT') %>%
  mutate(
    # State FIPS code -> state name
    state = FIPS[X_STATE, 'state'], # Note: Guam, Puerto Rico -> NA
    
    # Identify members of the study population
    ## Estimated household income using Sowill Chu's interval midpoint method (pdf p. 8)
    ### https://www.econ.berkeley.edu/sites/default/files/Sowill%20Chu%20Senior%20Thesis.pdf
    ### For income groups w/o lower/upper bounds, selecting given bound (levels 1 & 8)
    est.income = ifelse(INCOME2 == 1, 10000,
                        ifelse(INCOME2 == 2, 12500,
                               ifelse(INCOME2 == 3, 17500,
                                      ifelse(INCOME2 == 4, 22500,
                                             ifelse(INCOME2 == 5, 30000,
                                                    ifelse(INCOME2 == 6, 42500,
                                                           ifelse(INCOME2 == 7, 62500,
                                                                  ifelse(INCOME2 == 8, 75000, NA)))))))),
    
    ## Household size = num.adults + num.children
    num.children = ifelse(CHILDREN < 88, CHILDREN,
                          ifelse(CHILDREN == 88, 0, NA)),
    household.size = NUMADULT + num.children,
    
    ## Estimate income eligibility for Medicaid expansion
    pct.fpl = ifelse(household.size <= 8, est.income/fpl.14[household.size],
                     est.income/(fpl.14[8] + ((household.size - 8) * 4060))),
    
    too.low.income = ifelse(num.children > 0,
                            pct.fpl <= MCAID.LIMITS.14[state, 'parents.13'],
                            pct.fpl <= MCAID.LIMITS.14[state, 'other.13']),
    too.high.income = ifelse(pct.fpl > 1.38, TRUE, FALSE),
    
    elig.income = ifelse(!too.low.income & !too.high.income, TRUE, FALSE),
    
    ## Respondent age filter (exclude potential CHIP & Medicaid enrollees)
    elig.age = ifelse(X_AGE_G < 6, TRUE, FALSE), # 6 = 65+
    
    ## Estimate Medicaid expansion population membership
    med.exp.pop = elig.income & elig.age,
    
    # Calculate cohort identification & comparison vars
    ## Flu shot in last 12 mos boolean
    flu = ifelse(FLUSHOT6 == 1, TRUE,
                 ifelse(FLUSHOT6 == 2, FALSE, NA)),
    
    ## Tobacco users - 2014 had no e-cig question, which could result in 
    ## an underreporting of tobacco use (besides normal self-report issues)
    cig.smoker = ifelse(SMOKDAY2 <= 2, TRUE,
                        ifelse(SMOKDAY2 == 3, FALSE, NA)),
    smokeless.user = ifelse(USENOW3 <= 2, TRUE,
                            ifelse(USENOW3 == 3, FALSE, NA)),
    tobacco.user = cig.smoker | smokeless.user, # TRUE | NA -> TRUE, which is desirable
    
    ## High school education or less
    hs.or.less = ifelse(EDUCA == 9, NA,
                        ifelse(EDUCA <= 4, TRUE, FALSE)),
    
    ## Urban residence (in MSA city or in county including MSA city)
    urban = ifelse(MSCODE <= 2, TRUE, FALSE),
    
    ## Unemployed or unable to work
    unemployed.unable = ifelse(EMPLOY1 == 9, NA,
                                ifelse(EMPLOY1 %in% c(3, 4, 8), TRUE, FALSE)),
    
    ## Obese
    obese = ifelse(X_BMI5CAT == 4, TRUE, FALSE),
    
    ## No health care coverage
    no.coverage = ifelse(HLTHPLN1 %in% c(7, 9), NA,
                         ifelse(HLTHPLN1 == 2, TRUE, FALSE)),
    
    ## Indicator for medicaid as primary source of insurance
    medicaid.primary.ins = ifelse(HLTHCVR1 > 8, NA,
                                  ifelse(HLTHCVR1 == 4, TRUE, FALSE)),
    
    ## Do you have a personal doctor/provider?
    personal.provider = ifelse(PERSDOC2 %in% c(7,9), NA,
                               ifelse(PERSDOC2 %in% c(1, 2), TRUE, FALSE)),
    
    ## Foregoing care due to cost
    foregone.due.to.cost = ifelse(MEDCOST %in% c(7, 9), NA,
                                  ifelse(MEDCOST == 1, TRUE, FALSE)),
    
    ## Converting "Don't know"/"Refused" to NAs for PHYSHLTH/MENTHLTH/POORHLTH
    physhlth.clean = ifelse(PHYSHLTH == 88, 0,
                            ifelse(PHYSHLTH %in% c(77, 99), NA, PHYSHLTH)),
    menthlth.clean = ifelse(MENTHLTH == 88, 0,
                            ifelse(MENTHLTH %in% c(77, 99), NA, MENTHLTH)),
    poorhlth.clean = ifelse(POORHLTH == 88, 0,
                            ifelse(POORHLTH %in% c(77, 99), NA, POORHLTH)),
    
    ## HIV test ever
    hiv.test = ifelse(HIVTST6 == 1, TRUE,
                      ifelse(HIVTST6 == 2, FALSE, NA)),
    
    ## Set factor vars to factor types (instead of int/numeric)
    genhlth.helper = ifelse(GENHLTH %in% c(7, 9), NA, GENHLTH),
    genhlth.factor = as.factor(genhlth.helper),
    
    last.checkup.helper = ifelse(CHECKUP1 %in% c(7, 9), NA, CHECKUP1),
    last.checkup.factor = as.factor(last.checkup.helper),
    
    last.dental.helper = ifelse(LASTDEN3 %in% c(7, 9), NA, LASTDEN3),
    last.dental.factor = as.factor(last.dental.helper)
  )

# Create survey::svydesign object
## CDC recommends setting the following lonely PSU option, which allows one item per stratum
options(survey.lonely.psu = 'adjust')

BRFSS.14 <- svydesign(
  id = ~X_PSU,
  strata = ~X_STSTR,
  weights = ~X_LLCPWT,
  data = RAW.BRFSS.14,
  nest = TRUE
)

##### Create PCA-specific dataset #####
# Define state groups
southeast <- c('Alabama', 'Georgia', 'Mississippi', 'South Carolina', 'Tennessee')
potential.peers <- c('Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut',
                     'Illinois', 'Iowa', 'Kentucky', 'Maryland', 'Massachusetts',
                     'Michigan', 'Minnesota', 'Nevada', 'New Hampshire', 'New Mexico',
                     'New Jersey', 'New York', 'North Dakota', 'Ohio', 'Oregon',
                     'Rhode Island', 'Washington', 'West Virginia')
study.states <- c(southeast, potential.peers)

# Compute BRFSS eligible population-specific stats by state
BRFSS.14.ELIG <- subset(BRFSS.14, med.exp.pop == TRUE & state %in% study.states)
BRFSS.ELIG.STATS <- svyby(~hs.or.less + urban + unemployed.unable + obese +
                            tobacco.user + flu,
                          by=~state, FUN=svymean, design=BRFSS.14.ELIG, na.rm=TRUE) %>%
  select(c('hs.or.lessTRUE', 'urbanTRUE', 'unemployed.unableTRUE', 'obeseTRUE',
           'tobacco.userTRUE', 'fluTRUE')) %>%
  mutate(
    state = row.names(.) # svyby makes states rownames (nice but need state column)
  )

# Compute % of population that is med.exp.pop by state
MED.EXP.POP.PROP <- svyby(~med.exp.pop, by=~state, FUN=svymean,
                          design=BRFSS.14, na.rm=TRUE) %>%
  select('med.exp.popTRUE') %>%
  mutate(
    state = row.names(.) # need state column (svyby gives as row.names)
  )

# Join BRFSS data with non-BRFSS data by state
PEER.ID.STATS <- as.data.frame(BRFSS.ELIG.STATS) %>%
  left_join(MED.EXP.POP.PROP, by='state') %>%
  left_join(NON.BRFSS) %>%
  filter(state %in% study.states) # NON.BRFSS was not filtered by state when created

row.names(PEER.ID.STATS) <- PEER.ID.STATS$state

# Create list of all possible candidate state combos (possible PRs)
possible.PRs <- combn(potential.peers, 5, FUN=paste0, collapse='-', simplify=FALSE)

# Fill an empty data.frame with weighted stats of possible peer regions
PEER.ID.WEIGHTED <- data.frame()

for (i in possible.PRs) {
  iter.states <- strsplit(i, split='-')[[1]] # vector of 5 state names
  
  RELEVANT.STATS <- PEER.ID.STATS %>%
    filter(state %in% iter.states) # select only states of interest for this for-loop iteration
  
  # Compute population-based weights
  total.pop <- sum(RELEVANT.STATS$population.2014)
  weights <- RELEVANT.STATS$population.2014 / total.pop
  
  # Numerical columns with data of interest only
  ITER.DROP.COLS <- RELEVANT.STATS %>%
    select(-c('population.2014', 'state'))
  
  # colSum across weighted rows & add i, the region names
  ITER.FINAL <- c(colSums(ITER.DROP.COLS * weights), i)
  
  # Update final data.frame
  PEER.ID.WEIGHTED <- rbind(PEER.ID.WEIGHTED, ITER.FINAL)
}

# Add SOUTHEAST region to make final PCA dataset (same code as in the loop above, basically)
SE <- PEER.ID.STATS %>% filter(state %in% southeast)
total.pop <- sum(SE$population.2014)
weights <- SE$population.2014 / total.pop
SE.DROP.COLS <- SE %>%
  select(-c('population.2014', 'state'))
SE.2 <- c(colSums(SE.DROP.COLS * weights), 'SOUTHEAST')
PEER.ID.WEIGHTED <- rbind(PEER.ID.WEIGHTED, SE.2)

# Adjust data.frame names & drop non-numeric columns (needed to perform PCA)
names(PEER.ID.WEIGHTED) <- c(names(PEER.ID.STATS)[-c(7, 9)], 'region')
row.names(PEER.ID.WEIGHTED) <- PEER.ID.WEIGHTED$region
PEER.ID.FINAL <- PEER.ID.WEIGHTED %>%
  select(-'region') %>%
  mutate_all(function(x) as.numeric(x))
