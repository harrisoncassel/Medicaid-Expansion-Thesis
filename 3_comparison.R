##### Prepare 2018 & 2022 BRFSS data #####
# Store FPLs by year for study population filtering
## 2018: https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2018-poverty-guidelines
fpl.18 <- c(12140, 16460, 20780, 25100, 29420, 33740, 38060, 42380)

## 2022: https://www.federalregister.gov/documents/2022/01/21/2022-01166/annual-update-of-the-hhs-poverty-guidelines
fpl.22 <- c(13590, 18310, 23030, 27750, 32470, 37190, 41910, 46630)

## NOTE:
## Will use 2013 Medicaid eligibility limits (from MCAID.LIMITS.14 in 1_peer_region_data.R)
## when identifying members of the study population in order to maintain a consistent study
## population that is unaffected by later changes in these limits (namely, due to the expansion).

# 2018 BRFSS
## Read & transform the base BRFSS dataset
RAW.BRFSS.18 <- foreign::read.xport('data/13_2018_BRFSS.XPT') %>%
  mutate(
    # State FIPS code -> state name
    state = FIPS[X_STATE, 'state'],
    
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
    pct.fpl = ifelse(household.size <= 8, est.income/fpl.18[household.size],
                     est.income/(fpl.18[8] + ((household.size - 8) * 4320))),
    
    too.low.income = ifelse(num.children > 0,
                            pct.fpl <= MCAID.LIMITS.14[state, 'parents.13'],
                            pct.fpl <= MCAID.LIMITS.14[state, 'other.13']),
    too.high.income = ifelse(pct.fpl > 1.38, TRUE, FALSE),
    
    elig.income = ifelse(!too.low.income & !too.high.income, TRUE, FALSE),
    
    ## Respondent age filter (exclude potential CHIP & Medicaid enrollees)
    elig.age = ifelse(X_AGE_G < 6, TRUE, FALSE), # 6 = 65+
    
    ## Estimate Medicaid expansion population membership
    med.exp.pop = elig.income & elig.age,
    
    # Peer region identification features
    ## Flu shot in last 12 mos boolean
    flu = ifelse(FLUSHOT6 == 1, TRUE,
                 ifelse(FLUSHOT6 == 2, FALSE, NA)),
    
    ## Indicator for Medicaid as primary source of health insurance
    medicaid.primary.ins = ifelse(HLTHCVR1 > 8, NA,
                                  ifelse(HLTHCVR1 == 4, TRUE, FALSE)),
    
    ## No coverage
    no.coverage = ifelse(HLTHPLN1 %in% c(7, 9), NA,
                         ifelse(HLTHPLN1 == 2, TRUE, FALSE)),
    
    ## Personal provider indicator
    personal.provider = ifelse(PERSDOC2 %in% c(7, 9), NA,
                               ifelse(PERSDOC2 %in% c(1, 2), TRUE, FALSE)),
    
    ## Foregone care due to prohibitive cost
    foregone.due.to.cost = ifelse(MEDCOST %in% c(7, 9), NA,
                                  ifelse(MEDCOST == 1, TRUE, FALSE)),
    
    ## Clean # days physically/mentally/generally unwell
    physhlth.clean = ifelse(PHYSHLTH == 88, 0,
                            ifelse(PHYSHLTH %in% c(77, 99), NA, PHYSHLTH)),
    menthlth.clean = ifelse(MENTHLTH == 88, 0,
                            ifelse(MENTHLTH %in% c(77, 99), NA, MENTHLTH)),
    poorhlth.clean = ifelse(POORHLTH == 88, 0,
                            ifelse(POORHLTH %in% c(77, 99), NA, POORHLTH)),
    
    ## Converting factor vars from numeric -> factor
    genhlth.helper = ifelse(GENHLTH %in% c(7, 9), NA, GENHLTH),
    genhlth.factor = as.factor(genhlth.helper),
    
    last.checkup.helper = ifelse(CHECKUP1 %in% c(7, 9), NA, CHECKUP1),
    last.checkup.factor = as.factor(last.checkup.helper),
    
    last.dental.helper = ifelse(LASTDEN4 %in% c(7, 9), NA, LASTDEN4),
    last.dental.factor = as.factor(last.dental.helper),
    
    ## HIV test indicator
    hiv.test = ifelse(HIVTST6 > 2, NA,
                      ifelse(HIVTST6 == 1, TRUE, FALSE))
  )

## Same setting as used previously
options(survey.lonely.psu = 'adjust')

BRFSS.18 <- svydesign(
  id = ~X_PSU,
  strata = ~X_STSTR,
  weights = ~X_LLCPWT,
  data = RAW.BRFSS.18,
  nest = TRUE
)

##### 2022 BRFSS #####
# Read & transform the base BRFSS dataset
RAW.BRFSS.22 <- foreign::read.xport('data/12_2022_BRFSS.XPT') %>%
  mutate(
    # State FIPS code -> state name
    state = FIPS[X_STATE, 'state'], # Guam, PR -> NA
    
    # Identify members of the study population
    ## Estimated household income using Sowill Chu's interval midpoint method (pdf p. 8)
    ### https://www.econ.berkeley.edu/sites/default/files/Sowill%20Chu%20Senior%20Thesis.pdf
    ### For income groups w/o lower/upper bounds, selecting given bound (levels 1 & 8)
    est.income = ifelse(INCOME3 == 1, 10000,
                        ifelse(INCOME3 == 2, 12500,
                               ifelse(INCOME3 == 3, 17500,
                                      ifelse(INCOME3 == 4, 22500,
                                             ifelse(INCOME3 == 5, 30000,
                                                    ifelse(INCOME3 == 6, 42500,
                                                           ifelse(INCOME3 == 7, 62500,
                                                                  ifelse(INCOME3 == 8, 87500,
                                                                         ifelse(INCOME3 == 9, 125000,
                                                                                ifelse(INCOME3 == 10, 175000,
                                                                                       ifelse(INCOME3 == 11, 200000, NA))))))))))),
    
    ## Household size = num.adults + num.children
    num.children = ifelse(CHILDREN < 88, CHILDREN,
                          ifelse(CHILDREN == 88, 0, NA)),
    household.size = NUMADULT + num.children,
    
    ## Estimate income eligibility for Medicaid expansion
    pct.fpl = ifelse(household.size <= 8, est.income/fpl.22[household.size],
                     est.income/(fpl.22[8] + ((household.size - 8) * 4720))),
    
    too.low.income = ifelse(num.children > 0,
                            pct.fpl <= MCAID.LIMITS.14[state, 'parents.13'],
                            pct.fpl <= MCAID.LIMITS.14[state, 'other.13']),
    too.high.income = ifelse(pct.fpl > 1.38, TRUE, FALSE),
    
    elig.income = ifelse(!too.low.income & !too.high.income, TRUE, FALSE),
    
    ## Respondent age filter (exclude potential CHIP & Medicaid enrollees)
    elig.age = ifelse(X_AGE_G < 6, TRUE, FALSE), # 6 = 65+
    
    ## Medicaid expansion population membership
    med.exp.pop = elig.income & elig.age,
    
    # Peer region identification features
    ## Flu shot in last 12 mos boolean
    flu = ifelse(FLUSHOT7 == 1, TRUE,
                 ifelse(FLUSHOT7 == 2, FALSE, NA)),
    
    ## Indicator for Medicaid as primary source of health insurance
    medicaid.primary.ins = ifelse(PRIMINSR %in% c(77, 99), NA,
                                  ifelse(PRIMINSR == 5, TRUE, FALSE)),
    
    ## No coverage
    no.coverage = ifelse(X_HLTHPLN == 9, NA,
                         ifelse(X_HLTHPLN == 2, TRUE, FALSE)),
    
    ## Personal provider indicator
    personal.provider = ifelse(PERSDOC3 %in% c(7, 9), NA,
                               ifelse(PERSDOC3 %in% c(1, 2), TRUE, FALSE)),
    
    ## Foregone care due to prohibitive cost
    foregone.due.to.cost = ifelse(MEDCOST1 %in% c(7, 9), NA,
                                  ifelse(MEDCOST1 == 1, TRUE, FALSE)),
    
    ## Clean # days physically/mentally/generally unwell
    physhlth.clean = ifelse(PHYSHLTH == 88, 0,
                            ifelse(PHYSHLTH %in% c(77, 99), NA, PHYSHLTH)),
    menthlth.clean = ifelse(MENTHLTH == 88, 0,
                            ifelse(MENTHLTH %in% c(77, 99), NA, MENTHLTH)),
    poorhlth.clean = ifelse(POORHLTH == 88, 0,
                            ifelse(POORHLTH %in% c(77, 99), NA, POORHLTH)),
    
    ## Cleaning & converting factor vars from numeric -> factor
    genhlth.helper = ifelse(GENHLTH %in% c(7, 9), NA, GENHLTH),
    genhlth.factor = as.factor(genhlth.helper),
    
    last.checkup.helper = ifelse(CHECKUP1 %in% c(7, 9), NA, CHECKUP1),
    last.checkup.factor = as.factor(last.checkup.helper),
    
    last.dental.helper = ifelse(LASTDEN4 %in% c(5, 7, 9), NA, LASTDEN4), # 5 has no meaning and 4 obs. in 2022 survey
    last.dental.factor = as.factor(last.dental.helper),
    
    ## HIV test indicator
    hiv.test = ifelse(HIVTST7 > 2, NA,
                      ifelse(HIVTST7 == 1, TRUE, FALSE))
  )

# Same setting used previously
options(survey.lonely.psu = 'adjust')

BRFSS.22 <- svydesign(
  id = ~X_PSU,
  strata = ~X_STSTR,
  weights = ~X_LLCPWT,
  data = RAW.BRFSS.22,
  nest = TRUE
)

##### Create Study-Specific Datasets #####
SOUTH.14 <- subset(BRFSS.14.ELIG, state %in% southeast)
SOUTH.18 <- subset(BRFSS.18, med.exp.pop == TRUE &
                     state %in% southeast)
SOUTH.22 <- subset(BRFSS.22, med.exp.pop == TRUE &
                     state %in% southeast)

COHORT.14 <- subset(BRFSS.14.ELIG, state %in% c('Arkansas', 'Kentucky', 'Michigan', 'Oregon', 'West Virginia'))
COHORT.18 <- subset(BRFSS.18, med.exp.pop == TRUE &
                      state %in% c('Arkansas', 'Kentucky', 'Michigan', 'Oregon', 'West Virginia'))
COHORT.22 <- subset(BRFSS.22, med.exp.pop == TRUE &
                      state %in% c('Arkansas', 'Kentucky', 'Michigan', 'Oregon', 'West Virginia'))

##### Summary Statistics of Interest #####
# Function to compute each statistic individually to avoid na.rm=TRUE from
# eliminating a bunch of rows with partially-incomplete data

# FUNCTION: calculate_stats_to_list
## Function to compute each statistic individually to avoid na.rm=TRUE from
## eliminating a bunch of rows with partially-incomplete data
### array.of.var.names: the vector of variable *names* (e.g., 'flu', 'med.exp.pop') to calculate statistics for
### svy.design: the svydesign object to use
### na.rm.choice=TRUE: the na.rm option for the svymean function
## Returns a list of statistics in the order of array.of.var.names
calculate_stats_to_list <- function(array.of.var.names, svy.design, na.rm.choice=TRUE) {
  results.list <- list() # Holder of computed stats
  
  for (var in array.of.var.names) {
    var.text <- paste0('~', var) # Syntax required by svymean syntax
    
    # Compute stats & append to results.list
    stats <- svymean(eval(parse(text=var.text)), design=svy.design, na.rm=na.rm.choice)
    results.list <- append(results.list, list(stats))
    
    # Print status update -- can be a slow process
    print(paste(var, 'complete'))
  }
  return(results.list)
}

# Create vector of vars of interest and empty list to hold each region/year's results
brfss.vars <- c('flu', 'foregone.due.to.cost', 'genhlth.factor', 'hiv.test',
                'last.checkup.factor', 'last.dental.factor', 'medicaid.primary.ins',
                'menthlth.clean', 'no.coverage', 'personal.provider', 'physhlth.clean',
                'poorhlth.clean')
results <- list()

# Iteratively compute stats of interest for each region/year using the function above
for (svy.design in list(SOUTH.14, COHORT.14, SOUTH.18, COHORT.18, SOUTH.22, COHORT.22)) {
  # Compute stats using function defined above
  stats <- calculate_stats_to_list(brfss.vars, svy.design)
  
  # Save results to list of lists
  results <- append(results, stats)
}

# ^ the results list was manually transformed into .csv files for final analysis & plotting
