# The following vectors already exist:
## brfss.vars is a character vector with the (string) names of analyzed vars
## southeast is a character vector with the (string) names of non-exp. southeast states
# peer.region (defined below) is char vector of peer region state names
peer.region <- c('Arkansas', 'Kentucky', 'Michigan', 'Oregon', 'West Virginia')

# Create empty data.frames to hold sample size results
south.sample.sizes <- data.frame()
cohort.sample.sizes <- data.frame()

# Calculate unweighted sample size for each year
list.of.yrs <- list(RAW.BRFSS.14, RAW.BRFSS.18, RAW.BRFSS.22)

for (YEAR in list.of.yrs) {
  # Get region-specific raw data.frames
  SOUTH.current.yr <- YEAR %>%
    filter(state %in% southeast)
  COHORT.current.yr <- YEAR %>%
    filter(state %in% peer.region)
  
  # Calculate unweighted sample size for each variable
  for (var in brfss.vars) {
    # Calculate & store south unweighted sample size
    SOUTH.current.filtered <- SOUTH.current.yr %>%
      filter(!is.na(!!sym(var))) %>% # because na.rm = TRUE in svyby/svymean code
      group_by(state) %>%
      summarise(n=n())
    
    # Calculate & store cohort unweighted sample size
    COHORT.current.filtered <- COHORT.current.yr %>%
      filter(!is.na(!!sym(var))) %>% # because na.rm = TRUE in svyby/svymean code
      group_by(state) %>%
      summarise(n=n())
    
    # Update sample size DFs
    south.sample.sizes <- rbind(south.sample.sizes, SOUTH.current.filtered$n)
    cohort.sample.sizes <- rbind(cohort.sample.sizes, COHORT.current.filtered$n)
  }
  
  # Add appropriate row names
  names(south.sample.sizes) <- sort(southeast)
  names(cohort.sample.sizes) <- sort(peer.region)
}

# Return results
print(south.sample.sizes)
print(cohort.sample.sizes)
