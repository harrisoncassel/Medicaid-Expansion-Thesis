# The following vectors already exists:
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
    filter(state %in% southeast & med.exp.pop == TRUE) %>%
    group_by(state) %>%
    summarise(n=n())
  COHORT.current.yr <- YEAR %>%
    filter(state %in% peer.region & med.exp.pop == TRUE) %>%
    group_by(state) %>%
    summarise(n=n())
    
  # Update sample size DFs
  south.sample.sizes <- rbind(south.sample.sizes, SOUTH.current.yr$n)
  cohort.sample.sizes <- rbind(cohort.sample.sizes, COHORT.current.yr$n)
  
  # Add appropriate row names
  names(south.sample.sizes) <- sort(southeast)
  names(cohort.sample.sizes) <- sort(peer.region)
}

# Return results
print(south.sample.sizes)
print(cohort.sample.sizes)
