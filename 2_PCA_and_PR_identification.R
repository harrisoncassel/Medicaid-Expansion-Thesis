##### PCA computation & # of PCs selection #####
# PCA code based on pp. 401-404 of Introduction to Statistical Learning in R (1st ed.).
# Freely available from: https://www.statlearning.com/
pca <- prcomp(PEER.ID.FINAL, scale=TRUE) # center=TRUE by default

# Plotting -- choosing 4 PCs (82% of variance)
pca.var <- pca$sdev^2
pct.var.explained <- pca.var / sum(pca.var)

# Scree plot
plot(pct.var.explained, main='Scree Plot', ylab='Proportion of Variance Explained by each PC')
abline(h=.1, col='darkred')

# Cumulative variance explained plot
plot(cumsum(pct.var.explained), main='Cumulative Variance Explained',
     ylab='Cumulative Proportion of Variance Explained', xlab='Number of Principal Components')
abline(h=.8, col='darkred')

##### Projecting data & preparing for analysis #####
# Projecting data & retaining first 4 PCs
PC.DATA <- data.frame(predict(pca)[, 1:4]) %>% # Projection & retention
  mutate(
    region = row.names(.)
  )

# Split into candidate & southeast data groups
PC.CANDIDATES <- PC.DATA %>%
  filter(region != 'SOUTHEAST') %>%
  select(-'region')
PC.SOUTHEAST <- PC.DATA %>%
  filter(region == 'SOUTHEAST') %>%
  select(-'region')

##### Identifying the peer region #####
# Compute distances
distances <- c()

for (i in 1:nrow(PC.CANDIDATES)) {
  # Get projected vectors of interest
  SE.projection <- PC.SOUTHEAST[1, ] # transform data.frame to vector
  CANDIDATE.projection <- PC.CANDIDATES[i, ] # get this iteration's candidate region data
  
  # Format vectors as a matrix to satisfy dist() function input requirements
  DIST.INPUT.MATRIX <- matrix(data=c(SE.projection, CANDIDATE.projection), nrow=2, byrow=TRUE)
  
  # Calculate & save distance
  iter.dist <- dist(DIST.INPUT.MATRIX)
  distances <- c(distances, iter.dist)
}

# Analyzing distance results & filtering out all but the closest candidates
summary(distances)

top.regions <- row.names(PC.CANDIDATES[which(distances < 1.25), ])
top.distances <- distances[which(distances < 1.25)]
TOP <- data.frame(top.regions, top.distances); TOP[order(top.distances), ]
