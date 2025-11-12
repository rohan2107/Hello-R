library(ggplot2)
library(dplyr)

## Figure: average distance between points
avg_nn_distance <- function(d, n = 1000) {
  X <- matrix(runif(n * d), nrow = n, ncol = d)
  dist_mat <- as.matrix(dist(X))
  diag(dist_mat) <- Inf   # avoid self-distance = 0
  
  # nearest neighbour distance per point
  nn_dist <- apply(dist_mat, 1, min)
  
  # mean nearest neighbour distance
  return(mean(nn_dist))
}

# Vector of dimensions to test
dims <- 2^(1:12)

# Compute results
set.seed(123)
results <- data.frame(
  dimension = dims,
  mean_nn_dist = sapply(dims, avg_nn_distance)
)

# Plot
ggplot(results, aes(x = dimension, y = mean_nn_dist)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(size = 2, color = "firebrick") +
  scale_x_log10(breaks = dims) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Average Nearest-Neighbour Distance vs Dimension",
    subtitle = "1000 points uniformly distributed in [0,1]^d",
    x = "Dimension (log-spaced)",
    y = "Mean nearest-neighbour distance"
  )


#### ====================================
# Figure: data at different scales

n <- 10000
x <- seq(0, 10, length.out = n)
f_true <- sin(x) + 0.5 * cos(2 * x)

# Observed data
y <- f_true + rnorm(n, mean = 0, sd = 0.05)
df <- data.frame(x, y, f_true)

# Global view
ggplot(df, aes(x, y)) +
  geom_point(color = "gray60", alpha = 0.5, size = 1) +
  geom_line(aes(y = f_true), color = "steelblue", linewidth = 1.2) +
  #geom_smooth(span = 0.2, se = FALSE, color = "darkred", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Global view",
    x = "x",
    y = "y (signal + noise)"
  )

# Local view
ggplot(df %>% filter(x > 1.5 & x < 2), aes(x, y)) +
  geom_point(color = "gray40", alpha = 0.7, size = 1.2) +
  geom_line(aes(y = f_true), color = "steelblue", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Local view",
    x = "x",
    y = "y"
  )

# ====================

## Examples used in EDA section

## NOTE: this dataset is slightly different from the one used in the lecture.
## The code below works, but may result in different data summaries.
X <- readRDS("ex_data_v2.rds")

# Fix the point/comma standards
X$NominalVoltage <- gsub(",", ".", X$NominalVoltage, fixed = TRUE) %>%
  as.numeric()
X$PowerMVA <- gsub(".", "", X$PowerMVA, fixed = TRUE)
X$PowerMVA <- gsub(",", ".", X$PowerMVA, fixed = TRUE) %>%
  as.numeric()

# Remove nonascii characters
remove_non_ascii <- function(x) {
  iconv(x, "UTF-8", "ASCII", sub = "")
}
X <- X %>%
  mutate(across(where(is.character), remove_non_ascii))

# Make VoltageReg binary 
X$VoltageReg <- (X$VoltageReg == "Sim")

ggplot(X, aes(x = PowerMVA, y = 1:nrow(X))) + 
  geom_point() + 
  scale_x_log10() + 
  theme_minimal() + 
  ylab("Observation number")

# Correct powerMVA inconsistencies
X <- X %>%
  mutate(PowerMVA = ifelse(PowerMVA > 1000, PowerMVA/1000, PowerMVA))

ggplot(X, aes(x = PowerMVA)) + 
  geom_histogram(bins = 200) + 
  theme_minimal()

ggplot(X, aes(x = Phases)) + 
  geom_histogram(stat = "count") + 
  theme_minimal()

ggplot(X, aes(x = Function)) + 
  geom_histogram(stat = "count") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

X %>%
  mutate(across(where(is.character), as.factor)) %>%
  summary()


## Correlation heatmap
library(DataExplorer)
X %>%
  filter(complete.cases(X)) %>%
  dplyr::select(-Status) %>%
  mutate(VoltageReg = as.numeric(VoltageReg),
         Phases = ifelse(Phases == "Monofsico", 1, 3)) %>%
  plot_correlation(ggtheme = theme_light())


which(duplicated(X))
X <- X %>% filter(!duplicated(X))
