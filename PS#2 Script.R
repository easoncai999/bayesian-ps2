library(ggplot2)  # For plot
library(dplyr)    # For generate a dataframe
library(viridis)  # For coloring the plot

N <- 1e5
sample_sizes <- seq(1000, N, by = 10000)
seeds <- c(123, 456, 789, 111, 222, 333, 555, 777, 888, 999)

### PART 1 ###

# Mean Estimate
df <- expand.grid(sample_size = sample_sizes, seed = seeds) %>%
  rowwise() %>%
  mutate(mean_estimate = {
    set.seed(seed)
    mean(rnorm(sample_size, 0, 1))}) %>%
  ungroup()

df$seed <- as.factor(df$seed)

ggplot(df, aes(x = sample_size, y = mean_estimate, color = seed, group = seed)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "cividis") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Convergence of the Mean Estimate for Different Seeds",
       x = "Number of Runs",
       y = "Estimated Mean") +
  theme_minimal()

summary_mean <- df %>%
  group_by(sample_size) %>%
  summarise(
    mean = mean(mean_estimate),  # Average mean estimate across seeds
    uncertainty = sd(mean_estimate) / sqrt(length(seeds))  # Standard Error (SE) across seeds
  )

# 95th Percentile Estimate
df <- df %>%
  rowwise() %>%
  mutate(
    q95 = {
      set.seed(seed)
      quantile(rnorm(sample_size, 0, 1), 0.95)
    }
  ) %>%
  ungroup()

ggplot(df, aes(x = sample_size, y = q95, color = seed, group = seed)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "cividis") +
  geom_hline(yintercept = qnorm(.95, 0, 1), linetype = "dashed", color = "black") +
  labs(title = "Convergence of the 95th Percentile Estimate for Different Seeds",
       x = "Number of Runs",
       y = "Estimated 95th Percentile") +
  theme_minimal()

summary_95 <- df %>%
  group_by(sample_size) %>%
  summarise(
    mean = mean(q95),  
    uncertainty = sd(q95) / sqrt(length(seeds)) 
  )

### PART 2 ###

# Pi Estimate
df <- df %>%
  rowwise() %>%
  mutate(
    pi_estimate  = {
      set.seed(seed)
      x <- runif(sample_size, 0, 1)
      y <- runif(sample_size, 0, 1)
      inside_pts <- (x^2 + y^2) <= 1
      4 * sum(inside_pts) / sample_size
    }
  ) %>%
  ungroup()

ggplot(df, aes(x = sample_size, y = pi_estimate, color = seed, group = seed)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(option = "cividis") +
  geom_hline(yintercept = pi, linetype = "dashed", color = "black") +
  labs(title = "Convergence of Pi Estimate for Different Seeds",
       x = "Number of Runs",
       y = "Estimated Pi Value") +
  theme_minimal()

summary_pi <- df %>%
  group_by(sample_size) %>%
  summarise(
    mean = mean(pi_estimate),  
    uncertainty = sd(pi_estimate) / sqrt(length(seeds)) 
  )
