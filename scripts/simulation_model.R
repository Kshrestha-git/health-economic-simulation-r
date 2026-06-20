# ==========================================
# ADVANCED SIMULATION MODEL (ECONOMIC + MC)
# ==========================================

# install.packages("tidyverse")
library(tidyverse)

set.seed(123)

# -------------------------------
# 1. States
# -------------------------------
states <- c("Low", "Medium", "High")

# -------------------------------
# 2. Transition matrix (baseline)
# -------------------------------
transition_matrix <- matrix(
  c(0.6, 0.3, 0.1,
    0.2, 0.5, 0.3,
    0.1, 0.3, 0.6),
  nrow = 3, byrow = TRUE
)

rownames(transition_matrix) <- states
colnames(transition_matrix) <- states

# -------------------------------
# 3. Intervention matrix
# -------------------------------
intervention_matrix <- transition_matrix

intervention_matrix["Low", ] <- c(0.4, 0.4, 0.2)
intervention_matrix["Medium", ] <- c(0.1, 0.5, 0.4)

# -------------------------------
# 4. Costs and outcomes
# (DEFINE ECONOMIC MODEL)
# -------------------------------
state_costs <- c(
  Low = 100,
  Medium = 200,
  High = 400
)

state_effects <- c(
  Low = 1,
  Medium = 3,
  High = 6
)

# -------------------------------
# 5. Simulation function
# -------------------------------
run_simulation <- function(n_steps, start_state, transition_matrix) {
  
  current_state <- start_state
  states_path <- c(current_state)
  
  for (i in 1:n_steps) {
    
    probs <- transition_matrix[current_state, ]
    
    next_state <- sample(names(probs), 1, prob = probs)
    
    states_path <- c(states_path, next_state)
    
    current_state <- next_state
  }
  
  # Calculate outcomes
  total_cost <- sum(state_costs[states_path])
  total_effect <- sum(state_effects[states_path])
  
  return(list(
    states = states_path,
    cost = total_cost,
    effect = total_effect
  ))
}

# -------------------------------
# 6. Monte Carlo simulation
# -------------------------------
n_sim <- 1000
n_steps <- 50

results <- tibble(
  sim = 1:n_sim,
  cost_baseline = NA,
  effect_baseline = NA,
  cost_intervention = NA,
  effect_intervention = NA
)

for (i in 1:n_sim) {
  
  base <- run_simulation(n_steps, "Low", transition_matrix)
  intv <- run_simulation(n_steps, "Low", intervention_matrix)
  
  results$cost_baseline[i] <- base$cost
  results$effect_baseline[i] <- base$effect
  results$cost_intervention[i] <- intv$cost
  results$effect_intervention[i] <- intv$effect
}

# -------------------------------
# 7. Cost-effectiveness results
# -------------------------------
results <- results %>%
  mutate(
    incremental_cost = cost_intervention - cost_baseline,
    incremental_effect = effect_intervention - effect_baseline,
    ICER = incremental_cost / incremental_effect
  )

# Summary
summary_results <- results %>%
  summarise(
    mean_cost_base = mean(cost_baseline),
    mean_cost_int = mean(cost_intervention),
    mean_effect_base = mean(effect_baseline),
    mean_effect_int = mean(effect_intervention),
    mean_ICER = mean(ICER, na.rm = TRUE)
  )

print(summary_results)

# -------------------------------
# 8. Plot distributions
# -------------------------------

# Cost distribution
ggplot(results, aes(x = cost_baseline)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  geom_histogram(aes(x = cost_intervention),
                 bins = 30, fill = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Cost Distribution: Baseline vs Intervention")

# Effect distribution
ggplot(results, aes(x = effect_baseline)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  geom_histogram(aes(x = effect_intervention),
                 bins = 30, fill = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Effect Distribution")

# Cost-effectiveness plane
ggplot(results, aes(x = incremental_effect, y = incremental_cost)) +
  geom_point(alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Cost-Effectiveness Plane",
    x = "Incremental Effect",
    y = "Incremental Cost"
  )

# -------------------------------
# END
# -------------------------------
