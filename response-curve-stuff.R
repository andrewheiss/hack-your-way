library(tidyverse)
library(broom)
library(furrr)

plan(multisession, workers = 4)

source("explore-data.R")

full_data <- read_csv("data/538_p_hacking.csv")

pol_terms <- c("prez", "gov", "sen", "rep")
pol_combinations <- map(
  1:length(pol_terms),
  \(x) combn(pol_terms, x, simplify = FALSE)
) |>
  unlist(recursive = FALSE)

econ_terms <- c("unemp", "inflation", "gdp", "stocks")
econ_combinations <- map(
  1:length(econ_terms),
  \(x) combn(econ_terms, x, simplify = FALSE)
) |>
  unlist(recursive = FALSE)

all_models <- expand_grid(
  party = c("dem", "gop"),
  pol_terms = pol_combinations,
  econ_terms = econ_combinations,
  exclude_recessions = c(TRUE, FALSE),
  weighted = c(TRUE, FALSE)
) |> 
  mutate(id = row_number()) |> 
  nest(.key = "params", .by = id) |>
  mutate(
    data = future_map(
      params,
      \(x)
        data_to_use(
          full_data,
          x$party[[1]],
          x$pol_terms[[1]],
          x$econ_terms[[1]],
          x$exclude_recessions[[1]],
          x$weighted[[1]]
        )
    )
  ) |>
  mutate(
    model = map(data, \(x) lm(econ_score ~ pol_score, data = x)),
    coefs = map(model, \(x) tidy(x, conf.int = TRUE))
  )

pol_coefs <- all_models |>
  unnest(coefs) |>
  filter(term == "pol_score") |>
  unnest(params) |>
  arrange(estimate)

ggplot(pol_coefs, aes(x = 1:nrow(pol_coefs), y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Model specification", y = "Coefficient for `pol_score`") +
  theme_minimal()

ggplot(pol_coefs, aes(x = p.value)) +
  geom_histogram(binwidth = 0.05, boundary = 0, color = "white") +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "red") +
  labs(x = "p-value", y = "Count") +
  theme_minimal()
