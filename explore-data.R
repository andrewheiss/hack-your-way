library(tidyverse)

#' Choose data for p-hacking
#'
#' Create a data frame with political and economic scores to use in plotting 
#' and regression.
#' 
#' This recreates the now-dead "Hack Your Way To Scientific Glory" visualization 
#' from FiveThirtyEight: https://web.archive.org/web/20250130080601/https://projects.fivethirtyeight.com/p-hacking/
#'
#' @param data a data frame containing the cleaned FiveThirtyEight data
#' @param party a character string specifying the political party. Must be one 
#'   of `"dem"` (default) or `"rep"`.
#' @param pol_terms a character vector specifying the political terms to include. 
#'   Must be one or more of `"prez"` (default), `"gov"`, `"sen"`, `"rep"`.
#' @param econ_terms a character vector specifying the economic terms. Must be 
#'   one or more of `"unemp"`, `"inflation"`, `"gdp"`,` "stocks"`. 
#'   Default is `c("unemp", "stocks")`.
#' @param exclude_recessions logical. Should recessions be excluded?
#' @param weighted logical. Should more powerful positions count more? 
#'   President counts are multiplied by 100; governors are multiplied by 10; 
#'   senators are multiplied by 5; representatives are multiplied by 1
#'
#' @return A data frame with filtered and processed data, with `pol_score` for 
#'   the composite political score and `econ_score` for the composite economic score
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(readr)
#' full_data <- readr::read_csv("538_p_hacking.csv")
#' 
#' smaller_data <- data_to_use(
#'   full_data, 
#'   party = "rep", 
#'   pol_terms = c("prez", "sen"), 
#'   econ_terms = c("unemp", "gdp")
#' )
#' 
#' ggplot(smaller_data, aes(x = pol_score, y = econ_score)) +
#'   geom_point(alpha = 0.25) +
#'   geom_smooth(method = "lm", se = TRUE, formula = y ~ x)
#' 
#' lm(econ_score ~ pol_score, data = smaller_data) |> summary()
#' }
data_to_use <- function(
  data,
  party = "dem",
  pol_terms = c("prez"),
  econ_terms = c("unemp", "stocks"),
  exclude_recessions = FALSE,
  weighted = FALSE
) {
  stopifnot(
    "Data must be provided" = !missing(data),
    "Party must be 'dem' or 'gop'" = party %in% c("dem", "gop"),
    "pol_terms must be one or more of 'prez', 'gov', 'sen', 'rep'" = all(
      pol_terms %in% c("prez", "gov", "sen", "rep")
    ),
    "econ_terms must be one or more of 'unemp', 'inflation', 'gdp', 'stocks'" = all(
      econ_terms %in% c("unemp", "inflation", "gdp", "stocks")
    )
  )

  data |>
    filter(if (exclude_recessions) !recession else TRUE) |>
    filter(if_all(all_of(econ_terms), \(x) !is.na(x))) |>
    mutate(
      pol_score = if (weighted) {
        rowSums(across(
          all_of(paste0(pol_terms, "_", party)),
          \(x)
            x *
              case_when(
                str_detect(cur_column(), "prez") ~ 100,
                str_detect(cur_column(), "gov") ~ 10,
                str_detect(cur_column(), "sen") ~ 5,
                str_detect(cur_column(), "rep") ~ 1,
                TRUE ~ 1
              )
        ))
      } else {
        rowSums(across(all_of(paste0(pol_terms, "_", party))))
      }
    ) |>
    mutate(
      econ_score = rowSums(across(all_of(econ_terms)))
    )
}

# full_data <- read_csv("data/538_p_hacking.csv")

# # Playground for checking if results match 538's stuff
# blah <- data_to_use(
#   full_data, 
#   party = "dem", 
#   pol_terms = c("prez", "gov"), 
#   econ_terms = c("gdp", "inflation", "stocks"),
#   exclude_recessions = TRUE
# )

# ggplot(blah, aes(x = pol_score, y = econ_score)) +
#   geom_point(alpha = 0.25) +
#   geom_smooth(method = "lm", se = TRUE, formula = y ~ x)

# lm(econ_score ~ pol_score, data = blah) |> 
#   parameters::model_parameters(verbose = FALSE, keep = "pol_score")
