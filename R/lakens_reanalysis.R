library(tidyverse)
library(ggbeeswarm)
library(boot)
library(metafor)

theme_set(theme_light())


# Read and process data ---------------------------------------------------
# Lakens data from https://github.com/Lakens/statistical_inferences/blob/master/15-researchintegrity.qmd

john <- c(1436, 45.8, 63.4, 27.7, NA, 55.9, 38.2, NA, NA, 27, 22, 3, 0.6)
fiedler <- c(1138, 42, 34, 24, NA, 33, 40, NA, NA, 47, 22, 3, 3) #retrieved using get data graph digitizer as data are not shared beyond the graph
agnoli <- c(277, 40.1, 47.9, 16.4, NA, 53.2, 39.7, NA, NA, 37.4, 22.2, 3.1, 2.3)
motyl <- c(1166, 48, 78, 45, NA, 66, 58, NA, NA, 58, 33, 16, 2)
rabelo <- c(232, 54.7, 22.4, 34.5, NA, 22.4, 19.8, NA, NA, 9.1, 17.7, 4.3, 0.86)
fraser_eco <- c(494, 0, 64.1, NA, NA, 36.9, 24, NA, NA, 48.5, 27.3, NA, 4.5)
fraser_evo <- c(313, 0, 63.7, NA, NA, 50.7, 23.9, NA, NA, 54.2, 17.5, NA, 2.0)
makel <- c(1488, 61.69, NA, NA, 67.28, 28.61, 25.48, 41.64, 49.57, 45.80, 28.66, NA, 9.69)
bakker <- c(872, 60, NA, NA, 64, 23.4, 34, 46, 45, 46, 24, NA, 9)
chin <- c(711, 43, NA, NA, 53, 14, 24, 32, 39, 29, 27, NA, 7)
moran <- c(425, 38.8, 34.4, 11.5, NA, 14.4, NA, NA, NA, 29.6, 11.3, NA, NA)
swift <- c(164, 34.8, 11.6, 3.0, NA, 11.0, 11.8, NA, NA, 14.0, 12.8, 1.2, 0) # faculty
latan <- c(472, 49.15, 57.63, 43.43, NA, 59.32, 42.80, NA, NA, 37.29, 22.67, 20.3, 44.70)
garciagarzon <- c(131, 24.6, 53.8, 27.7, NA, 20.0, 27.7, NA, NA, 43.1, 13.8, 13.8, 0) #Involved
brachem <- c(1397, 15.2, 36.4, 17, 25.5, 11.4, 22.4, NA, NA, 12.3, 3.4, NA, 0)

labels <- c("n", "Selectively reporting what 'worked'", "Selectively reporting outcomes", "Failing to report all conditions", "Selectively reporting performed analyses", "Optional stopping", "Exclude data depending on impact on results", "Selectively including covariates", "Switch analysis selectively", "HARKing", "Opportunistically rounding p-values", "Hiding demographic moderators", "Falsifying data")

qrp_review <- tibble(labels, john, agnoli, motyl, rabelo, fraser_eco, fraser_evo, makel, bakker, chin, fiedler, moran, swift, latan, garciagarzon, brachem)

qrp_rev_long <-
  qrp_review |> 
  t() |> 
  janitor::row_to_names(row_number = 1) |> 
  as.data.frame() |> 
  rownames_to_column("article") |> 
  mutate(across(-article, parse_number)) |> 
  pivot_longer(-c(article, n), names_to = "labels", values_to = "prop", values_drop_na = TRUE) |> 
  mutate(prop = prop / 100,
         x = as.integer(prop * n))


# Perform meta analysis ---------------------------------------------------
# Calculate standard arcsine transformed proportions that can deal with 0s and 1s
qrp_rev_ma <- 
  escalc(measure = "PAS", xi = x, ni = n, data = qrp_rev_long) |> 
  group_by(labels) |> 
  nest()

qrp_meta_res <- 
  qrp_rev_ma %>%
  # Calculate pooled effect size (weighted mean of transformed values)
  mutate(meta = map(data, ~rma(yi = yi, vi = vi, ni = n, method = "REML", data = .x)),
         pred = map(meta, ~predict(.x, transf = transf.iarcsin) |> 
                as_tibble()),
         k = map_int(data, nrow),
         n_sum = map_int(data, ~summarise(.x, sum(n)) |> 
                 pull())
         ) |> 
  unnest(pred) |> 
  ungroup()

# A meta-analysis pooled effect sizes
qrp_meta_res |> 
  mutate(labels = paste0(labels, "\nk = ", k, ", N = ", n_sum),
         labels = fct_reorder(labels, pred, .na_rm = TRUE)) |> 
  ggplot() +
  aes(x = pred, y = labels, xmin = ci.lb, xmax = ci.ub) +
  geom_point(aes(size = n_sum), shape = 15) +
  geom_errorbar(width = .25) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_size_continuous(trans = "log10", range = c(2,5)) +
  scale_fill_viridis_d() +
  labs(y = NULL, title = "Pooled effect sizes (proportions) of QRP prevalence rates",
       size = "Pooled participants",
       x = "Pooled prevalance",
       caption  =" k: Number of studies, N: Total number of participants")
  


# Alternate method using the meta package -----------------------------------------------------

qrp_meta_alt <- 
  qrp_rev_ma %>%
  # Calculate pooled effect size (weighted mean of transformed values)
  mutate(metamodel = map(data, ~meta::metaprop(event = x, n = n, sm = "PAS", backtransf = TRUE, 
                                               data = .x)),
         k = map_int(data, nrow),
         n_sum = map_int(data, ~summarise(.x, sum(n)) |> 
                           pull())
  ) |> 
  ungroup()

library(meta)

qrp_meta_alt |> 
  slice(1) |> 
  pull(metamodel) |> 
  print.meta()
  str()


  labs(y = NULL, 
       x = "Pooled prevalence (95% CI)", 
       # title = "Pooled effect sizes (proportions) of QRP prevalence rates",
       # caption  =" k: Number of studies, N: Total number of participants"
       )


# A Box plot of observed proportions (obsolete)
qrp_rev_long |> 
  mutate(labels = fct_reorder(labels, prop, .na_rm = TRUE)) |> 
  ggplot() +
  aes(x = prop, y = labels, fill = labels) +
  geom_quasirandom(show.legend = FALSE, alpha = .5) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0, alpha = .7) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_viridis_d() +
  labs(y = NULL)

meta::metaprop()
