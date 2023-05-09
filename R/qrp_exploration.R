# Create a network of QRP topics

library(tidyverse)
library(googlesheets4)
library(tidytext)
library(stm)
library(ldatuning)

qrp_raw <- 
  range_read("1Zl3Wc6JlmT4C2SgqC_6vlrl_1u-JDut1kYSuH-MDWhk") |> 
  filter(include == 1)

qrp_def <- 
  qrp_raw |> 
  select(qrp, definition)

qrp_sparse <- 
  qrp_def |> 
  unnest_tokens(word, definition) |> 
  anti_join(stop_words, by = "word") |> 
  count(qrp, word) |> 
  cast_sparse(qrp, word, n)

# Optimal number of topics
topics_rs <- FindTopicsNumber(qrp_sparse, topics = 3:15)
FindTopicsNumber_plot(topics_rs)


qrp_topic <- stm(qrp_sparse, K = 11)
qrp_beta <- tidy(qrp_topic, matrix = "beta")
qrp_gamma <- tidy(qrp_topic, matrix = "gamma", document_names = rownames(qrp_sparse))

# Which terms belong to which topic
qrp_beta |> 
  group_by(topic) |> 
  slice_max(beta, n = 10, with_ties = FALSE) |> 
  ungroup() |> 
  mutate(term = reorder_within(term, beta, topic)) |> 
  ggplot() +
  aes(x = beta, y = term, fill = topic) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~topic, scales = "free_y")

# Which documents belong to which topic
qrp_gamma |> 
  ggplot() +
  aes()


# Create damage matrix ----------------------------------------------------

qrp_raw |> 
  select(qrp, damage) |> 
  separate_rows(damage, sep = "\n|\r\n") |> 
  mutate(damage = str_remove(damage, "- ") |> str_squish(),
         value = "X") |> 
  pivot_wider(names_from = damage,
              values_from = value, values_fill = "") |> 
  insight::print_html()
  


# Qrps by research phase --------------------------------------------------

qrp_raw |> 
  count(research_phase)

qrp_raw |> 
  count(detectability)

# Contributors ------------------------------------------------------------

contributors_raw <- 
  range_read("1B6IdiEgawLZgNod61Pu5CP6ZfKYXh5Za5LFuWGxXkdg") 

contributors_raw |> 
  count(continued_work)

contributors_raw |> 
  count(country, sort = TRUE)

contributors_raw |> 
  count(position = `unified position`, sort = TRUE)



# Clues -------------------------------------------------------------------

qrp_raw |> 
  select(qrp, clues) |> 
  separate_rows(clues, sep = "- |\n") |> 
  filter(!clues %in% c("", "-", NA)) |> 
  count(clues, sort = TRUE) |> 
  print(n = 100)
