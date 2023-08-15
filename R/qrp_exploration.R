# Create a network of QRP topics

library(tidyverse)
library(googlesheets4)
library(tidytext)
library(gt)
# library(venndir)

theme_set(theme_light())

qrp_raw <- 
  range_read("1Zl3Wc6JlmT4C2SgqC_6vlrl_1u-JDut1kYSuH-MDWhk")

qrp <- 
  qrp_raw |> 
  filter(include == 1) |> 
  select(-`assigned group`, -include) |> 
  mutate(research_phase = fct_inorder(research_phase))


# QRPs by research phase --------------------------------------------------
qrp |> 
  count(research_phase)

qrp |> 
  select(research_phase, qrp) |> 
  group_by(research_phase) |> 
  arrange(research_phase, qrp) |> 
  mutate(qrp_order = row_number()) |> 
  pivot_wider(names_from = research_phase, values_from = qrp) |> 
  select(-qrp_order) |>
  gt() |> 
  sub_missing(missing_text = "")


# Umbrella terms ----------------------------------------------------------
qrp |> 
  separate_rows(umbrella_terms, sep = "\n") |> 
  count(umbrella_terms, sort = TRUE)

umbrella <-
  qrp |> 
  select(umbrella_terms, qrp) |> 
  separate_rows(umbrella_terms, sep = "\n") |> 
  # mutate(umbrella_terms = if_else(umbrella_terms == "-", NA, umbrella_terms)) |> 
  group_by(umbrella_terms) |> 
  summarize(items = list(qrp))

umbrella |> 
  pull(items) |> 
  set_names(umbrella$umbrella_terms)

library(ggVennDiagram)

umbrella |> 
  pull(items) |> 
  set_names(umbrella$umbrella_terms) |> 
  ggvenn::ggvenn(show_elements = TRUE)

library(venndir)

umbrella |> 
  pull(items) |> 
  set_names(umbrella$umbrella_terms) |> 
  venndir(
        poly_alpha=0.3,
        # label_preset="main items",
        show_items="item",
        proportional=TRUE)

# As a table
qrp |> 
  select(umbrella_terms, qrp) |> 
  separate_rows(umbrella_terms, sep = "\n") |> 
  mutate(umbrella_terms = if_else(umbrella_terms == "-", NA, umbrella_terms)) |>  
  group_by(umbrella_terms) |> 
  arrange(umbrella_terms, qrp) |> 
  mutate(qrp_order = row_number()) |> 
  pivot_wider(names_from = umbrella_terms, values_from = qrp) |> 
  select(-qrp_order) |>
  gt() |> 
  sub_missing(missing_text = "")


# Contributors ------------------------------------------------------------
contributors_raw <- 
  range_read("1B6IdiEgawLZgNod61Pu5CP6ZfKYXh5Za5LFuWGxXkdg") 

contributors_raw |> 
  count(continued_work)

contributors_raw |> 
  count(country, sort = TRUE)

contributors_raw |> 
  count(position = `unified position`, sort = TRUE)


# Damages -----------------------------------------------------------------
min_damage_n = 3

qrp |> 
  select(qrp, damage_aggregated) |> 
  separate_rows(damage_aggregated, sep = "- |\n") |> 
  filter(!damage_aggregated %in% c("", "-", NA)) |> 
  count(damage_aggregated, sort = TRUE) |> 
  filter(n <= 3) |> 
  print(n = 100) 

# Create a table, with aggregated damages
qrp |> 
  select(qrp, damage = damage_aggregated) |> 
  separate_rows(damage, sep = "\n|\r\n") |> 
  mutate(damage = str_remove(damage, "- ") |> str_squish(),
         value = "X",
         damage = fct_lump_min(damage, min = min_damage_n, other_level = "Other damage")) |> 
  pivot_wider(names_from = damage,
              values_from = value, 
              values_fn = first,
              values_fill = "") |> 
  relocate(-`Other damage`) |> 
  gt()

# List other damages

qrp |> 
  separate_rows(damage_aggregated, sep = "\n|\r\n") |> 
  transmute(
            damage = str_remove(damage_aggregated, "- ") |> str_squish(),
            damage_other = fct_lump_min(damage, min = min_damage_n, other_level = "Other damage")) |> 
  filter(damage_other == "Other damage") |> 
  count(damage, sort = TRUE)


# Remedies -----------------------------------------------------------------
qrp |> 
  select(qrp, remedy) |> 
  separate_rows(remedy, sep = "- |\n") |> 
  filter(!remedy %in% c("", "-", NA)) |> 
  count(remedy, sort = TRUE) |> 
  print(n = 100)

# Detectability -----------------------------------------------------------

qrp |> 
  count(detectability)


# Clues -------------------------------------------------------------------
qrp |> 
  select(qrp, clues) |> 
  separate_rows(clues, sep = "- |\n") |> 
  filter(!clues %in% c("", "-", NA)) |> 
  count(clues, sort = TRUE) |> 
  print(n = 100)

# By research phase
# TODO: Instead of this, determine the place of each clue in a publication in reading order, e.g., introduction, methods, results, etc.
# This will be needed for the app
qrp |> 
  select(qrp, research_phase, clues) |> 
  separate_rows(clues, sep = "- |\n") |> 
  filter(!clues %in% c("", "-", NA)) |> 
  count(research_phase, sort = TRUE) |> 
  print(n = 100)




# Create a document that can be inserted into the manuscript --------------
yaml_header <- 
"
---
output: 
  word_document:
    reference_docx: ../docs/docx-template.docx
---
"
  
qrp_template <- read_file("docs/qrp_list_template.txt")

qrp_text <- 
  qrp |> 
  mutate(clues = if_else(clues == "-", "None  \n", clues),
         aliases = if_else(is.na(aliases), "-  \n", aliases),
         umbrella_terms = if_else(umbrella_terms == "-", "None  \n", umbrella_terms),
         `source(s)` = str_replace_all(`source(s)`, "(?<=\n|^)", "- ")) |> 
  select(-phase_order, -qrp_order) |> 
  mutate(qrp = fct_inorder(qrp),
         qrp_id = row_number()) |> 
  group_by(qrp_id) |> 
  nest() |> 
  mutate(text = map(data, ~str_glue(qrp_template))) |> 
  unnest(text) |> 
  pull(text)
  

paste0(qrp_text, collapse = "\n") %>%
  paste0(yaml_header, .,collapse = "\n\n") |> 
  write_lines("docs/qrp_text.Rmd")

rmarkdown::render("docs/qrp_text.Rmd", output_file = "qrp_text.docx")

