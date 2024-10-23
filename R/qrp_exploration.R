# Create a network of QRP topics

library(tidyverse)
library(googlesheets4)
library(tidytext)
library(gt)
library(janitor)

theme_set(theme_light())

qrp_raw <- 
  range_read("1Zl3Wc6JlmT4C2SgqC_6vlrl_1u-JDut1kYSuH-MDWhk")

qrp <- 
  qrp_raw |> 
  filter(include == 1) |> 
  select(-`assigned group`, -include) |> 
  mutate(research_phase = fct_inorder(research_phase))

page_size = 4

qrp_long <-   
  qrp |> 
  # To create a paged wide layout, define the number of columns on the page
  mutate(page = ((row_number() - 1) %/% page_size) + 1) |> 
  arrange(page, phase_order, qrp) |> 
  mutate(clues = if_else(clues == "-", "None  \n", clues),
         aliases = if_else(is.na(aliases), "--  \n", aliases),
         umbrella_terms = if_else(umbrella_terms == "-", "None  \n", umbrella_terms),
         `source(s)` = str_replace_all(`source(s)`, "(?<=\n|^)", "- "),
         qrp = fct_inorder(qrp)
  )


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
  mutate(umbrella_terms = if_else(umbrella_terms == "-", "None", umbrella_terms)) |>
  group_by(umbrella_terms) |> 
  summarize(items = list(qrp))

umbrella |> 
  pull(items) |> 
  set_names(umbrella$umbrella_terms)


umbrella |> 
  mutate(n = map_int(items, length),
         items = map_chr(items, ~paste(sort(.x), collapse = ", "))) |> 
  mutate(Description = "", .before = items) |> 
  gt() |> 
  cols_label(umbrella_terms = "Umbrella term",
             items = "QRPs") # |> 
  # gtsave("docs/umbrella_terms.docx")


# Contributors ------------------------------------------------------------
contributors_raw <- 
  range_read("1B6IdiEgawLZgNod61Pu5CP6ZfKYXh5Za5LFuWGxXkdg") 

contributors_raw |> 
  count(continued_work)

contributors_raw |> 
  count(country, sort = TRUE)

contributors_raw |> 
  count(position = `unified position`, sort = TRUE)

credit_raw <- range_read("1Wap7eVD8Y1atM4fbHAkyw7pXOVCG-uobPPoouU5KNIs")

credit <- 
  credit_raw |> 
  drop_na(Surname) |> 
  mutate(`Middle name` = if_else(is.na(`Middle name`), "", `Middle name`))

paste0(credit$Surname, 
       ", ",
       credit$Firstname,
       # " ",
       credit$`Middle name`
       ) |> 
  paste(collapse = "; ")

# Harms -----------------------------------------------------------------
min_damage_n = 3

qrp |> 
  select(qrp, damage_aggregated) |> 
  separate_rows(damage_aggregated, sep = "- |\n") |> 
  filter(!damage_aggregated %in% c("", "-", NA)) |> 
  count(damage_aggregated, sort = TRUE) |> 
  filter(n > 3) |> 
  print(n = 100) 

# Create a table, with aggregated harms
qrp |> 
  select(qrp, research_phase, damage = damage_aggregated) |> 
  separate_rows(damage, sep = "\n|\r\n") |> 
  mutate(damage = str_remove(damage, "- ") |> str_squish(),
         value = "X",
         damage = fct_lump_min(damage, min = min_damage_n, other_level = "Other damage")) |> 
  pivot_wider(names_from = damage,
              values_from = value, 
              values_fn = first,
              values_fill = "") |> 
  select(-`NA`) %>%
  select(names(.) |> sort()) |> 
  relocate(qrp) |> 
  relocate(-`Other damage`) |> 
  group_by(research_phase) |> 
  arrange(qrp, .by_group = TRUE) |> 
  gt() |> 
  cols_label(`Other damage` = "Other specific damage",
             qrp = "QRP") |> 
  tab_options(
              # column_labels.background.color = "#CCCCCC",
              column_labels.font.size = 12, 
              table.font.size = 12,
              column_labels.font.weight = "bold", 
              row_group.background.color = "#EEEEEE", 
              data_row.padding = 0, row_group.padding = 0,
              row_group.font.weight = "bold",
              table.align = "left")

# List other damages

qrp |> 
  separate_rows(damage_aggregated, sep = "\n|\r\n") |> 
  transmute(
            damage = str_remove(damage_aggregated, "- ") |> str_squish(),
            damage_other = fct_lump_min(damage, min = min_damage_n, other_level = "Other damage")) |> 
  filter(damage_other == "Other damage") |> 
  count(damage, sort = TRUE)


# List inflated confidence damages ------------------------------------------------------------
qrp |> 
  filter(str_detect(damage_aggregated, "confidence|credibility")) |> 
  separate_rows(damage, sep = "\n|\r\n") |> 
  transmute(
    damage = str_remove(damage, "- ") |> str_squish()
    ) |> 
  filter(str_detect(damage, "confidence|credibility")) |>
  count(damage, sort = TRUE)



# Remedies -----------------------------------------------------------------
qrp |> 
  select(qrp, remedy) |> 
  separate_rows(remedy, sep = "- |\n") |> 
  filter(!remedy %in% c("", "-", NA)) |> 
  count(remedy, sort = TRUE) |> 
  print(n = 100)


qrp |> 
  select(qrp, remedy_aggregated) |> 
  separate_rows(remedy_aggregated, sep = "- |\n") |> 
  filter(!remedy_aggregated %in% c("", "-", NA)) |> 
  count(str_squish(remedy_aggregated), sort = TRUE) |> 
  print(n = 100)

# Create a table, with aggregated remedies
qrp |> 
  select(qrp, research_phase, remedy = remedy_aggregated) |> 
  separate_rows(remedy, sep = "\n|\r\n") |> 
  mutate(remedy = str_remove(remedy, "- ") |> str_squish(),
         value = "X") |> 
  pivot_wider(names_from = remedy,
              values_from = value, 
              values_fn = first,
              values_fill = "") %>% 
  select(names(.) |> sort()) |> 
  relocate(qrp) |> 
  relocate(-`Other specific remedy`) |> 
  group_by(research_phase) |> 
  arrange(qrp, .by_group = TRUE) |> 
  gt() |> 
  cols_label(qrp = "QRP") |> 
  tab_options(
    # column_labels.background.color = "#CCCCCC",
    column_labels.font.size = 12, 
    table.font.size = 12,
    column_labels.font.weight = "bold", 
    row_group.background.color = "#EEEEEE", 
    data_row.padding = 0, row_group.padding = 0,
    row_group.font.weight = "bold",
    table.align = "left") |> 
    gtsave("docs/remedies_table.docx")



# Detectability -----------------------------------------------------------

qrp |> 
  count(detectability)

# Detectability table
detectability <- 
  qrp |> 
  select(qrp, detectability) |> 
  group_by(detectability) |> 
  summarize(items = list(qrp))

detectability |> 
  pull(items) |> 
  set_names(detectability$detectability)

detectability |> 
  mutate(n = map_int(items, length),
         items = map_chr(items, ~paste(sort(.x), collapse = ", ")),
         detectability = fct_relevel(detectability, c("Yes", "Maybe", "No"))) |> 
  arrange(detectability) |> 
  gt() |> 
  cols_label(detectability = "Detectability",
             items = "QRPs") |> 
  gtsave("docs/detectability_table.docx")


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
         `source(s)` = str_replace_all(`source(s)`, "(?<=\n|^)", "* ")) |> 
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


# Create a landscape layout -----------------------------------------------
# Create a broken up table and reassemble to be able to print properly

qrp_assembled <-
  qrp_long |> 
  select(qrp, `Alias(es) & related concepts` = aliases, Definition = definition, `Umbrella term(s)` = umbrella_terms, `Research phase` = research_phase, `Example(s)`, `Potential harms` = damage, Remedies = remedy, Detectability = detectability, Clues = clues, Sources = `source(s)`, page) |> 
  group_by(page) |> 
  nest() |> 
  mutate(data = map(data, ~ t(.x) %>%
                            row_to_names(row_number = 1) %>%
                            as.data.frame() %>%
                            rownames_to_column("Attribute")
                )
         )
 
# Create a function to apply formatting to all subtables 
format_qrp_table <- function(df){
  gt(df) |> 
    fmt_markdown() |> 
    opt_row_striping(row_striping = TRUE) |> 
    opt_table_lines(extent = "none") |> 
    cols_width(starts_with("Attribute") ~ px(120),
               everything() ~ px(270)) |> 
    tab_options(column_labels.background.color = "#CCCCCC",
                column_labels.font.weight = "bold",
                column_labels.font.size = 11, 
                table.font.size = 11,
                row.striping.background_color = "#EEEEEE",
                table.align = "left")
}


# Create the table
qrp_table <- 
  pull(qrp_assembled, data) |> 
  map(format_qrp_table) |> 
  gt_group(.list = _)

qrp_table

qrp_table

# if gtsave doesn't work, export printed view as html manually!
gtsave(qrp_table, "docs/qrp_table.html")


# Wide format -------------------------------------------------------------------------------
qrp_table_wide <- 
  qrp_long |>
  select(
    QRP = qrp,
    `Alias(es) & related concepts` = aliases,
    Definition = definition,
    `QRP umbrella term(s)` = umbrella_terms,
    `Research phase` = research_phase,
    `Example(s)`,
    `Potential harms` = damage,
    `Preventive measures` = remedy,
    Detectability = detectability,
    Clues = clues,
    Sources = `source(s)`
  ) |>
  gt() |> 
  fmt_markdown(columns = c(`Alias(es) & related concepts`,  `QRP umbrella term(s)`, `Example(s)`, `Potential harms`, `Preventive measures`, Clues, Sources)) |> 
  opt_row_striping(row_striping = TRUE) |> 
  opt_table_lines(extent = "none") |> 
  cols_width(starts_with(c("QRP", "Alias(es) & related concepts", "Umbrella term(s)", "Research phase","Detectability")) ~ px(120),
             starts_with("Sources")~ px(350),
             everything() ~ px(270)) |> 
  tab_options(column_labels.background.color = "#CCCCCC",
              column_labels.font.weight = "bold",
              column_labels.font.size = 11, 
              table.font.size = 11,
              row.striping.background_color = "#EEEEEE") |> 
  tab_style(style = cell_text(v_align = "top"),
            locations = cells_body()) |> 
  cols_align(columns = everything(),
             align = "left")

gtsave(qrp_table_wide, "docs/qrp_table_wide.html")

# !!!!
# ADD THE FOLLOWING LINE IN THE HTML HEAD:
# <link rel="stylesheet" type="text/css" href="no-indent-bullet.css">



  
