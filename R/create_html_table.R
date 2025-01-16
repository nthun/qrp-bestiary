library(tidyverse)
library(googlesheets4)
library(DT)
library(htmltools)

# Read the QRP data from the Google Sheet
qrp_raw <- 
  range_read("1Zl3Wc6JlmT4C2SgqC_6vlrl_1u-JDut1kYSuH-MDWhk")


# Process data
qrp_long <- 
  qrp_raw |> 
  filter(include == 1) |> 
  select(-`assigned group`, -include) |> 
  mutate(research_phase = fct_inorder(research_phase)) |> 
  arrange(phase_order, qrp) |> 
  mutate(clues = if_else(clues == "-", "None  \n", clues),
         aliases = if_else(is.na(aliases), "—  \n", aliases),
         umbrella_terms = if_else(umbrella_terms == "-", "None  \n", umbrella_terms),
         `source(s)` = str_replace_all(`source(s)`, "(?<=\n|^)", "- "),
         qrp = fct_inorder(qrp),
         damage = if_else(is.na(damage), "—", damage)
  )

# Format the data for printing
qrp_2print <- 
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
  rowwise() |> 
  mutate(across(everything(), commonmark::markdown_html)) |> 
  ungroup()

datatable_html <- datatable(
  qrp_2print,
  escape = FALSE, # Allow HTML formatting in the table
  extensions = c("Buttons", "Responsive"), # Enable export buttons
  rownames = FALSE, # Disable row numbers
  filter = "top", # Add column-wise filters above the table
  class = "display",
  options = list(
    pageLength = 40, # Page size
    caption = "QRP Bestiary",
    searchHighlight = TRUE,
    autoWidth = TRUE, # Adjust column widths
    dom = 'Bfrtip', # Include buttons, filtering, and pagination
    buttons = c('copy', 'excel', 'pdf', 'print'), # Export options
    initComplete = JS(
      "function(settings, json) {",
      "$('ul').css({'list-style-position': 'inside', 'margin': '0', 'padding': '0'});", # Fix bullet points
      "$('.dt-buttons').css({'margin-bottom': '10px'});", # Style export buttons
      "$('td, th').css({'text-align': 'left', 'vertical-align': 'top'});", # Top-left alignment for all columns
      "}"
    )
  )
)

datatable_html

# Add a title and save as an HTML file

html_output <- tagList(
  # Add global CSS for table text and title
  tags$style(HTML("
    h1 {
      text-align: center;
      font-family: Roboto, Arial, sans-serif;
      color: #333;
      margin-bottom: 16px;
    }
    table.dataTable td, table.dataTable th {
      font-family: Roboto, Arial, sans-serif;
      font-size: 14px;
    }
  ")),
  # Add the title
  tags$h1("QRP Bestiary"),
  # Add the DataTable CSS
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.datatables.net/1.13.6/css/dataTables.bootstrap5.min.css"),
    tags$script(src = "https://cdn.datatables.net/1.13.6/js/jquery.dataTables.min.js"),
    tags$script(src = "https://cdn.datatables.net/1.13.6/js/dataTables.bootstrap5.min.js")
  ),
  # Add the datatable
  datatable_html
)


# Save the HTML file
save_html(html_output, "docs/qrp_bestiary_table.html")
