# To convert a tibble into a network where the tibble contains two columns, "category" and "item", with 39 items and 5 categories, you can use the `igraph` package in R. Here's an example code that demonstrates the conversion:


library(ggsankey)

umb_left <- 
  umbrella |> 
  mutate(side = c(0, 0, 0, 1, 1, 1)) |> 
  unnest(items) |> 
  filter(side == 0) |> 
  transmute(x = 1, node = items, next_x = 2, next_node = umbrella_terms)

umb_right <- 
  umbrella |> 
  mutate(side = c(0, 0, 0, 1, 1, 1)) |> 
  unnest(items) |> 
  filter(side == 1) |> 
  transmute(x = 4, node = umbrella_terms, next_x = 5, next_node = items)

umb_center <-
  tibble(x = 2, node = unique(umb_left$next_node), next_x = 3, next_node = "QRP") |> 
  bind_rows(
  tibble(x = 3, node = "QRP", next_x = 4, next_node = unique(umb_right$node))  
  )

cat_colors <- 
  tibble(umbrella_terms = c(unique(umb_left$next_node),
                "QRP",
                unique(umb_right$node))) |> 
  mutate(fill = viridisLite::viridis(n = 7)) |> 
  left_join(umbrella_terms, by = "umbrella_terms") |> 
  select(items, fill) |> 
  deframe()

umb_all <- 
  bind_rows(umb_left, umb_center, umb_right)


umb_all |> 
  ggplot() +
  aes(x = x, 
      next_x = next_x, 
      node = node, 
      next_node = next_node,
      label = node,
      fill = node) +
  geom_sankey(show.legend = FALSE) +
  geom_sankey_text(size = 3, show.legend = FALSE) +
  # geom_alluvial(show.legend = FALSE) +
  # geom_alluvial_text(size = 3, show.legend = FALSE) +
  scale_fill_manual(cat_colors) +
  theme_sankey()

# QRPs as mindmap?

dir.create("qrp")

umbrella |> 
  unnest(items) |> 
  rowwise() |> 
  mutate(path = fs::path("qrp", umbrella_terms, items)) |> 
  pull() |> 
  walk(fs::dir_create)


??path


# As a mindmap

library(mindr)

mindr::mm(here::here("qrp"))

input <- system.file("examples/mindr-md.Rmd", package = "mindr")
input_txt <- readLines(input, encoding = "UTF-8")

mm_output <- mm(input_txt, output_type = c("mindmap", "markdown", "R", "widget"))
mm_output
