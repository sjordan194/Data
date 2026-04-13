library(tidyverse)
library(palmerpenguins)
library(RColorBrewer)
library(showtext)
library(patchwork)
library(ggtext)
library(glue)
font_add_google("Roboto", "roboto")
showtext_auto()
# Load the penguins dataset
data("penguins")
theme_set(theme_classic(base_size = 13))
theme_update(
  panel.grid.majaor.y = element_line(color = "grey70", linewidth = 0.2)
)
glimpse(penguins)
# Filter out rows with missing values in 'body_mass_g' and  'sex  ' 
penguins_clean <- penguins %>%
  drop_na()
glimpse(penguins_clean)

which(is.na(penguins))
sum(is.na(penguins))
colSums(is.na(penguins))

penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = sex, shape = sex)) + 
  geom_point()

RColorBrewer::display.brewer.all()

penguins_clean |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = sex)) +
  geom_point(size = 1.5) + 
  scale_color_grey() + 
  theme_bw(family = "roboto") + 
  theme(panel.border = element_blank())


penguins |>
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE),
.by = island)


penguins |> 
  ggplot(aes(x = island, y = body_mass_g)) + 
  stat_summary(geom = "col")

palmerpenguins::penguins |> 
  ggplot(aes(x = island, y = body_mass_g)) +
  stat_summary(fun = mean, geom = "col") +
  stat_summary(
    fun.data = mean_sdl,
    fun.args = list(mult = 1), # mult = 1 → ±1 SD; mult = 2 → ±2 SD
    geom = "errorbar",
    width = 0.2
  )

ggplot(penguins_clean, aes(x = flipper_length_mm)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") + 
  labs(title = "Distribution of Bill Lengths in Penguins",
       x = "flipper Length (mm)",
       y = "Frequency") + 
  theme_classic()
penguins_clean |> 
  ggplot(aes(x = island, y = body_mass_g)) + 
  geom_boxplot() +
  facet_wrap(~species)
penguins_clean |> 
  drop_na(island, body_mass_g, species, sex) |> # remove missing values
  ggplot(aes(x = island, y = body_mass_g)) +
  geom_boxplot() +
  facet_grid(rows = vars(sex), cols = vars(species))

ggplot(penguins_clean, aes(x = flipper_length_mm, y = bill_length_mm, color = sex)) + 
  geom_point(size = 2, alpha = 0.7) + 
  facet_wrap(~species) + 
  theme_minimal(base_family = "roboto") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p <- penguins |>
  drop_na() |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, fill = species)) +
  geom_point(shape = 21, size = 2) + 
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::label_comma())

p + theme_bw() + theme(legend.title = element_text(face = "bold", family = "roboto"), 
                            legend.position = "top", 
                       panel.grid.minor = element_blank(), 
                       panel.grid.major.x = element_blank(), 
                       panel.grid.major.y = element_line(color = "grey70", linetype = "longdash", linewidth = 0.5))


library(grid)
library(cowplot)
library(png)
penguin_img_url <- "https://raw.githubusercontent.com/osu-codeclub/osu-codeclub.github.io/main/posts/S11E06_ggplot_06/3penguins.png"
download.file(url = penguin_img_url, destfile = "3penguins.png")

calculate_bmi <- function(weight_kg, height_cm) {
  weight_kg/(height_cm/100)^2
}
calculate_bmi(weight_kg = 80, height_cm = 180)

for (focal_species in penguin_species) {
  # filter your df for each focal_species  
  one_penguin_species <- penguins |> 
    filter(species == focal_species) |> 
    drop_na(bill_length_mm, bill_depth_mm, sex)


  
  map(.x = 1:5, 
      function(x) x + 1)

