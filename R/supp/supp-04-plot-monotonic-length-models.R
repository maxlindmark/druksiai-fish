# Code to plot output from 03-fit-length-models.qmd
library(ggplot2)
library(forcats)
library(viridis)
library(patchwork)
library(dplyr)
library(readr)
library(tidybayes)
library(stringr)
library(RColorBrewer)

# Not on CRAN !
# devtools::install_github("seananderson/ggsidekick")
library(ggsidekick)
theme_set(theme_sleek())

# Set relative path
home <- here::here()

# Read data
load(file = paste0(home, "/data/druksiai2.RData"))

# Scale values by species averages - lifetime and birth temperature and length
d <- df6 |>
  group_by(species) |>
  mutate(
    age_sc = (age - mean(age)) / sd(age),
    age_sq = age_sc * age_sc,
    lifetime_avg_temp_sc = (lifetime_avg_temp - mean(lifetime_avg_temp)) / sd(lifetime_avg_temp),
    birth_temp_sc = (birth_temp - mean(birth_temp)) / sd(birth_temp)
  ) |>
  ungroup()

# Read output
prior_post_all <- read_csv(paste0(home, "/output/supp/prior_post_all_mono.csv"))
ppc_all <- read_csv(paste0(home, "/output/supp/ppc_all_mono.csv"))
preds_all <- read_csv(paste0(home, "/output/supp/preds_all_mono.csv"))

# Interaction coefficients
species_order <- prior_post_all |>
  filter(
    type == "posterior",
    parameter == "bsp_moage_int:birth_temp_sc"
  ) |>
  group_by(species) |>
  summarise(mean_value = mean(value), .groups = "drop") |>
  arrange(mean_value) |>
  pull(species)

species_colors <- setNames(
  brewer.pal(n = 12, name = "Set3")[c(1, 3:12)][seq_along(species_order)],
  species_order
)

prior_post_all <- prior_post_all |>
  mutate(
    species = factor(species, levels = species_order),
    species_id = as.numeric(species)
  )


# Read main output
prior_post_all_main <- read_csv(paste0(home, "/output/prior_post_all_mono.csv"))
ppc_all_main <- read_csv(paste0(home, "/output/ppc_all_mono.csv"))
preds_all_main <- read_csv(paste0(home, "/output/preds_all_mono.csv"))

# Interaction coefficients
species_order_main <- prior_post_all_main |>
    filter(
        type == "posterior",
        parameter == "bsp_moage_int:birth_temp_sc"
    ) |>
    group_by(species) |>
    summarise(mean_value = mean(value), .groups = "drop") |>
    arrange(mean_value) |>
    pull(species)

prior_post_all_main <- prior_post_all_main |>
    mutate(
        species = factor(species, levels = species_order),
        species_id = as.numeric(species)
    )

# Combine and color by type of temperature used
prior_post_all_comb <- 
    bind_rows(
        prior_post_all_main |> 
            mutate(Temperature = "Lifetime average temperature") |> 
            mutate(parameter2 = ifelse(parameter %in% c("bsp_moage_int:birth_temp_sc",
                                                       "bsp_moage_int:lifetime_avg_temp_sc"),
                                      "age_temp_interaction", parameter)),
        prior_post_all |> 
            mutate(Temperature = "Birth temperature") |> 
            mutate(parameter2 = ifelse(parameter %in% c("bsp_moage_int:birth_temp_sc",
                                                        "bsp_moage_int:lifetime_avg_temp_sc"),
                                       "age_temp_interaction", parameter))
    )

ggplot(
    prior_post_all_comb |>
    filter(
      type == "posterior",
      parameter2 == "age_temp_interaction"
    ),
  aes(value, species, fill = Temperature, color = Temperature)
) +
  stat_halfeye(
    size = 0.5, width = 0.95, justification = -0.1, height = 1, alpha = 0.5
  ) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5, linewidth = 0.3) +
  #guides(fill = "none", color = "none") +
  xlim(-0.065, 0.025) +
  labs(x = "Temperature-age interaction coefficient") +
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  annotate(
    "text",
    x = c(-0.05, 0.015),
    y = c(10.75, 10.75),
    label = c(
      "Temp. coefficient decreasing with age",
      "Temp. coefficient increasing with age"
    ),
    size = 2.4,
    fontface = "italic",
    color = "grey30"
  ) +
  theme(axis.title.y.left = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

ggsave(paste0(home, "/figures/supp/supp_interaction_coef.png"), width = 17, height = 12, units = "cm")
