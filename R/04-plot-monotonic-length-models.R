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
prior_post_all <- read_csv(paste0(home, "/output/prior_post_all_mono.csv"))
ppc_all <- read_csv(paste0(home, "/output/ppc_all_mono.csv"))
preds_all <- read_csv(paste0(home, "/output/preds_all_mono.csv"))

# Plot posterior predictive checks
ggplot(ppc_all) +
  geom_density(
    aes(x = value, group = .draw, color = type),
    linewidth = 0.4
  ) +
  facet_wrap(~species, ncol = 4, scales = "free") +
  scale_color_manual(values = viridis(n = 5, option = "plasma")[c(2, 4)]) +
  coord_cartesian(expand = 0) +
  guides(color = guide_legend(position = "inside")) +
  theme(
    legend.position.inside = c(0.05, 0.95),
    legend.title = element_blank()
  ) +
  labs(x = "Value", y = "Density")

ggsave(paste0(home, "/figures/supp/pp_check_length.png"), width = 18, height = 15, units = "cm")

# Plot prior vs posterior
# Join limits and filter both prior and posterior

# Group parameters to plot together
prior_post_all <- prior_post_all |>
  mutate(
    par_group = case_when(
      str_starts(parameter, "simo_moage_int1") ~ "simo_moage_int1",
      str_starts(parameter, "simo_moage_int:") ~ "simo_moage_int:",
      str_starts(parameter, "b_") ~ "b",
      str_starts(parameter, "bsp") ~ "bsp_moage",
      TRUE ~ "sd_shape"
    )
  )

for (i in unique(prior_post_all$par_group)) {
  dd <- prior_post_all |>
    filter(par_group == i) |> 
    mutate(parameter = ifelse(par_group == "simo_moage_int:",
                              str_replace(parameter, ":", ":\n"),
                              parameter))

  limits <- dd |>
    filter(type == "posterior") |>
    group_by(parameter) |>
    summarise(
      lower = quantile(value, 0.01),
      upper = quantile(value, 0.99)
    )

  ncol <- if (i %in% c("simo_moage_int1", "simo_moage_int:")) {
    4
  } else if (i == "b") {
    2
  } else {
    1
  }

  dd |>
    left_join(limits, by = "parameter") |>
    filter(value > 0.2 * lower, value < upper * 5) |>
    ggplot(aes(value)) +
    geom_density(
      data = \(d) d |> filter(type == "posterior"),
      aes(fill = species, color = species),
      alpha = 0.4,
      color = NA
    ) +
    geom_density(
      data = \(d) d |> filter(type == "prior"),
      aes(group = parameter),
      fill = "grey60",
      alpha = 0.4,
      color = "grey20"
    ) +
    scale_fill_viridis(option = "plasma", discrete = TRUE) +
    scale_color_viridis(option = "plasma", discrete = TRUE) +
    facet_wrap(~parameter, ncol = ncol, scales = "free") +
    labs(x = "Value", y = "Density") +
    coord_cartesian(expand = 0) +
    theme(
      strip.text.x.top = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_blank()
    )

  ggsave(paste0(home, "/figures/supp/prior_post/prior_post_", i, ".png"), width = 18, height = 22, units = "cm")
}


# Interaction coefficients
species_order <- prior_post_all |>
  filter(
    type == "posterior",
    parameter == "bsp_moage_int:lifetime_avg_temp_sc"
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
  mutate(species = factor(species, levels = species_order),
         species_id = as.numeric(species))

ggplot(prior_post_all |>
         filter(type == "posterior",
                parameter == "bsp_moage_int:lifetime_avg_temp_sc"),
       aes(value, species, fill = species, color = species)) +
  stat_halfeye(
    size = 0.5, width = 0.95, justification = -0.1, height = 1
  ) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5, linewidth = 0.3) +
  scale_fill_manual(values = species_colors) +
  scale_color_manual(values = species_colors) +
  guides(fill = "none", color = "none") +
  xlim(-0.065, 0.025) +
  labs(x = "Temperature-age interaction coefficient") +
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
  theme(axis.title.y.left = element_blank())

ggsave(paste0(home, "/figures/interaction_coef.png"), width = 17, height = 12, units = "cm")


# Conditional predictions (mean vs warm)
pred_summary <- preds_all |>
  mutate(
    temp_label = ifelse(
      lifetime_avg_temp == 19,
      "Mean temperature",
      "Warm (mean +2°C)"
    )
  ) |>
  group_by(age_int, life_stage, temp_label, species) |>
  median_qi(.epred, .width = 0.95)

f_labels <- d |>
  summarise(n = n(), .by = species) |>
  mutate(label = paste("n =", n))

dplot <- d |>
  slice_sample(n = 50, by = c(species, age))

ggplot(
  pred_summary,
  aes(age_int, .epred, color = temp_label, fill = temp_label)
) +
  facet_wrap(~species, ncol = 3, scale = "free") +
  geom_jitter(
    data = dplot, aes(age, l), inherit.aes = FALSE, alpha = 0.25, height = 0, width = 0.4,
    size = 0.75, color = "grey50"
  ) +
  geom_text(
    data = f_labels, aes(label = label, hjust = -0.1, vjust = 1.5), inherit.aes = FALSE,
    x = -Inf, y = Inf, size = 2.3, color = "grey20"
  ) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper),
    width = 0, alpha = 0.5, position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  labs(
    x = "Age (years)",
    y = "Predicted length (cm)",
    color = "Temperature",
    fill = "Temperature"
  ) +
  scale_color_manual(
    values = brewer.pal(n = 9, name = "Set1")[2:1]
  ) +
  scale_fill_manual(
    values = brewer.pal(n = 9, name = "Set1")[2:1]
  ) +
  guides(
    fill = "none",
    color = guide_legend(title.position = "top", title.hjust = 0.5, position = "inside")
  ) +
  theme(
    legend.position.inside = c(0.67, 0.12),
    legend.direction = "horizontal",
    legend.key.width = unit(0.6, "cm"),
    legend.key.height = unit(0.1, "cm")
  )

ggsave(paste0(home, "/figures/length_at_age_pred.png"), width = 17, height = 17, units = "cm")


# Compute warm-mean differences within draw and age
delta_draws <- preds_all |>
  dplyr::select(species, age_int, life_stage, lifetime_avg_temp, .draw, .epred) |>
  tidyr::pivot_wider(
    id_cols = c(species, age_int, life_stage, .draw),
    names_from = lifetime_avg_temp,
    values_from = .epred,
    names_prefix = "temp_"
  ) |>
  mutate(
    delta_pct = ((temp_21 - temp_19) / temp_19) *100
  )

# Temperature effect at age (absolute, cm)
delta_age_summary <- delta_draws |>
  group_by(species, age_int) |>
  median_qi(delta_pct, .width = 0.95)

# Match species order
delta_age_summary <- delta_age_summary |>
  mutate(species = factor(species, levels = species_order))

ggplot(delta_age_summary, aes(age_int, delta_pct, color = species, fill = species)) +
  facet_wrap(~species, ncol = 3, scales = "free") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
    alpha = 0.3, color = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5, linewidth = 0.3) +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  labs(
    x = "Age (years)",
    y = "Percent change in length after +2°C"
  ) +
  guides(
    fill = "none",
    color = "none"
  )

summary(delta_age_summary$delta_pct)
quantile(delta_age_summary$delta_pct, probs = c(0.05, 0.5, 0.95))

ggsave(paste0(home, "/figures/diff_at_age_pred.png"), width = 17, height = 17, units = "cm")


# Life-stage summary (mean % difference per stage)
# Summarise % change by draw, species and life stage, then summarise quantiles by species and life stage (across draws)
life_stage_species_delta <- delta_draws |>
  group_by(species, life_stage, .draw) |>
  summarise(mean_delta_pct = mean(delta_pct, na.rm = TRUE), .groups = "drop") |>
  group_by(species, life_stage) |>
  median_qi(mean_delta_pct, .width = 0.95) |> 
  mutate(life_stage = factor(life_stage,
                             levels = c("Juvenile", "Maturation age", "Adult", "Old adult"))) |>
  mutate(species = factor(species, levels = species_order))

# Points + error bars per species
p1 <- ggplot(life_stage_species_delta,
             aes(life_stage, mean_delta_pct, color = species)) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5, linewidth = 0.3) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper),
                width = 0,
                position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_color_manual(values = species_colors) +
  labs(
    x = "Life stage",
    y = "Percent change in length after +2°C"
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    axis.title.y.left = element_blank()
  )

p1

# Halfeye across species, by life stage only, don't do the final summarizing and instead plot posterior
life_stage_delta <- delta_draws |>
  group_by(life_stage, .draw) |>
  summarise(mean_delta_pct = mean(delta_pct, na.rm = TRUE), .groups = "drop") |>
  mutate(life_stage = factor(life_stage,
                             levels = c("Juvenile", "Maturation age", "Adult", "Old adult")))

p2 <- ggplot(life_stage_delta, 
             aes(mean_delta_pct, life_stage)) +
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5, linewidth = 0.3) +
  stat_halfeye(.width = c(0.95), alpha = 0.6) +
  scale_color_manual(values = brewer.pal(n = 12, name = "Set3")[c(1, 3:12)]) +
  labs(
    x = "Percent change in length after +2°C",
    y = "Life stage"
  ) +
  theme(
    axis.title.y.left = element_blank(),
    legend.title = element_blank()
  )

p2

# Combine plots
(p2 + p1) +
  plot_layout(axes = "collect") +
  plot_annotation(tag_levels = "A")

ggsave(paste0(home, "/figures/life_stage_sp_diff.png"), width = 18, height = 10, units = "cm")
