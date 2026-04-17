# Regenerate MCMC animation GIFs with updated dark theme
# Run this script once to refresh images/ after theme changes.

library(tidyverse)
library(gganimate)
library(gifski)

BG <- "#202123"

theme_dark_site <- function(...) {
  ggplot2::theme_minimal(...) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = BG, colour = NA),
      panel.background  = ggplot2::element_rect(fill = BG, colour = NA),
      panel.grid.major  = ggplot2::element_line(colour = "#333333"),
      panel.grid.minor  = ggplot2::element_line(colour = "#2d2e30"),
      text              = ggplot2::element_text(colour = "#cccccc"),
      axis.text         = ggplot2::element_text(colour = "#999999"),
      plot.title        = ggplot2::element_text(colour = "#e2e2e2", face = "bold"),
      plot.subtitle     = ggplot2::element_text(colour = "#999999"),
      plot.caption      = ggplot2::element_text(colour = "#666666"),
      legend.background = ggplot2::element_rect(fill = BG, colour = NA),
      legend.key        = ggplot2::element_rect(fill = BG, colour = NA),
      legend.text       = ggplot2::element_text(colour = "#cccccc"),
      legend.title      = ggplot2::element_text(colour = "#cccccc"),
      strip.background  = ggplot2::element_rect(fill = "#2d2e30", colour = NA),
      strip.text        = ggplot2::element_text(colour = "#cccccc")
    )
}

true_param <- c(2, -1)

target_density <- function(x, y) {
  exp(-0.5 * ((x - true_param[1])^2 / 1^2 + (y - true_param[2])^2 / 0.5^2))
}

# ── 1. Single-chain MCMC path ─────────────────────────────────────────────────
message("Generating single_chain_mcmc_animation.gif ...")
set.seed(123)
n_iter <- 100
x <- y <- 0
samples <- tibble(x = x, y = y, iteration = 1)
for (i in 2:n_iter) {
  x_prop <- rnorm(1, x, 0.4); y_prop <- rnorm(1, y, 0.4)
  if (runif(1) < target_density(x_prop, y_prop) / target_density(x, y)) {
    x <- x_prop; y <- y_prop
  }
  samples <- samples |> add_row(x = x, y = y, iteration = i)
}

anim1 <- ggplot(samples, aes(x = x, y = y)) +
  geom_path(color = "#FF5733", linewidth = 0.7) +
  geom_point(aes(x = x, y = y), color = "#e2e2e2", size = 2) +
  geom_point(aes(x = true_param[1], y = true_param[2]),
             color = "#00A68A", size = 3, shape = 4, stroke = 2) +
  annotate("text", x = true_param[1] + 0.1, y = true_param[2],
           label = "True value", color = "#00A68A", hjust = 0) +
  transition_reveal(iteration) +
  coord_fixed() +
  labs(title = "MCMC Chain Step: {frame_along}",
       x = bquote(beta[0]), y = bquote(beta[1])) +
  theme_dark_site()

animate(anim1, fps = 10, duration = 10, width = 800, height = 600,
        bg = BG, renderer = gifski_renderer())
anim_save("images/single_chain_mcmc_animation.gif")

# ── 2. Single-chain cumulative posterior ──────────────────────────────────────
message("Generating posterior_mcmc_animation.gif ...")
samples_long <- samples |>
  pivot_longer(cols = c(x, y), names_to = "parameter", values_to = "value") |>
  mutate(true_value = if_else(parameter == "x", true_param[1], true_param[2]))

samples_long_cumulative <- samples_long |>
  group_by(parameter) |>
  group_split() |>
  map_dfr(function(df) {
    param <- unique(df$parameter)
    map_dfr(1:max(df$iteration), function(i) {
      df |> filter(iteration <= i) |> mutate(frame = i, parameter = param)
    })
  }) |>
  mutate(parameter_label = case_when(
    parameter == "x" ~ "beta[0]",
    parameter == "y" ~ "beta[1]"
  ))

anim2 <- ggplot(samples_long_cumulative, aes(x = value)) +
  geom_histogram(binwidth = 0.4, fill = "#FF5733", color = "#e2e2e2", boundary = 0) +
  geom_vline(aes(xintercept = true_value), linetype = "dashed",
             color = "#00A68A", linewidth = 1) +
  facet_wrap(~parameter_label, scales = "free_x", ncol = 1, labeller = label_parsed) +
  transition_manual(frame) +
  labs(title = "Cumulative Posterior up to Iteration {current_frame}",
       x = "Parameter Value", y = "Frequency") +
  theme_dark_site()

animate(anim2, fps = 10, duration = 10, width = 800, height = 600,
        bg = BG, renderer = gifski_renderer())
anim_save("images/posterior_mcmc_animation.gif")

# ── 3. Single-chain traceplot ─────────────────────────────────────────────────
message("Generating trace_mcmc_animation.gif ...")
samples_long_cumulative_trace <- samples_long |>
  group_by(parameter) |>
  group_split() |>
  map_dfr(function(df) {
    param <- unique(df$parameter)
    map_dfr(1:max(df$iteration), function(i) {
      df |> filter(iteration <= i) |> mutate(frame = i, parameter = param)
    })
  }) |>
  mutate(parameter_label = case_when(
    parameter == "x" ~ "beta[0]",
    parameter == "y" ~ "beta[1]"
  ))

anim3 <- ggplot(samples_long_cumulative_trace, aes(x = iteration, y = value)) +
  geom_line(color = "#FF5733", linewidth = 0.8) +
  geom_hline(aes(yintercept = true_value), linetype = "dashed",
             color = "#00A68A", linewidth = 1) +
  facet_wrap(~parameter_label, scales = "free_x", ncol = 1, labeller = label_parsed) +
  transition_manual(frame) +
  labs(title = "Traceplot up to Iteration {current_frame}",
       x = "Iteration", y = "Parameter Value") +
  theme_dark_site()

animate(anim3, fps = 10, duration = 10, width = 800, height = 600,
        bg = BG, renderer = gifski_renderer())
anim_save("images/trace_mcmc_animation.gif")

# ── 4. Multi-chain MCMC path ──────────────────────────────────────────────────
message("Generating multichain_mcmc_animation.gif ...")
set.seed(123)
n_iter <- 1000
n_chains <- 4

chains_list <- map_dfr(1:n_chains, function(chain_id) {
  x <- y <- 0
  s <- tibble(x = x, y = y, iteration = 1, chain = chain_id)
  for (i in 2:n_iter) {
    x_prop <- rnorm(1, x, 0.4); y_prop <- rnorm(1, y, 0.4)
    if (runif(1) < target_density(x_prop, y_prop) / target_density(x, y)) {
      x <- x_prop; y <- y_prop
    }
    s <- s |> add_row(x = x, y = y, iteration = i, chain = chain_id)
  }
  s
})

anim4 <- ggplot(chains_list, aes(x = x, y = y, group = chain, color = as.factor(chain))) +
  geom_path(linewidth = 0.7) +
  geom_point(aes(x = x, y = y), size = 1.5) +
  geom_point(aes(x = true_param[1], y = true_param[2]),
             color = "#e2e2e2", size = 4, shape = 4, stroke = 2, inherit.aes = FALSE) +
  annotate("text", x = true_param[1] + 0.1, y = true_param[2],
           label = "True value", color = "#e2e2e2", hjust = 0) +
  transition_reveal(along = iteration) +
  coord_fixed() +
  scale_color_brewer(palette = "Set1", name = "Chain") +
  labs(title = "MCMC Chains Step: {round(frame_along, digits = 0)}",
       x = bquote(beta[0]), y = bquote(beta[1])) +
  theme_dark_site()

animate(anim4, fps = 25, duration = 20, width = 800, height = 600,
        bg = BG, renderer = gifski_renderer())
anim_save("images/multichain_mcmc_animation.gif")

# ── 5 & 6. Multi-chain posterior + traceplot ──────────────────────────────────
message("Generating multichain_posterior_mcmc_animation.gif ...")
samples_long_mc <- chains_list |>
  pivot_longer(cols = c(x, y), names_to = "parameter", values_to = "value") |>
  mutate(
    true_value = if_else(parameter == "x", true_param[1], true_param[2]),
    parameter_label = case_when(
      parameter == "x" ~ "beta[0]",
      parameter == "y" ~ "beta[1]"
    )
  )

samples_long_cumulative_mc <- samples_long_mc |>
  group_by(parameter, chain) |>
  group_split() |>
  map_dfr(function(df) {
    param <- unique(df$parameter); chain_id <- unique(df$chain)
    map_dfr(1:max(df$iteration), function(i) {
      df |> filter(iteration <= i) |> mutate(frame = i, parameter = param, chain = chain_id)
    })
  })

anim5 <- ggplot(samples_long_cumulative_mc,
                aes(x = value, fill = as.factor(chain))) +
  geom_histogram(binwidth = 0.4, color = "#e2e2e2", boundary = 0,
                 position = position_dodge(), alpha = 0.6) +
  geom_vline(aes(xintercept = true_value), linetype = "dashed",
             color = "#00A68A", linewidth = 1) +
  facet_wrap(~parameter_label, scales = "free_x", labeller = label_parsed, ncol = 1) +
  transition_manual(frame) +
  scale_fill_brewer(palette = "Set1", name = "Chain") +
  labs(title = "Cumulative Posterior up to Iteration {current_frame}",
       x = "Parameter Value", y = "Frequency") +
  theme_dark_site()

animate(anim5, fps = 25, duration = 20, width = 800, height = 600,
        bg = BG, renderer = gifski_renderer())
anim_save("images/multichain_posterior_mcmc_animation.gif")

message("Generating multichain_trace_mcmc_animation.gif ...")
anim6 <- ggplot(samples_long_cumulative_mc,
                aes(x = iteration, y = value, color = as.factor(chain))) +
  geom_line(linewidth = 0.7) +
  geom_hline(aes(yintercept = true_value), linetype = "dashed",
             color = "#00A68A", linewidth = 1) +
  facet_wrap(~parameter_label, scales = "free_y", labeller = label_parsed, ncol = 1) +
  transition_manual(frame) +
  scale_color_brewer(palette = "Set1", name = "Chain") +
  labs(title = "Traceplot up to Iteration {current_frame}",
       x = "Iteration", y = "Parameter Value") +
  theme_dark_site()

animate(anim6, fps = 25, duration = 20, width = 800, height = 600,
        bg = BG, renderer = gifski_renderer())
anim_save("images/multichain_trace_mcmc_animation.gif")

message("All GIFs regenerated successfully.")
