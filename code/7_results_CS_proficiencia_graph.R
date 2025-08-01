library(tidyverse)
library(did)
library(furrr)
library(future)

# Setup do paralelismo externo: um outcome por núcleo
plan(multisession, workers = 2)

rm(list=ls())
gc()


f<-function(subject){
  # Load the corresponding ATT results
  load(paste0("results/atts_", subject, "_cov.RData"))  # loads object named `att_cov`
  
  # Estimate dynamic effects
  ag <- aggte(att_cov, type = "dynamic", min_e = -5, max_e = 5)
  
  # Prepare data frame
  dyn_df <- data.frame(
    period = ag$egt,
    att = ag$att.egt,
    se = ag$se.egt
  )
  
  # Confidence intervals
  dyn_df$ci_lower <- dyn_df$att - 1.96 * dyn_df$se
  dyn_df$ci_upper <- dyn_df$att + 1.96 * dyn_df$se
  
  # Pre/post treatment classification
  dyn_df$period_type <- ifelse(dyn_df$period < 0, "Pre", "Post")
  
  # Create plot
  p <- ggplot(dyn_df, aes(x = as.factor(period), y = att, color = period_type, fill = period_type)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    scale_color_manual(values = c("Pre" = "#2166ac", "Post" = "#b2182b")) +
    scale_fill_manual(values = c("Pre" = "#2166ac", "Post" = "#b2182b")) +
    labs(
      x = "Periods to Treatment",
      y = "Average Treatment Effect"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 13)
    )+
    ylim(-0.2, 0.6)

  
  # Save plot
  ggsave(filename = paste0("results/graph_", subject, ".png"), plot = p, width = 8, height = 6)
}
future_map(c("mat","lp"), f)
