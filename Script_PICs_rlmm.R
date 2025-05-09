#Load library

library(readxl)
library(naniar)
library(visdat)
library(dplyr)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(janitor)
library(cowplot)
library(emmeans)
library(lme4)
library(car)
library(rmcorr)
library(psycho)
library(sjstats)
library(pwr)
library(viridis)
library(tidyr)
library(ggplot2)
library(MuMIn)
library(merTools)
library(broom.mixed)
library(ggpp)
library(writexl)
library(optimx)
library(misty)
library(magrittr)
library(robustlmm)

#----------------------------------------------------------------------------------------------------------------------------------------------

# LOAD DATA

#Set the path in your computer the data is located
setwd("C:/Users/name/folderd")

#Replace 
d = read_excel("Data_pics_remote_contraction.xlsx", sheet = "Sheet1") %>%
  clean_names() %>%
  mutate(
    mu_id = as.factor(mu_id),
    condition = as.factor(condition),
    time = as.factor(time),
    participant = as.factor(participant))

#----------------------------------------------------------------------------------------------------------------------------------------------
#[1] DELTA F ANALYSIS / Robust lmm

d$condition <- factor(d$condition, levels = c("control", "4015", "4030", "8015"))
fit_df <- rlmer(deltaf ~ 1 + rt + dr + as.factor(time)*as.factor(condition) + (1|participant/mu_id), data = d)

# Create a tidy summary of fixed effects
tidy_output <- tidy(fit_df, effects = "fixed", conf.int = TRUE)

# Extract only main effects and interactions
main_effects_interactions <- tidy_output 

# View the main effects and interactions
print(main_effects_interactions)

# EMMean differences
emm <- emmeans(fit_df, pairwise ~ condition|time, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

emm <- emmeans(fit_df, pairwise ~ time|condition, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

# Get estimated marginal means and pairwise contrasts
emm <- emmeans(fit_df, pairwise ~ time | condition, adjust = "none")

# Extract contrasts for time (pre vs post) within each condition
contrast_df <- summary(emm$contrasts)

# View the results
print(contrast_df)

# Extract residual standard deviation from rlmer model
sigma_val <- summary(fit_df)$sigma

# Add Cohen's d to the contrast table
contrast_df$cohen_d <- contrast_df$estimate / sigma_val
contrast_df$d_lower <- contrast_df$cohen_d - 1.96 * (contrast_df$SE / sigma_val)
contrast_df$d_upper <- contrast_df$cohen_d + 1.96 * (contrast_df$SE / sigma_val)

# View final table with effect sizes
print(contrast_df)

#----------------------------------------------------------------------------------------------------------------------------------------------

#[1.H] Plot - Delta F
d_summary <- d %>%
  mutate(condition = factor(condition, levels = c('control', '4015', '4030', '8015'))) %>%
  group_by(participant, condition, time) %>%
  summarize(deltaf = mean(deltaf, na.rm = TRUE), .groups = "drop")

emm_df <- as.data.frame(emm$emmeans)
mar_deltaf <- emm_df %>%
  mutate(
    condition = factor(condition, levels = c("control", "4015", "4030", "8015")),
    time = factor(time, levels = c("pre", "post"))
  )

ggplot(data = d, aes(x = time, y = deltaf)) +
  geom_point(data = d %>% filter(time == 'pre'), aes(x = time, y = deltaf),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_point(data = d %>% filter(time == 'post'), aes(x = time, y = deltaf),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_jitter(data = d_summary, width = 0.0, alpha = 1, size = 4, shape = "circle", aes(x = time, y = deltaf, colour = participant)) +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = d_summary, aes(x = time, y = deltaf, group = participant),color="gray5", size = .75, alpha = .25) +
  geom_errorbar(data = mar_deltaf %>% filter(time == 'pre'), aes(ymin = asymp.LCL, ymax = asymp.UCL, y = emmean),
                position = position_nudge(x = 0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_deltaf %>% filter(time == 'pre'), aes(x = time, y = emmean), 
             position = position_nudge(x = 0.3), size = 4, colour = "black") +
  geom_errorbar(data = mar_deltaf %>% filter(time == 'post'), aes(ymin = asymp.LCL, ymax = asymp.UCL, y = emmean),
                position = position_nudge(x = -0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_deltaf %>% filter(time == 'post'), aes(x = time, y = emmean), 
             position = position_nudge(x = -0.3), size = 4, colour = "black") +
  #geom_line(data = d_mean, aes(x = time, y = deltaf, group = participant),
  #color='grey50', size = .5, alpha = .25, position = position_nudge(x = 0.0)) +
  #ylim(-2.0,10.0) +
  scale_x_discrete(limits=c("pre", "post"), breaks=c("pre", "post"), labels=c("Before", "After")) +
  theme(
    axis.text = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    strip.text.x = element_text(size = 26),
    strip.text.y = element_text(size = 222),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(y = "Δ F (pps)", x = "Time") +
  facet_grid(~factor(condition, levels=c('control', '4015', '4030', '8015'), labels = c('Control', '40%15s', '40%30s', '80%15s'))) +
  theme(strip.background = element_rect(fill="gray95", size=1, color="black")) -> plot_deltaf
plot_deltaf
ggsave(file = "fig3_deltaf.png", units="in", width = 10, height = 6, dpi = 600)

#[1.I] Plot Mean difference - Delta F

# Get estimated marginal means and pairwise contrasts
emm <- emmeans(fit_df, pairwise ~ time | condition, adjust = "none")

# Extract the contrast results as a data frame
contrast_results <- as.data.frame(emm$contrasts)

# Calculate confidence intervals manually using SE
df <- data.frame(
  condition = c('control', '4015', '4030', '8015'),
  md = contrast_results$estimate,
  lower_95 = contrast_results$estimate - qt(0.975, contrast_results$df) * contrast_results$SE,
  upper_95 = contrast_results$estimate + qt(0.975, contrast_results$df) * contrast_results$SE,
  lower_90 = contrast_results$estimate - qt(0.95, contrast_results$df) * contrast_results$SE,
  upper_90 = contrast_results$estimate + qt(0.95, contrast_results$df) * contrast_results$SE
) %>%
  mutate(
    condition = recode_factor(condition,
                            'control' = 'Control',
                            '4015' = '40%15s',
                            '4030' = '40%30s',
                            '8015' = '80%15s')
  )

# Create the plot
ggplot(data = df, aes(x = condition, y = md)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), size = 0.4, width = 0) +
  geom_errorbar(aes(ymin = lower_90, ymax = upper_90), size = 1.2, width = 0) +
  theme_bw(base_size = 12) +
  labs(x = "Condition", y = "ΔF After - Before Mean Difference (pps)") +
  geom_hline(yintercept=0, linetype="solid",color = "red", size=0.25) +
  theme(
    axis.text = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 22),
    strip.text.x = element_text(size = 22),
    strip.text.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) -> plot_md_deltaf
#scale_y_continuous(limits = c(-2.5,0.5), n.breaks = 6) +
#facet_grid(~"ΔF") 

plot_md_deltaf

#cowplot delta F 20% (unmatched + matched motor units)

deltaf_cowplot <- plot_grid(
  plot_deltaf + theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
  plot_md_deltaf + theme(plot.margin = unit(c(1, 1, 1, 1), "lines")),
  labels = c("A", "B"), 
  label_size = 28, 
  ncol = 1, 
  rel_heights = c(2, 1),  # A is twice the height of B
  label_x = 0.05,  # Center the labels
  label_y = c(1, 1.1)  # Adjust the y position for label B
)

print(deltaf_cowplot)
ggsave("combined_plot_rlmm.png", deltaf_cowplot, width = 14, height = 16, dpi = 600)
#----------------------------------------------------------------------------------------------------------------------------------------------

#[2] BRACE HEIGHT NORMALIZED ANALYSIS / Robust lmm

d$condition <- factor(d$condition, levels = c("control", "4015", "4030", "8015"))
fit_bh <- rlmer(bhnall ~ 1 + rtall + drall + as.factor(time)*as.factor(condition) + (1|participant/mu_id), data = d)

# Create a tidy summary of fixed effects
tidy_output <- tidy(fit_bh, effects = "fixed", conf.int = TRUE)

# Extract only main effects and interactions
main_effects_interactions <- tidy_output 

# View the main effects and interactions
print(main_effects_interactions)

# EMMean differences
emm <- emmeans(fit_bh, pairwise ~ condition|time, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

emm <- emmeans(fit_bh, pairwise ~ time|condition, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

# Get estimated marginal means and pairwise contrasts
emm <- emmeans(fit_bh, pairwise ~ time | condition, adjust = "none")

# Extract contrasts for time (pre vs post) within each condition
contrast_bh <- summary(emm$contrasts)

# View the results
print(contrast_bh)

# Extract residual standard deviation from rlmer model
sigma_val <- summary(fit_bh)$sigma

# Add Cohen's d to the contrast table
contrast_bh$cohen_d <- contrast_bh$estimate / sigma_val
contrast_bh$d_lower <- contrast_bh$cohen_d - 1.96 * (contrast_bh$SE / sigma_val)
contrast_bh$d_upper <- contrast_bh$cohen_d + 1.96 * (contrast_bh$SE / sigma_val)

# View final table with effect sizes
print(contrast_bh)

#----------------------------------------------------------------------------------------------------------------------------------------------

#[2.H] Plot - Brace height normalized
d_summary <- d %>%
  mutate(condition = factor(condition, levels = c('control', '4015', '4030', '8015'))) %>%
  group_by(participant, condition, time) %>%
  summarize(bhnall = mean(bhnall, na.rm = TRUE), .groups = "drop")

emm_df <- as.data.frame(emm$emmeans)
mar_bhnall <- emm_df %>%
  mutate(
    condition = factor(condition, levels = c("control", "4015", "4030", "8015")),
    time = factor(time, levels = c("pre", "post"))
  )

ggplot(data = d, aes(x = time, y = bhnall)) +
  geom_point(data = d %>% filter(time == 'pre'), aes(x = time, y = bhnall),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_point(data = d %>% filter(time == 'post'), aes(x = time, y = bhnall),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_jitter(data = d_summary, width = 0.0, alpha = 1, size = 4, shape = "circle", aes(x = time, y = bhnall, colour = participant)) +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = d_summary, aes(x = time, y = bhnall, group = participant),color="gray5", size = .75, alpha = .25) +
  geom_errorbar(data = mar_bhnall %>% filter(time == 'pre'), aes(ymin = asymp.LCL, ymax = asymp.UCL, y = emmean),
                position = position_nudge(x = 0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_bhnall %>% filter(time == 'pre'), aes(x = time, y = emmean), 
             position = position_nudge(x = 0.3), size = 4, colour = "black") +
  geom_errorbar(data = mar_bhnall %>% filter(time == 'post'), aes(ymin = asymp.LCL, ymax = asymp.UCL, y = emmean),
                position = position_nudge(x = -0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_bhnall %>% filter(time == 'post'), aes(x = time, y = emmean), 
             position = position_nudge(x = -0.3), size = 4, colour = "black") +
  #geom_line(data = d_summary, aes(x = time, y = bhnall, group = participant),
  #color='grey50', size = .5, alpha = .25, position = position_nudge(x = 0.0)) +
  #ylim(-2.0,10.0) +
  scale_x_discrete(limits=c("pre", "post"), breaks=c("pre", "post"), labels=c("Before", "After")) +
  theme(
    axis.text = element_text(size = 26),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    strip.text.x = element_text(size = 26),
    strip.text.y = element_text(size = 222),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(y = "Brace height (% rTri)", x = "Time") +
  facet_grid(~factor(condition, levels=c('control', '4015', '4030', '8015'), labels = c('Control', '40%15s', '40%30s', '80%15s'))) +
  theme(strip.background = element_rect(fill="gray95", size=1, color="black")) -> plot_bhnall
plot_bhnall
ggsave(file = "fig4_bhn_rlmm.png", units="in", width = 10, height = 6, dpi = 600)

#----------------------------------------------------------------------------------------------------------------------------------------------

#[3] ATTENUATION SLOPE ANALYSIS / Robust lmm

d$condition <- factor(d$condition, levels = c("control", "4015", "4030", "8015"))
fit_att <- rlmer(attenuationall ~ 1 + rtall + drall + as.factor(time)*as.factor(condition) + (1|participant/mu_id), data = d)

# Create a tidy summary of fixed effects
tidy_output <- tidy(fit_att, effects = "fixed", conf.int = TRUE)
print(tidy_output)

# Extract only main effects and interactions
main_effects_interactions <- tidy_output 

# View the main effects and interactions
print(main_effects_interactions)

# EMMean differences
emm <- emmeans(fit_att, pairwise ~ condition|time, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

emm <- emmeans(fit_att, pairwise ~ time|condition, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

# Get estimated marginal means and pairwise contrasts
emm <- emmeans(fit_att, pairwise ~ time | condition, adjust = "none")

# Extract contrasts for time (pre vs post) within each condition
contrast_att <- summary(emm$contrasts)

# View the results
print(contrast_att)

# Extract residual standard deviation from rlmer model
sigma_val <- summary(fit_att)$sigma

# Add Cohen's d to the contrast table
contrast_att$cohen_d <- contrast_att$estimate / sigma_val
contrast_att$d_lower <- contrast_att$cohen_d - 1.96 * (contrast_att$SE / sigma_val)
contrast_att$d_upper <- contrast_att$cohen_d + 1.96 * (contrast_att$SE / sigma_val)

# View final table with effect sizes
print(contrast_att)

#----------------------------------------------------------------------------------------------------------------------------------------------

#[3.H] Plot - attenuationall
d_summary <- d %>%
  mutate(condition = factor(condition, levels = c('control', '4015', '4030', '8015'))) %>%
  group_by(participant, condition, time) %>%
  summarize(attenuationall = mean(attenuationall, na.rm = TRUE), .groups = "drop")

emm_df <- as.data.frame(emm$emmeans)
mar_attenuationall <- emm_df %>%
  mutate(
    condition = factor(condition, levels = c("control", "4015", "4030", "8015")),
    time = factor(time, levels = c("pre", "post"))
  )

ggplot(data = d, aes(x = time, y = attenuationall)) +
  geom_point(data = d %>% filter(time == 'pre'), aes(x = time, y = attenuationall),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_point(data = d %>% filter(time == 'post'), aes(x = time, y = attenuationall),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_jitter(data = d_summary, width = 0.0, alpha = 1, size = 4, shape = "circle", aes(x = time, y = attenuationall, colour = participant)) +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = d_summary, aes(x = time, y = attenuationall, group = participant),color="gray5", size = .75, alpha = .25) +
  geom_errorbar(data = mar_attenuationall %>% filter(time == 'pre'), aes(ymin = asymp.LCL, ymax = asymp.UCL, y = emmean),
                position = position_nudge(x = 0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_attenuationall %>% filter(time == 'pre'), aes(x = time, y = emmean), 
             position = position_nudge(x = 0.3), size = 4, colour = "black") +
  geom_errorbar(data = mar_attenuationall %>% filter(time == 'post'), aes(ymin = asymp.LCL, ymax = asymp.UCL, y = emmean),
                position = position_nudge(x = -0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_attenuationall %>% filter(time == 'post'), aes(x = time, y = emmean), 
             position = position_nudge(x = -0.3), size = 4, colour = "black") +
  #geom_line(data = d_summary, aes(x = time, y = attenuationall, group = participant),
  #color='grey50', size = .5, alpha = .25, position = position_nudge(x = 0.0)) +
  #ylim(-2.0,10.0) +
  scale_x_discrete(limits=c("pre", "post"), breaks=c("pre", "post"), labels=c("Before", "After")) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    strip.text.x = element_text(size = 26),
    strip.text.y = element_text(size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  coord_cartesian(ylim = c(NA, 2)) +
  labs(y = "Attenuation slope (pps/%MVT)", x = "Time") +
  facet_grid(~factor(condition, levels=c('control', '4015', '4030', '8015'), labels = c('Control', '40%15s', '40%30s', '80%15s'))) +
  theme(strip.background = element_rect(fill="gray95", size=1, color="black")) -> plot_attenuationall
plot_attenuationall
ggsave(file = "fig5_att_rlmm.png", units="in", width = 8, height = 10, dpi = 600)

#Combine Brace height and Attenuation slope plots

# Adjusted versions of your plots (removing unnecessary axis labels)
brace_height_clean <- plot_bhnall +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(), # Remove duplicate Y-axis label
    plot.margin = margin(5, 5, 5, 40)
  )

attenuation_slope_clean <- plot_attenuationall +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), # Remove duplicate Y-axis label
    strip.text.x = element_blank(), # Remove condition labels from bottom plot
    plot.margin = margin(5, 5, 5, 40)
  )

# Combine plots into one vertical layout
combined_plot <- plot_grid(
  brace_height_clean, attenuation_slope_clean,
  ncol = 1, align = "v", axis = "lr", label_fontface = "bold"
)

# Add shared axis labels with cowplot
final_plot <- ggdraw() +
  draw_plot(combined_plot, 0.1, 0.1, 0.8, 0.85) + # Adjust margins to avoid overlapping
  draw_label("Brace height (% rTri)", 
             x = 0.1, y = 0.75, angle = 90, fontface = "bold", size = 28, hjust = 0.5) + # Align to top graph
  draw_label("Attenuation slope (pps/%MVT)", 
             x = 0.1, y = 0.30, angle = 90, fontface = "bold", size = 28, hjust = 0.5) + # Align to bottom graph
  draw_label("Time", 
             x = 0.5, y = 0.08, fontface = "bold", size = 28, hjust = 0.5) # Adjust X-axis label position

# Display the final figure
final_plot

# Save the combined plot
ggsave("combined_fig4_rlmmto2.png", plot = final_plot, units = "in", width = 14, height = 16, dpi = 600, device = "png", bg = "white")

#----------------------------------------------------------------------------------------------------------------------------------------------

#[4] DISCHARGE RATE ANALYSIS / Robust lmm

d$condition <- factor(d$condition, levels = c("control", "4015", "4030", "8015"))
fit_drall <- rlmer(drall ~ 1 + rtall + as.factor(time)*as.factor(condition) + (1|participant/mu_id), data = d)

# Create a tidy summary of fixed effects
tidy_output <- tidy(fit_drall, effects = "fixed", conf.int = TRUE)
print(tidy_output)

# Extract only main effects and interactions
main_effects_interactions <- tidy_output 

# View the main effects and interactions
print(main_effects_interactions)

# EMMean differences
emm <- emmeans(fit_drall, pairwise ~ condition|time, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

emm <- emmeans(fit_drall, pairwise ~ time|condition, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

# Get estimated marginal means and pairwise contrasts
emm <- emmeans(fit_drall, pairwise ~ time | condition, adjust = "none")

# Extract contrasts for time (pre vs post) within each condition
contrast_drall <- summary(emm$contrasts)

# View the results
print(contrast_drall)

# Extract residual standard deviation from rlmer model
sigma_val <- summary(fit_drall)$sigma

# Add Cohen's d to the contrast table
contrast_drall$cohen_d <- contrast_drall$estimate / sigma_val
contrast_drall$d_lower <- contrast_drall$cohen_d - 1.96 * (contrast_drall$SE / sigma_val)
contrast_drall$d_upper <- contrast_drall$cohen_d + 1.96 * (contrast_drall$SE / sigma_val)

# View final table with effect sizes
print(contrast_drall)

#----------------------------------------------------------------------------------------------------------------------------------------------

#[5] RECRUITMENT THRESHOLD ANALYSIS / Robust lmm

d$condition <- factor(d$condition, levels = c("control", "4015", "4030", "8015"))
fit_rtall <- rlmer(rtall ~ 1 + drall + as.factor(time)*as.factor(condition) + (1|participant/mu_id), data = d)

# Create a tidy summary of fixed effects
tidy_output <- tidy(fit_rtall, effects = "fixed", conf.int = TRUE)
print(tidy_output)

# Extract only main effects and interactions
main_effects_interactions <- tidy_output 

# View the main effects and interactions
print(main_effects_interactions)

# EMMean differences
emm <- emmeans(fit_rtall, pairwise ~ condition|time, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

emm <- emmeans(fit_rtall, pairwise ~ time|condition, adjust = "bonferroni",pbkrtest.limit = 100000)
confint(emm)
summary(emm)

# Get estimated marginal means and pairwise contrasts
emm <- emmeans(fit_rtall, pairwise ~ time | condition, adjust = "none")

# Extract contrasts for time (pre vs post) within each condition
contrast_rtall <- summary(emm$contrasts)

# View the results
print(contrast_rtall)

# Extract residual standard deviation from rlmer model
sigma_val <- summary(fit_rtall)$sigma

# Add Cohen's d to the contrast table
contrast_rtall$cohen_d <- contrast_rtall$estimate / sigma_val
contrast_rtall$d_lower <- contrast_rtall$cohen_d - 1.96 * (contrast_rtall$SE / sigma_val)
contrast_rtall$d_upper <- contrast_rtall$cohen_d + 1.96 * (contrast_rtall$SE / sigma_val)

# View final table with effect sizes
print(contrast_rtall)