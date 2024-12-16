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

#----------------------------------------------------------------------------------------------------------------------------------------------

# LOAD DATA

#Set the path in your computer the data is located
setwd("C:/Users/Lucas Ugliara/Dropbox/Doutorado/Doc Sanduíche/Writing - PICs/Data")

#Replace 
d = read_excel("data_pics_remote_contraction.xlsx", sheet = "Sheet1") %>%
  clean_names() %>%
  mutate(
    mu_id = as.factor(mu_id),
    condition = as.factor(condition),
    time = as.factor(time),
    participant = as.factor(participant))

# Calculate mean - All variables
d_mean = d %>% group_by(participant, condition, time) %>%
  summarise(
    deltaf = mean(deltaf, na.rm = TRUE),
    dr = mean(dr, na.rm = TRUE), 
    rt = mean(rt, na.rm = TRUE),
    bhnall = mean(bhnall, na.rm = TRUE),
    attenuationall = mean(attenuationall, na.rm = TRUE),
    drall = mean(drall, na.rm = TRUE),
    rtall = mean(rtall, na.rm = TRUE),
  )

#----------------------------------------------------------------------------------------------------------------------------------------------

#[1] DELTA F ANALYSIS

#[1.A] Identify the best model - Delta F
fit_df1 <- lmer(deltaf ~ 1 + as.factor(condition)*as.factor(time) +(1 | participant/mu_id), data = d, REML = FALSE, 
                control = lmerControl(optimizer ="Nelder_Mead"))
fit_df2 <- lmer(deltaf ~ rt + as.factor(condition)*as.factor(time) + (1| participant/mu_id), data = d)
fit_df3 <- lmer(deltaf ~ rt + dr + as.factor(condition)*as.factor(time) + (1|participant/mu_id), data = d)

#[1.B] Check for best model - Delta F
anova(fit_df1,fit_df2,fit_df3)

#[1.C] Set best model - Delta F
fit_df <- fit_df3

#[1.D] Model diagnostics - Delta F
qqPlot(residuals(fit_df))
summary(fit_df)

#[1.E] Confit - Delta F
anovafit_df <- anova(fit_df)
effectsize::omega_squared(anovafit_df)
anovafit_df

#[1.F] Mean difference and Effect size - Delta F
emm <- emmeans(fit_df, pairwise ~ time|condition, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_df, ~ time|condition, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_df), edf = df.residual(fit_df))

emm <- emmeans(fit_df, pairwise ~ condition|time, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_df, ~ condition|time, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_df), edf = df.residual(fit_df))

#[1.G] Refgrid - Delta F
(refgrid <- list (time=c("pre","post"), condition=c("control","4015","4030","8015")))
mar_deltaf <- emmip(fit_df, ~ as.factor(time)|as.factor(condition), at = refgrid, CIs = T, plotit = F)
mar_deltaf

#[1.H] Plot - Delta F
condition.labs <- c("control" = "Control", "4015" = "40%15s", "4030" = "40%30s", "8015" = "80%15s")
time.labs <- c ("pre" = "Before", "post" = "After")

ggplot(data = d_mean, aes(x = time, y = deltaf)) +
  geom_point(data = d %>% filter(time == 'pre'), aes(x = time, y = deltaf),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_point(data = d %>% filter(time == 'post'), aes(x = time, y = deltaf),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_jitter(width = 0.0, alpha = 1, size = 4, shape = "circle", aes(colour = participant)) +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = d_mean, aes(x = time, y = deltaf, group = participant),color="gray5", size = .75, alpha = .25) +
  geom_errorbar(data = mar_deltaf %>% filter(time == 'pre'), aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = 0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_deltaf %>% filter(time == 'pre'), aes(x = time, y = yvar), 
             position = position_nudge(x = 0.3), size = 4, colour = "black") +
  geom_errorbar(data = mar_deltaf %>% filter(time == 'post'), aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_deltaf %>% filter(time == 'post'), aes(x = time, y = yvar), 
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
emm <- emmeans(fit_df, pairwise ~ time|condition)
summary(emm)
ci_95 <- confint(emm, level = .95)
ci_90 <- confint(emm, level = .90)

# MD 4015
md  <- ci_95$contrasts$estimate[1]
lower_90 <- ci_90$contrasts$lower.CL[1]
upper_90 <- ci_90$contrasts$upper.CL[1]
lower_95 <- ci_95$contrasts$lower.CL[1]
upper_95 <- ci_95$contrasts$upper.CL[1]
condition <- '4015'

df_4015 <- cbind(condition, md,lower_90,upper_90,lower_95,upper_95)

# MD 4030
md  <- ci_95$contrasts$estimate[2]
lower_90 <- ci_90$contrasts$lower.CL[2]
upper_90 <- ci_90$contrasts$upper.CL[2]
lower_95 <- ci_95$contrasts$lower.CL[2]
upper_95 <- ci_95$contrasts$upper.CL[2]
condition <- '4030'

df_4030 <- cbind(condition, md,lower_90,upper_90,lower_95,upper_95)

# MD 8015
md  <- ci_95$contrasts$estimate[3]
lower_90 <- ci_90$contrasts$lower.CL[3]
upper_90 <- ci_90$contrasts$upper.CL[3]
lower_95 <- ci_95$contrasts$lower.CL[3]
upper_95 <- ci_95$contrasts$upper.CL[3]
condition <- '8015'

df_8015 <- cbind(condition, md,lower_90,upper_90,lower_95,upper_95)

# MD control
md  <- ci_95$contrasts$estimate[4]
lower_90 <- ci_90$contrasts$lower.CL[4]
upper_90 <- ci_90$contrasts$upper.CL[4]
lower_95 <- ci_95$contrasts$lower.CL[4]
upper_95 <- ci_95$contrasts$upper.CL[4]
condition <- 'control'

df_control <- cbind(condition, md,lower_90,upper_90,lower_95,upper_95)

# Join
df <- rbind(df_control, df_4015, df_4030, df_8015) %>%
  as_tibble() %>%
  mutate(
    md = as.numeric(md),
    lower_90 = as.numeric(lower_90),
    lower_95 = as.numeric(lower_95),
    upper_90 = as.numeric(upper_90),
    upper_95 = as.numeric(upper_95),
    condition = recode_factor(condition, 'control' = 'Control', '4015' = '40%15s', '4030' = '40%30s', '8015' = '80%15s')
  )

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
ggsave("combined_plot.png", deltaf_cowplot, width = 14, height = 16, dpi = 600)
#----------------------------------------------------------------------------------------------------------------------------------------------

#[2] BRACE HEIGHT NORMALIZED ANALYSIS

#[2.A] Identify the best model - Brace height normalized
fit_bhnall1 <- lmer(bhnall ~ 1 + as.factor(condition)*as.factor(time) +(1 | participant/mu_id), data = d, REML = FALSE, 
                control = lmerControl(optimizer ="Nelder_Mead"))
fit_bhnall2 <- lmer(bhnall ~ rtall + as.factor(condition)*as.factor(time) + (1| participant/mu_id), data = d)
fit_bhnall3 <- lmer(bhnall ~ rtall + drall + as.factor(condition)*as.factor(time) + (1| participant/mu_id), data = d)

#[2.B] Check for best model - Brace height normalized
anova(fit_bhnall1,fit_bhnall2, fit_bhnall3)

#[2.C] Set best model - Brace height normalized
fit_bhnall <- fit_bhnall2

#[2.D] Model diagnostics - Brace height normalized
qqPlot(residuals(fit_bhnall))
summary(fit_bhnall)

#[2.E] Confint - Brace height normalized
anovafit_bhnall <- anova(fit_bhnall)
effectsize::omega_squared(anovafit_bhnall)
anovafit_bhnall

#[2.F] Mean difference and Effect size - Brace height normalized
emm <- emmeans(fit_bhnall, pairwise ~ time|condition, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_bhnall, ~ time|condition, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_bhnall), edf = df.residual(fit_bhnall))

emm <- emmeans(fit_bhnall, pairwise ~ condition|time, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_bhnall, ~ condition|time, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_bhnall), edf = df.residual(fit_bhnall))

#[2.G] Refgrid - Brace height normalized
(refgrid <- list (time=c("pre","post"), condition=c("control","4015","4030","8015")))
mar_bhnall <- emmip(fit_bhnall, ~ as.factor(time)|as.factor(condition), at = refgrid, CIs = T, plotit = F)
mar_bhnall

#[2.H] Plot - Brace height normalized
condition.labs <- c("control" = "Control", "4015" = "40%15s", "4030" = "40%30s", "8015" = "80%15s")
time.labs <- c ("pre" = "Before", "post" = "After")

ggplot(data = d_mean, aes(x = time, y = bhnall)) +
  geom_point(data = d %>% filter(time == 'pre'), aes(x = time, y = bhnall),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_point(data = d %>% filter(time == 'post'), aes(x = time, y = bhnall),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_jitter(width = 0.0, alpha = 1, size = 4, shape = "circle", aes(colour = participant)) +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = d_mean, aes(x = time, y = bhnall, group = participant),color="gray5", size = .75, alpha = .25) +
  geom_errorbar(data = mar_bhnall %>% filter(time == 'pre'), aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = 0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_bhnall %>% filter(time == 'pre'), aes(x = time, y = yvar), 
             position = position_nudge(x = 0.3), size = 4, colour = "black") +
  geom_errorbar(data = mar_bhnall %>% filter(time == 'post'), aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_bhnall %>% filter(time == 'post'), aes(x = time, y = yvar), 
             position = position_nudge(x = -0.3), size = 4, colour = "black") +
  #geom_line(data = d_mean, aes(x = time, y = bhnall, group = participant),
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
ggsave(file = "fig4_bh.png", units="in", width = 10, height = 6, dpi = 600)

#----------------------------------------------------------------------------------------------------------------------------------------------

#[3] ATTENUATION SLOPE ANALYSIS

#[3.A] Identify the best model - Attenuation
fit_attenuationall1 <- lmer(attenuationall ~ 1 + as.factor(condition)*as.factor(time) + (1 | participant/mu_id), data = d, REML = FALSE, 
                            control = lmerControl(optimizer = "Nelder_Mead"))
fit_attenuationall2 <- lmer(attenuationall ~ rtall + as.factor(condition)*as.factor(time) + (1 | participant/mu_id), data = d)
fit_attenuationall3 <- lmer(attenuationall ~ rtall + drall + as.factor(condition)*as.factor(time) + (1 | participant/mu_id), data = d)

#[3.B] Check for best model - Attenuation slope
anova(fit_attenuationall1, fit_attenuationall2, fit_attenuationall3)

#[3.C] Set best model - Attenuation slope
fit_attenuationall <- fit_attenuationall2

#[3.D] Model diagnostics - Attenuation slope
qqPlot(residuals(fit_attenuationall))
summary(fit_attenuationall)

#[3.E] Confint - Attenuation slope
anovafit_attenuationall <- anova(fit_attenuationall)
effectsize::omega_squared(anovafit_attenuationall)
anovafit_attenuationall

#[3.F] Mean difference and Effect size - Attenuation slope
emm <- emmeans(fit_attenuationall, pairwise ~ time|condition, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_attenuationall, ~ time|condition, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_attenuationall), edf = df.residual(fit_attenuationall))

emm <- emmeans(fit_attenuationall, pairwise ~ condition|time, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_attenuationall, ~ condition|time, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_attenuationall), edf = df.residual(fit_attenuationall))

#[3.G] Refgrid - Attenuation slope
(refgrid <- list(time = c("pre", "post"), condition = c("control", "4015", "4030", "8015")))
mar_attenuationall <- emmip(fit_attenuationall, ~ as.factor(time)|as.factor(condition), at = refgrid, CIs = TRUE, plotit = FALSE)
mar_attenuationall

#[3.H] Plot - attenuationall
condition.labs <- c("control" = "Control", "4015" = "40%15s", "4030" = "40%30s", "8015" = "80%15s")
time.labs <- c ("pre" = "Before", "post" = "After")

ggplot(data = d_mean, aes(x = time, y = attenuationall)) +
  geom_point(data = d %>% filter(time == 'pre'), aes(x = time, y = attenuationall),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_point(data = d %>% filter(time == 'post'), aes(x = time, y = attenuationall),
             position = position_jitter(width = 0.3), size = 2, color = "gray90") +
  geom_jitter(width = 0.0, alpha = 1, size = 4, shape = "circle", aes(colour = participant)) +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = d_mean, aes(x = time, y = attenuationall, group = participant),color="gray5", size = .75, alpha = .25) +
  geom_errorbar(data = mar_attenuationall %>% filter(time == 'pre'), aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = 0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_attenuationall %>% filter(time == 'pre'), aes(x = time, y = yvar), 
             position = position_nudge(x = 0.3), size = 4, colour = "black") +
  geom_errorbar(data = mar_attenuationall %>% filter(time == 'post'), aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 1.5) +
  geom_point(data = mar_attenuationall %>% filter(time == 'post'), aes(x = time, y = yvar), 
             position = position_nudge(x = -0.3), size = 4, colour = "black") +
  #geom_line(data = d_mean, aes(x = time, y = attenuationall, group = participant),
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
  labs(y = "Attenuation slope (pps/MVT)", x = "Time") +
  facet_grid(~factor(condition, levels=c('control', '4015', '4030', '8015'), labels = c('Control', '40%15s', '40%30s', '80%15s'))) +
  theme(strip.background = element_rect(fill="gray95", size=1, color="black")) -> plot_attenuationall
plot_attenuationall
ggsave(file = "fig4_att.png", units="in", width = 10, height = 6, dpi = 600)

#----------------------------------------------------------------------------------------------------------------------------------------------

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
  draw_label("Attenuation slope (pps/MVT)", 
             x = 0.1, y = 0.30, angle = 90, fontface = "bold", size = 28, hjust = 0.5) + # Align to bottom graph
  draw_label("Time", 
             x = 0.5, y = 0.08, fontface = "bold", size = 28, hjust = 0.5) # Adjust X-axis label position

# Display the final figure
final_plot

# Save the combined plot
ggsave("combined_fig4.png", plot = final_plot, units = "in", width = 14, height = 16, dpi = 600, device = "png", bg = "white")
#----------------------------------------------------------------------------------------------------------------------------------------------

#[4] DISCHARGE RATE ANALYSIS

#[4.A] Identify the best model - Discharge rate
fit_mdrsrvall1 <- lmer(mdrsrvall ~ 1 + as.factor(condition)*as.factor(time) + (1 | participant/mu_id), data = d, REML = FALSE, 
                       control = lmerControl(optimizer = "Nelder_Mead"))
fit_mdrsrvall2 <- lmer(mdrsrvall ~ rtall + as.factor(condition)*as.factor(time) + (1 | participant/mu_id), data = d)

#[4.B] Check for best model - Discharge rate
anova(fit_mdrsrvall1, fit_mdrsrvall2)

#[4.C] Set best model - Discharge rate
fit_mdrsrvall <- fit_mdrsrvall2

#[4.D] Model diagnostics - Discharge rate
qqPlot(residuals(fit_mdrsrvall))
summary(fit_mdrsrvall)

#[4.E] Confint - Discharge rate
anovafit_mdrsrvall <- anova(fit_mdrsrvall)
effectsize::omega_squared(anovafit_mdrsrvall)
anovafit_mdrsrvall

#[4.F] Mean difference and Effect size - Discharge rate
emm <- emmeans(fit_mdrsrvall, pairwise ~ time|condition, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_mdrsrvall, ~ time|condition, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_mdrsrvall), edf = df.residual(fit_mdrsrvall))

emm <- emmeans(fit_mdrsrvall, pairwise ~ condition|time, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_mdrsrvall, ~ condition|time, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_mdrsrvall), edf = df.residual(fit_mdrsrvall))

#[4.G] Refgrid - Discharge rate
(refgrid <- list(time = c("pre", "post"), condition = c("control", "4015", "4030", "8015")))
mar_mdrsrvall <- emmip(fit_mdrsrvall, ~ as.factor(time)|as.factor(condition), at = refgrid, CIs = TRUE, plotit = FALSE)
mar_mdrsrvall

#----------------------------------------------------------------------------------------------------------------------------------------------

#[5] RECRUITMENT THRESHOLD ANALYSIS

#[5.A] Identify the best model - Recruitment threshold
fit_rtall1 <- lmer(rtall ~ 1 + as.factor(condition)*as.factor(time) + (1 | participant/mu_id), data = d, REML = FALSE, 
                   control = lmerControl(optimizer = "Nelder_Mead"))
fit_rtall2 <- lmer(rtall ~ mdrsrvall + as.factor(condition)*as.factor(time) + (1 | participant/mu_id), data = d)

#[5.B] Check for best model - Recruitment threshold
anova(fit_rtall1, fit_rtall2)

#[5.C] Set best model - Recruitment threshold
fit_rtall <- fit_rtall2

#[5.D] Model diagnostics - Recruitment threshold
qqPlot(residuals(fit_rtall))
summary(fit_rtall)

#[5.E] Confint - Recruitment threshold
anovafit_rtall <- anova(fit_rtall)
effectsize::omega_squared(anovafit_rtall)
anovafit_rtall

#[5.F] Mean difference and Effect size - Recruitment threshold
emm <- emmeans(fit_rtall, pairwise ~ time|condition, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_rtall, ~ time|condition, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_rtall), edf = df.residual(fit_rtall))

emm <- emmeans(fit_rtall, pairwise ~ condition|time, adjust = "none")
confint(emm)
summary(emm)
conditional_effect <- emmeans(fit_rtall, ~ condition|time, adjust = "none")
eff_size(conditional_effect, sigma = sigma(fit_rtall), edf = df.residual(fit_rtall))

#[5.G] Refgrid - Recruitment threshold
(refgrid <- list(time = c("pre", "post"), condition = c("control", "4015", "4030", "8015")))
mar_rtall <- emmip(fit_rtall, ~ as.factor(time)|as.factor(condition), at = refgrid, CIs = TRUE, plotit = FALSE)
mar_rtall
