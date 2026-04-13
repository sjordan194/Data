library(readxl)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(ggstats)
library(ggtext)
library(see)
library(RColorBrewer)
library(showtext)
library(coin)
font_add_google("Roboto", "roboto")
showtext_auto()
#importing the data
international_gpa <- read_excel("C:/Users/jordan.194/OneDrive - The Ohio State University/Data/1258_International_GPA.xlsx")
View(international_gpa)
glimpse(international_gpa)
#renaming columns for ease of use
international_gpa <- international_gpa %>% 
  janitor::clean_names()
#converting TOEFL scores to numeric
international_gpa <- international_gpa %>% 
  mutate(across(starts_with("toefl"), as.numeric ))
#Converting test scores to a standard z-score.
international_gpa <- international_gpa %>%
  mutate(
    toefl_z    = scale(toefl_totli),
    ielts_z   = scale(ielts_scrob),
    duolingo_z = scale(duolng_ovrl)
  )
international_gpa %>% 
  select(toefl_z, ielts_z, duolingo_z) %>% 
  pivot_longer(everything(), names_to = "English test", values_to = "z_score") %>% 
  ggplot(aes(x = z_score, fill = `English test`)) + 
  geom_histogram(alpha = 0.6) + 
  labs(title = "Standardized English Test Score Distributions",
       x = "Z-Score",
       y = "Frequency") +
  theme_classic()

scores_long <- international_gpa %>% 
  select(cum_gpa, toefl_z, ielts_z, duolingo_z) %>% 
  pivot_longer(
    cols = c(toefl_z, ielts_z, duolingo_z),
    names_to = "English Test",
    values_to = "z_score"
  )
ggplot(scores_long, aes(x = z_score)) + 
  geom_density(fill = "steelblue", alpha = 0.7) + 
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) + 
  facet_wrap(~ `English Test`, ncol = 1) + 
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2), 
                      labels = c("Far below Average", "Below Average", "Average", "Above Average", "Far Above Average")) + 
  labs(title = "Most Students Score Near the Average on English Tests",
       subtitle = "Scores are shown relative to each test's average", 
       x = "Relative Performance", 
       y = "Distribution of Students") + 
  theme_classic()

#Creating scatter plots to visualize relationships between English test scores and GPA
ggplot(scores_long, aes(x = z_score, y = cum_gpa)) + 
  annotate(
    "rect", xmin = -Inf, xmax = Inf, 
    ymin = 0, ymax = 2.99, alpha = 0.07, fill = "firebrick"
  ) + 
  geom_point(alpha = 0.4, size = 1.5, 
             position = position_jitter(width = 0.15, height = 0.02)) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.0, color = "gray40") + 
  annotate(
    "label",
    x = min(scores_long$z_score, na.rm = TRUE),
    y = 1.5,
    label = "Shaded area indicates\ncommon academic risk\nthreshold (GPA < 3.0)",
    hjust = 0,
    vjust = 1.0, 
    size = 3.1,
    color = "gray30", fill = alpha("white", 0.5), label.size = 0
  ) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2), 
                     labels = c("Well below\naverage", "Below\naverage", "Average", "Above\naverage", "Well\nabove\naverage")) +
  labs(title = "English Proficiency Relative to Cohort Average\nand First-Semester GPA", 
       x = "Standardized English Test Score (Z-Score)",
       y = "Cumulative GPA") +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    plot.title = element_text(size = 16, face = "bold", 
                                  hjust = 0, lineheight = 1.1)
  )

#Creating plot where low GPA plots are highlighted.
ggplot(scores_long, aes(x= z_score, y = cum_gpa)) + 
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = 0, ymax = 2.99, alpha = 0.05, fill = "gray20") + 
  geom_point(aes(fill = cum_gpa < 3.0), 
             shape = 21, size = 3.5, color = "white", stroke = 0.3, 
             alpha = 0.6, 
             position = position_jitter(width = 0.15, height = 0.02)) + 
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "gray60", 
                               guide = "none")) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.0, color = "black", size = 0.8) + 
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2), 
                     labels = c("Well below\naverage", "Below\naverage", "Average", "Above\naverage", "Well\nabove\naverage")) + 
  scale_y_continuous(breaks = seq(0, 4, by = 0.5), limits = c(0, 4)) +
  labs(title = "English Proficiency Relative to Cohort Average\nand First-Semester GPA",
       x = "Standardized English Test Score (Z-Score)", 
       y = "Cumulative GPA", 
       caption = "Note: Red points indicate academic risk (GPA < 3.0)") + 
  theme_minimal(base_family = "roboto") + 
  theme(panel.grid.major.x  = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(margin = ggplot2:: margin(r = 20)), 
        plot.caption = element_text(hjust = 0, face = "italic", color = "gray30"),
        plot.title.position = "panel", 
        plot.title = element_text(size = 16, face = "bold", 
                                  hjust = 0, lineheight = 1.1), 
        axis.title = element_text(face = "bold", size = 16, lineheight = 1.2, 
                                  margin = ggplot2:: margin(b = 15)), 
        legend.position = "none")

ggsave("ESL GPA Scatterplot.png", width = 8, height = 6, dpi = 300)
ggsave("ESL GPA Scatterplot_with_RL.png", width = 8, height = 6, dpi = 300)

spearman_test(z_score ~ cum_gpa, data = scores_long)
cor.test(scores_long$z_score, scores_long$cum_gpa, method = "spearman", exact = FALSE)
#rho 0.1265394

zero_gpa <- scores_long %>% 
  filter(cum_gpa == 0)
nrow(zero_gpa)         
