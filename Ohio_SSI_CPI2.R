library(fredr)
library(tidyverse)
library(zoo)
library(showtext)
library(readr)
library(scales)
library(ggtext)
library(ggrepel)

font_add_google("Roboto", "roboto")
showtext_auto()

#Setting FREDR API key and pulling data
fredr_set_key("33523224c679890d7c128d53c2199011")
cpi_raw <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("2015-01-01")
)
#fixing the gap in CPI for October 2025.

cpi_data <- cpi_raw %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>%
  mutate(
    month = month(date),
    year = year(date),
    fiscal_year = if_else(month >= 7, year + 1, year)
  ) %>%
  group_by(fiscal_year) %>%
  summarize(avg_cpi = mean(value)) %>%
  mutate(
    cpi_rate = (avg_cpi - lag(avg_cpi)) / lag(avg_cpi)
  )

#Including Mid-west specific CPI data
midwest_cpi <- fredr(
  series_id = "CUUR0200SA0",
  observation_start = as.Date("2015-01-01")
)

midwest_cpi_data <- midwest_cpi %>%
  mutate(
    month = month(date),
    year = year(date),
    fiscal_year = if_else(month >= 7, year + 1, year)
  ) %>%
  group_by(fiscal_year) %>%
  summarize(mw_avg_cpi = mean(value)) %>%
  mutate(
    mw_cpi_rate = (mw_avg_cpi - lag(mw_avg_cpi)) / lag(mw_avg_cpi)
  )
#Adding the HEPI data from Commonfund institute.
hepi_data <- tibble(
  fiscal_year = 2016:2025,
  hepi_rate = c(
    0.015,
    0.030,
    0.026,
    0.030,
    0.019,
    0.027,
    0.052,
    0.040,
    0.034,
    0.036
  )
)

#Source: Ohio Legislative Service Commission Dec. 25.
ohio_ssi_data <- tribble(
  ~fiscal_year , ~total_ssi_m , ~total_fte ,
          2016 , 1857.0       ,     343212 ,
          2017 , 1926.4       ,     335687 ,
          2018 , 1961.4       ,     335565 ,
          2019 , 2000.6       ,     335754 ,
          2020 , 2010.5       ,     336400 ,
          2021 , 1990.2       ,     306101 ,
          2022 , 2057.4       ,     299289 ,
          2023 , 2075.1       ,     289380 ,
          2024 , 2096.8       ,     291837 ,
          2025 , 2119.8       ,     296999
) %>%
  mutate(ssi_per_fte = (total_ssi_m * 1000000) / total_fte)
#Combining all three datasets.
final_df <- ohio_ssi_data %>%
  left_join(cpi_data, by = "fiscal_year") %>%
  left_join(hepi_data, by = "fiscal_year") %>%
  left_join(
    midwest_cpi_data %>% select(fiscal_year, mw_cpi_rate),
    by = "fiscal_year"
  ) %>%
  mutate(
    ssi_per_fte_nominal = (total_ssi_m * 1e6) / total_fte,
    hepi_cum_index = cumprod(1 + hepi_rate),
    ssi_per_fte_real_hepi = ssi_per_fte_nominal / hepi_cum_index
  ) %>%
  rename(midwest_cpi_rate = mw_cpi_rate)


#Adjusting the code so that 2016 HEPI is the baseline and set at 1.0
final_df <- final_df %>%
  mutate(
    hepi_index_base = hepi_cum_index / first(hepi_cum_index),
    ssi_per_fte_real_2016 = ssi_per_fte_nominal / hepi_index_base
  )

#Adding a funding gap column per student multiplying the 2016 base(5410.65) by the new index.
final_df <- final_df %>%
  mutate(
    target_ssi_per_fte = first(ssi_per_fte) * hepi_index_base,
    per_student_gap = ssi_per_fte - target_ssi_per_fte,
    total_shortfall_m = (per_student_gap * total_fte) / 1e6
  )

#Adding a percentage change for SSI to normalize scale
final_df <- final_df %>%
  arrange(fiscal_year) %>%
  mutate(ssi_growth_rate = (total_ssi_m - lag(total_ssi_m)) / lag(total_ssi_m))

#Testing for normal distribution
par(mfrow = c(1, 2))
hist(final_df$hepi_rate, main = "Historgram of HEPI")
qqnorm(final_df$hepi_rate, main = "Q-Q PLot of Hepi")
qqline(final_df$hepi_rate, col = "tomato")
#Initial findings sugges non-normal distribution
#Running Shapiro-Wilk test to make final determiantion
shapiro.test(final_df$hepi_rate) #results: W = 0.97, p-value = 0.897 suggests normalcy

hist(final_df$total_ssi_m, main = "Histogram of SSI")
qqnorm(final_df$total_ssi_m, manin = "Q-Q Plot of SSI")
qqline(final_df$total_ssi_m, col = "tomato")

shapiro.test(final_df$total_ssi_m) #results: w= 0.9989, p-value 0.88 suggests normalcy

hist(final_df$avg_cpi, main = "Histogram of Average CPI")
qqnorm(final_df$avg_cpi, main = "Q-Q Plot of Average CPI")
qqline(final_df$avg_cpi, col = "tomato")

shapiro.test(final_df$avg_cpi) #Results: w = 0.89661, p-value 0.201

ggplot(final_df, aes(x = hepi_rate, y = ssi_growth_rate)) +
  geom_smooth(
    method = "lm",
    fill = "grey80",
    color = "#0072B2",
    alpha = 0.5,
    linewidth = 1.2
  ) +
  geom_point(size = 3, color = "grey25", alpha = 0.9) +
  geom_point(
    data = subset(final_df, hepi_rate == max(hepi_rate)),
    color = "#D55E00",
    size = 6
  ) +
  geom_text(
    data = subset(final_df, hepi_rate == max(hepi_rate)),
    aes(label = "Peak Cost Increase"),
    vjust = -1.5,
    hjust = 1.1
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "SSI Growth Shows Little\nRelationship to HEPI Increases",
    x = "HEPI Increase (%)",
    y = "SSI Growth (%)"
  ) +
  scale_x_continuous(
    breaks = seq(0.015, 0.055, by = 0.005),
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  annotate(
    "text",
    x = -Inf,
    y = -Inf,
    label = paste0("italic(R)^2 == ", round(r2, 2)),
    parse = TRUE,
    size = 3,
    color = "#0072B2",
    hjust = -0.2,
    vjust = -2.5
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", margin = ggplot2::margin(b = 25)),
    #plot.margin = ggplot2::margin(5.5, 40, 5.5, 5.5),
    axis.title.x = element_text(
      margin = ggplot2::margin(t = 15),
      color = "grey30"
    ),
    axis.title.y = element_text(
      margin = ggplot2::margin(r = 20),
      color = "grey30"
    ),
    plot.margin = margin(t = 25, r = 10, b = 10, l = 10)
  )


ggsave("HEPI and SSI Cor.svg")
ggsave("HEPI and SSI Cor2.svg")
ggsave("HEPI and SSI3.png", dpi = 300, width = 5, height = 5, scale = 0.8)

#Transforming row labels and creating export copy
final_df_clean <- final_df %>%
  mutate(total_ssi_m = total_ssi_m / 1000) %>%
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
  rename(
    `Fiscal Year` = fiscal_year,
    `Actual Funding ($)` = ssi_per_fte_nominal,
    `HEPI Rate Increase` = hepi_rate,
    `Total Enrollment` = total_fte,
    `Average CPI` = avg_cpi,
    `Target Funding (2016 Power)` = target_ssi_per_fte,
    `Total SSI ($B)` = total_ssi_m,
    `Gap Per Student` = per_student_gap,
    `SSI Shortfall` = total_shortfall_m,
    `CPI Increase Rate` = cpi_rate,
    `Midwest CPI Increase` = midwest_cpi_rate,
    `SSI Rate Increase` = ssi_growth_rate
  )

#Exporting to CSV for Figma
write_csv(final_df_clean, "Ohio_SSI_Funding_Gap.csv")

cor.test(final_df$ssi_growth_rate, final_df$hepi_rate)
cor.test(final_df$cpi_rate, final_df$ssi_growth_rate)

mod <- lm(ssi_growth_rate ~ hepi_rate, data = final_df)
summary(mod)
r2 <- summary(mod)$r.squared

label_df <- final_df %>%
  filter(fiscal_year == max(fiscal_year))

ggplot(final_df, aes(x = fiscal_year)) +
  geom_ribbon(
    aes(
      ymin = pmin(ssi_per_fte_real_2016, target_ssi_per_fte),
      ymax = pmax(ssi_per_fte_real_2016, target_ssi_per_fte)
    ),
    fill = "#D55E00",
    alpha = 0.2
  ) +
  geom_line(
    aes(y = ssi_per_fte_real_2016, color = "Actual SSI"),
    linewidth = 1.5
  ) +
  geom_line(
    aes(y = target_ssi_per_fte, color = "HEPI Target"),
    linewidth = 1.8
  ) +
  scale_color_manual(
    values = c("Actual SSI" = "#0072B2", "HEPI Target" = "#D55E00")
  ) +
  annotate(
    "text",
    x = 2023,
    y = 6200,
    label = "Funding Gap",
    color = "#A24600",
    size = 4,
    fontface = "bold"
  ) +
  geom_text_repel(
    data = label_df,
    aes(x = fiscal_year, y = target_ssi_per_fte, label = "HEPI"),
    color = "#D55E00",
    nudge_y = 30
  ) +
  geom_text_repel(
    data = label_df,
    aes(
      x = fiscal_year,
      y = ssi_per_fte_real_2016,
      label = "SSI"
    ),
    color = "#0072B2",
    nudge_y = -30
  ) +
  scale_x_continuous(
    breaks = seq(min(final_df$fiscal_year), max(final_df$fiscal_year), 1)
  ) +
  scale_y_continuous(labels = dollar_format(accuracy = 1)) +
  labs(
    title = "Gap Between <span style='color:#0072B2;'>SSI</span> Funding and <span style='color:#D55E00;'> HEPI-Adjusted Costs</span>",
    subtitle = "Shaded Area = Per-Student Funding Gap",
    x = "Fiscal Year",
    y = "Dollars Per Student (2016-Adjusted)",
    color = ""
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    legend.position = "none",
    axis.title.x = element_text(
      margin = ggplot2::margin(t = 25),
      color = "grey30"
    ),
    axis.title.y = element_text(
      margin = ggplot2::margin(r = 25),
      color = "grey30"
    ),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold"),
    plot.subtitle = element_markdown(margin = ggplot2::margin(b = 25)),
    plot.margin = margin(t = 25, r = 10, b = 10, l = 10)
  )
