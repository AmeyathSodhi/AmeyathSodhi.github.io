# 01_pull_and_first_plot.R
# Does getting richer still mean emitting more?
# GDP per capita vs CO2 per capita, 1990-latest, selected countries
# Data: World Bank, World Development Indicators (CC BY 4.0), via {WDI}

library(WDI)
library(dplyr)
library(ggplot2)
library(readr)

# ---- 0. Confirm indicator codes -------------------------------------------
# The World Bank replaced its old CO2 series (EN.ATM.CO2E.*) with new
# EN.GHG.* codes in Dec 2024. Refresh the cache so search sees current codes.
cache <- WDIcache()
WDIsearch("CO2.*per capita", cache = cache)
WDIsearch("renewable energy consumption", cache = cache)

# ---- 1. Pull data ----------------------------------------------------------
indicators <- c(
  co2_pc      = "EN.GHG.CO2.PC.CE.AR5",  # CO2 excl. LULUCF, t CO2e per capita
  ghg_pc      = "EN.GHG.ALL.PC.CE.AR5",  # total GHG excl. LULUCF, t CO2e per capita
  gdp_pc      = "NY.GDP.PCAP.KD",        # GDP per capita, constant 2015 US$
  renew_share = "EG.FEC.RNEW.ZS"         # renewables, % of final energy consumption
)

countries <- c("US", "GB", "DE", "CN", "IN", "BR", "1W")  # 1W = World

raw <- WDI(country = countries, indicator = indicators,
           start = 1990, end = 2024, extra = TRUE)

# Save a local copy so the project doesn't depend on the API being up
dir.create("data", showWarnings = FALSE)
write_csv(raw, "data/wdi_climate_raw.csv")

# ---- 2. Quick checks -------------------------------------------------------
raw |>
  group_by(country) |>
  summarise(across(c(co2_pc, gdp_pc, renew_share),
                   ~ max(year[!is.na(.x)]), .names = "last_{.col}"))

# ---- 3. First plot: connected scatter (decoupling) -------------------------
df <- raw |>
  filter(!is.na(co2_pc), !is.na(gdp_pc)) |>
  arrange(country, year)

ends <- df |> group_by(country) |> filter(year == max(year))

p1 <- ggplot(df, aes(gdp_pc, co2_pc, colour = country)) +
  geom_path(linewidth = 0.8,
            arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_text(data = ends, aes(label = country),
            hjust = -0.1, size = 3.2, show.legend = FALSE) +
  scale_x_log10(labels = scales::label_dollar()) +
  labs(
    title = "Does getting richer still mean emitting more?",
    subtitle = "GDP per capita vs. CO2 emissions per capita, 1990 to latest year",
    x = "GDP per capita (constant 2015 US$, log scale)",
    y = "CO2 emissions (t CO2e per person)",
    caption = "Source: World Bank, World Development Indicators (CC BY 4.0)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p1

# ---- 4. Second plot: renewables share over time ----------------------------
p2 <- raw |>
  filter(!is.na(renew_share)) |>
  ggplot(aes(year, renew_share, colour = country)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Renewable share of final energy consumption",
    x = NULL, y = "% of total final energy",
    caption = "Source: World Bank, World Development Indicators (CC BY 4.0)"
  ) +
  theme_minimal(base_size = 12)

p2

dir.create("figs", showWarnings = FALSE)
ggsave("figs/decoupling.png", p1, width = 8, height = 5.5, dpi = 300)
ggsave("figs/renewables.png", p2, width = 8, height = 5, dpi = 300)
