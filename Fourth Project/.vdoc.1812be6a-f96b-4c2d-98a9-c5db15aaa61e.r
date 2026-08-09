#
#
#
#
#
#
library(tidyverse)

set.seed(101)

advanced_macro_data <- expand_grid(
  country = c("China", "South Korea", "United States", "Japan", "Taiwan"),
  year = 1980:2025
) %>%
  mutate(
    # Simulate Foreign Direct Investment with some variance
    fdi_bln = case_when(
      country == "China" & year > 2000 ~ (year - 2000)^1.5 * 10 + rnorm(n(), 50, 20),
      country == "United States" ~ (year - 1980) * 12 + rnorm(n(), 100, 30),
      TRUE ~ (year - 1980) * 5 + rnorm(n(), 20, 10)
    ),
    # Simulate Unemployment Rate (spiking during known crises)
    unemployment_rate = case_when(
      country == "South Korea" & year %in% 1997:1999 ~ rnorm(n(), 7.5, 1),
      country == "United States" & year %in% c(2008, 2009, 2010, 2020) ~ rnorm(n(), 8.5, 1.5),
      TRUE ~ rnorm(n(), 4.0, 1.2)
    ),
    # Simulate Inflation - introducing NA values randomly to force data cleaning
    inflation_rate = rnorm(n(), 3.0, 2.5),
    # Simulate Government Debt as % of GDP
    gov_debt_pct = case_when(
      country == "Japan" ~ (year - 1980) * 4 + 50 + rnorm(n(), 10, 5),
      country == "United States" & year > 2000 ~ (year - 2000) * 3 + 60 + rnorm(n(), 5, 5),
      TRUE ~ (year - 1980) * 1.5 + 20 + rnorm(n(), 5, 5)
    ),
    # Simulate Tech Exports
    tech_exports_bln = if_else(
      country %in% c("South Korea", "Taiwan", "China") & year > 1995,
      (year - 1995)^2 * 1.2 + rnorm(n(), 10, 5),
      (year - 1980) * 2 + rnorm(n(), 5, 2)
    ),
    # Add a messy text column mimicking qualitative analyst notes
    analyst_note = sample(
      c(" stable_growth ", "CRISIS_watch", "recession Warning", " steady ", NA), 
      size = n(), replace = TRUE, prob = c(0.4, 0.1, 0.1, 0.3, 0.1)
    )
  ) %>%
  # Introduce random missing values (NAs) in inflation to practice cleaning
  mutate(inflation_rate = ifelse(runif(n()) < 0.1, NA, inflation_rate)) %>%
  # Round numerics
  mutate(across(where(is.numeric) & !matches("year"), ~round(.x, 2)))

# Save to your working directory
write_csv(advanced_macro_data, "advanced_macro_data.csv")
#
#
#
#
#
#
advanced_macro_data %>%
    group_by(country) %>%
    summarize(
        avg_fdi = mean(fdi_bln, na.rm = TRUE),
        sd_fdi = sd(fdi_bln, na.rm = TRUE)
    ) %>%
    arrange(desc(avg_fdi))
#
#
#
#
#
#
advanced_macro_data %>%
    group_by(country) %>%
    summarize(
        avg_debt = mean(gov_debt_pct, na.rm = TRUE),
        med_debt = median(gov_debt_pct, na.rm = TRUE)
    ) %>%
    arrange(desc(med_debt))
#
#
#
#
#
#
advanced_macro_data %>%
    group_by(country) %>%
    filter(unemployment_rate == max(unemployment_rate, na.rm = TRUE)) %>%
    select(country, year, unemployment_rate)
#
#
#
#
#
#
advanced_macro_data %>%
    group_by(country) %>%
    filter(fdi_bln == max(fdi_bln, na.rm = TRUE)) %>%
    select(country, year, fdi_bln) %>%
    arrange(desc(fdi_bln))
#
#
#
#
#
#
advanced_macro_data %>%
    group_by(country) %>%
    summarize(missing_inflation_count = sum(is.na(inflation_rate))) %>%
    arrange(desc(missing_inflation_count))
#
#
#
#
#
#
advanced_macro_data %>%
    group_by(country) %>%
    summarize(missing_analyst_note = sum(is.na(analyst_note))) %>%
    arrange(desc(missing_analyst_note))
#
#
#
#
#
advanced_macro_data %>%
    mutate(decade = year %/% 10 * 10) %>%
    group_by(country, decade) %>%
    summarize(
        median_debt = median(gov_debt_pct, na.rm = TRUE)
    )
#
#
#
#
#
#
advanced_macro_data %>%
    mutate(five_year_block = year %/% 5 * 5) %>%
    group_by(country, five_year_block) %>%
    summarize(
        avg_unemployment = mean(unemployment_rate, na.rm = TRUE)
    )
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'South Korea') %>%
    arrange(year) %>%
    mutate(
        prev_inflation = lag(inflation_rate),
        yoy_inflation_change = inflation_rate - prev_inflation
    )
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'United States') %>%
    arrange(year) %>%
    mutate(ly_gov_debt_pct = lag(gov_debt_pct),
    yoy_gov_debt_pct = gov_debt_pct - ly_gov_debt_pct
    ) %>%
    select(country, year, gov_debt_pct, ly_gov_debt_pct, yoy_gov_debt_pct)
```
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'Taiwan') %>%
    arrange(year) %>%
    mutate(total_exports = cumsum(tech_exports_bln)) %>%
    ggplot(aes(x = year, y = total_exports)) +
    geom_area()
#
#
#
#
#
#
#
#
advanced_macro_data %>%
filter(country %in% c('Japan', 'South Korea'), year >= 2000) %>%
group_by(country) %>%
arrange(year) %>%
mutate(sum_fdi_bln = cumsum(fdi_bln)) %>%
ggplot(aes(x = year, y = sum_fdi_bln, color = country)) +
geom_line()
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('China', 'Taiwan', 'South Korea'), year >= 1990) %>%
    arrange(year) %>%
    mutate(sum_exports = cumsum(tech_exports_bln)) %>%
    ggplot(aes(x = year, y = sum_exports, color = country)) +
    geom_line()
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('United States', 'China', 'Japan'), year >= 2005) %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(sum_gdebtp = cumsum(gov_debt_pct)) %>%
    ggplot(aes(x = year, y = sum_gdebtp, color = country)) +
    geom_line()
```
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'China') %>%
    arrange(year) %>%
    mutate(
        fdi_rolling_avg = ((fdi_bln + lag(fdi_bln) + lag(fdi_bln, 2)) /3)
    ) %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = fdi_bln), color = 'black') +
    geom_line(aes(y = fdi_rolling_avg), color = 'blue', linewidth = 1)
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'South Korea') %>%
    arrange(year) %>%
    mutate(
        rol_avg_tech = ((tech_exports_bln + lag(tech_exports_bln) + lag(tech_exports_bln, 2)) / 3)
    ) %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = tech_exports_bln), color = 'purple') +
    geom_line(aes(y = rol_avg_tech), color = 'orange')
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'United States', year >= 2000) %>%
    arrange(year) %>%
    mutate(
        rol_avg_gdpt = ((gov_debt_pct) + lag(gov_debt_pct) + lag(gov_debt_pct, 2) + lag(gov_debt_pct, 3)) / 4
    ) %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = gov_debt_pct, color = 'blue')) +
    geom_line(aes(y = rol_avg_gdpt, color = 'red'))
#
#
#
#
#
#
advanced_macro_data %>%
    select(year, country, tech_exports_bln) %>%
    pivot_wider(
        names_from = country,
        values_from = tech_exports_bln
    )
#
#
#
#
#
#
advanced_macro_data %>%
    select(year, country, fdi_bln) %>%
    pivot_wider(
        names_from = country,
        values_from = fdi_bln
    )
```
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('China', 'Japan', 'Taiwan', 'South Korea')) %>%
    select(year, country, gov_debt_pct) %>%
    pivot_wider(
        names_from = country,
        values_from = gov_debt_pct
    )
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('United States', 'China')) %>%
    arrange(year) %>%
    mutate(rol_avg = ((unemployment_rate) + lag(unemployment_rate) + lag(unemployment_rate, 2)) / 3) %>%
    select(year, country, rol_avg) %>%
    pivot_wider(
        names_from = country,
        values_from = rol_avg
    )
#
#
#
#
#
#
advanced_macro_data %>%
    select(year, country, tech_exports_bln) %>%
    pivot_wider(
        names_from = country,
        values_from = tech_exports_bln
    ) %>%
    mutate(tech_diff = `South Korea` - `Taiwan`) %>%
    ggplot(aes(x = year, y = tech_diff)) +
    geom_col(fill = 'blue') +
    labs(
        title = "Tech Export Difference: South Korea and Taiwan",
        x = "Year",
        y = "Difference (Billions USD)"
    )
#
#
#
#
#
advanced_macro_data %>%
    select(year, country, unemployment_rate) %>%
    pivot_wider(
        names_from = country,
        values_from = unemployment_rate
    ) %>%
    mutate(unemp_gap = `United States` - `Japan`) %>%
    ggplot(aes(x = year, y = unemp_gap)) +
    geom_col(fill = 'red') +
    labs(
        title = "Unemployment Gap between the United States and Japan",
        x = "Year",
        y = "Unemployment Gap (In Percent)"
    )
#
#
#
#
#
#
advanced_macro_data %>%
    filter(year >= 2005) %>%
    select(year, country, fdi_bln) %>%
    pivot_wider(
        names_from = country,
        values_from = fdi_bln
    ) %>%
    mutate(fdi_diff = `China` - `South Korea`) %>%
    ggplot(aes(x = year, y = fdi_diff)) +
    geom_col(fill = 'purple') +
    labs(
        title = "FDI difference between China and South Korea",
        x = "Year",
        y = "FDI difference"
    )
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    select(year, country, gov_debt_pct) %>%
    pivot_wider(
        names_from = country,
        values_from = gov_debt_pct
    ) %>%
    mutate(total_debt_pct = `Taiwan` + `South Korea`) %>%
    ggplot(aes(x = year, y = total_debt_pct)) +
    geom_col(fill = 'green') +
    labs(
        title = "Total amount of cumulative debt between South Korea and Taiwan",
        x = "Year",
        y = "Cumulative Debt"
    )
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('United States', 'China')) %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(rolling_fdi = (fdi_bln +lag(fdi_bln)+ lag(fdi_bln, 2)) / 3) %>%
    filter(year >= 2000) %>%
    select(year, country, rolling_fdi) %>%
    pivot_wider(
        names_from = country,
        values_from = rolling_fdi
    ) %>%
    mutate(fdi_gap = `China` - `United States`) %>%
    ggplot(aes(x = year, y = fdi_gap)) +
    geom_col(fill = 'red')
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    select(year, country, tech_exports_bln) %>%
    pivot_wider(
        names_from = country,
        values_from = tech_exports_bln
    ) %>%
    pivot_longer(
        cols = -year,
        names_to = "country",
        values_to = "tech_exports_bln"
    )
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    select(year, country, fdi_bln) %>%
    pivot_wider(
        names_from = country,
        values_from = fdi_bln
    ) %>%
    pivot_longer(
        cols = -year,
        names_to = "country",
        values_to = "fdi_bln"
    )
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('China', 'Japan', 'South Korea')) %>%
    select(year, country, gov_debt_pct) %>%
    pivot_wider(
        names_from = country,
        values_from = gov_debt_pct
    ) %>%
    pivot_longer(
        cols = c(`China`, `Japan`, `South Korea`),
        names_to = "country",
        values_to = "gov_debt_pct"
    )
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('United States', 'China')) %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(rolavgun = ((unemployment_rate) + lag(unemployment_rate) + lag(unemployment_rate, 2)) / 3) %>%
    ungroup(country) %>%
    select(year, country, rolavgun) %>%
    pivot_wider(
        names_from = country,
        values_from = rolavgun
    ) %>%
    pivot_longer(
        cols = c(`United States`, China),
        names_to = "country",
        values_to = "rolavgun"
    )
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    ggplot(aes(x = year, y = gov_debt_pct)) +
    geom_line(color = 'blue') +
    facet_wrap(~country, scales = "free_y") +
    labs(
        title = "test",
        x = "Year",
        y = "Government Debt (% of GDP)"
    )
#
#
#
#
#
#
advanced_macro_data %>%
    ggplot(aes(x = year, y = fdi_bln)) +
    geom_line(color = 'red') +
    facet_wrap(~country, scales = "free_y") +
    labs(
        title = "Second Test",
        x = "Year",
        y = "Foreign Direct Investment (Billions)"
    )
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(year >= 2005) %>%
    ggplot(aes(x = year, y = unemployment_rate)) +
    geom_line(color = 'red') +
    facet_wrap(~country, scales = "free_y") +
    labs(
        title = "Third Test",
        x = "Year",
        y = "Unemployment Rate"
    )
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    group_by(country) %>%
    arrange(year) %>%
    mutate(rol_avg_teb = (tech_exports_bln + lag(tech_exports_bln, 1) + lag(tech_exports_bln, 2)) / 3) %>%
    ungroup(country) %>%
    ggplot(aes(x = year, y = rol_avg_teb)) +
    geom_line() +
    facet_wrap(~country, scales = "free_y") +
    labs(
        title = "fourth test",
        x = "year",
        y = "rolling average of tech exports"
    )
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'United States') %>%
    ggplot(aes(x = year, y = fdi_bln)) +
    geom_line(color = "darkgreen") +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
        title = "United States FDI over time",
        x = "Year",
        y = "Foreign Direct Investment (Billions)"
    )
```
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'China') %>%
    ggplot(aes(x = year, y = tech_exports_bln))+
    geom_line()+
    scale_y_continuous(labels = scales::label_dollar())
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'South Korea') %>%
    ggplot(aes(x = year, y = unemployment_rate)) +
    geom_line() +
    scale_y_continuous(labels = scales::label_percent(scale =1))
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'Japan') %>%
    ggplot(aes(x = year, y = gov_debt_pct)) +
    geom_line() +
    scale_y_continuous(labels = scales::label_comma())
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    drop_na(inflation_rate, unemployment_rate) %>%
    ggplot(aes(x = inflation_rate, y = unemployment_rate, color = country)) +
    geom_point() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(
        title = "Inflation Rate vs Unemployment Rate",
        x = "Inflation Rate (%)",
        y = "Unemployment Rate (%)",
        color = "Country"
    )
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    drop_na(gov_debt_pct, inflation_rate) %>%
    ggplot(aes(x = gov_debt_pct, y = inflation_rate, color = country)) +
    geom_point() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs(
        title = "Comparison of Government Debt to Inflation Rate",
        x = "Government Debt (%)",
        y = "Inflation Rate (%)",
        color = "Country"
    )
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
filter(country %in% c('United States', 'China')) %>%
drop_na(tech_exports_bln, fdi_bln) %>%
ggplot(aes(x = tech_exports_bln, y = fdi_bln, color = country)) +
geom_point() +
theme_bw() +
theme(legend.position = "bottom") +
labs(
    title = "first test",
    x = "tech exports bln",
    y = "fdi bln"
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(year >= 2010) %>%
    drop_na(fdi_bln, gov_debt_pct) %>%
    ggplot(aes(x = fdi_bln, y = gov_debt_pct, color = country)) +
    geom_point() +
    theme_classic() +
    theme(legend.position = "bottom") +
    scale_x_continuous(labels = scales::label_dollar()) +
    labs(
        title = "Graph showing fdi to gvt debt",
        x = "fdi (bil)",
        y = "gvt debt (%)"
    )
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'South Korea') %>%
    ggplot(aes(x = year, y = unemployment_rate)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 1997, linetype = "dashed", color = "red") +
    annotate(
        "text",
        x = 1997.5,
        y = 20,
        label = "Asian Financial Crisis",
        color = "red",
        hjust = 0
    ) +
    labs(
        title = "graph showing asian financial crisis damage.",
        x = "Year",
        y = "Unemployment Rate(%)"
    )
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'United States') %>%
    ggplot(aes(x = year, y = gov_debt_pct)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = 2008, linetype = "solid", color = "red") +
    annotate(
        "text",
        x = 2008.5,
        y = 10,
        label = "Global Financial Crisis",
        color = "red",
        hjust = "red"
    ) +
    labs(
        title = "Graph Showing Effects of the Global Financial Crisis",
        x = "Year",
        y = "Government Debt as a Percent"
    )
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'China') %>%
    ggplot(aes(x = year, y = inflation_rate))+
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 3, linetype = "dashed", color = "blue") +
    annotate(
        "text",
        x = 2000,
        y = 3.5,
        label = "Inflation Target",
        color = "red",
        hjust = 0
    ) +
    labs(
        title = "Graph showing Inflation Target",
        x = "Year",
        y = "Inflation Rate"
    )
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'Japan') %>%
    ggplot(aes(x = year, y = unemployment_rate)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "blue") +
    annotate(
        "Text",
        x = 2010,
        y = 7,
        label = "COVID-19 Shock",
        color = "red",
        hjust = 0
    ) +
    labs(
        title = "Graph showing damage of COVID-19",
        x = "Year",
        y = "Unemployment Rate"
    )
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(year >= 1995, year <= 1997) %>%
    filter(country %in% c('United States', 'South Korea')) %>%
    group_by(country) %>%
    arrange(country, year) %>%
    mutate(rol_avg_teb = (tech_exports_bln + lag(tech_exports_bln) + lag(tech_exports_bln, 2)) / 3) %>%
    ungroup(country) %>%
    select(country, year, tech_exports_bln, rol_avg_teb)
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(year >= 2010) %>%
    filter(country %in% c('China', 'Japan', 'South Korea')) %>%
    select(year, country, tech_exports_bln) %>%
    pivot_wider(
        names_from = country,
        values_from = tech_exports_bln
    )

#
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(year >= 2010) %>%
    filter(country %in% c('China', 'Japan', 'South Korea')) %>%
    select(year, country, tech_exports_bln) %>%
    pivot_wider(
        names_from = country,
        values_from = tech_exports_bln
    ) %>%
    pivot_longer(
        cols = -year,
        names_to = "country",
        values_to = "tech_exports_bln"
    )
#
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('United States', 'China')) %>%
    select(year, country, fdi_bln) %>%
    pivot_wider(
        names_from = country,
        values_from = fdi_bln
    ) %>%
    pivot_longer(
        cols = -year,
        names_to = "country",
        values_to = "fdi_bln"
    ) %>%
    ggplot(aes(x = year, y = fdi_bln, color = country)) +
    geom_line() +
    scale_y_continuous(labels = scales::label_dollar()) +
    labs(
        title = "Comparison of China and United States FDI (billions USD)",
        x = "Year",
        y = "FDI (in Billions)"
    )
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    arrange(year) %>%
    group_by(country) %>%
    mutate(rlng_avg_unem = (unemployment_rate + lag(unemployment_rate) + lag(unemployment_rate, 2)) / 3) %>%
    ungroup(country) %>%
    ggplot(aes(x = year, y = rlng_avg_unem)) +
    geom_line() +
    geom_point()+
    theme_minimal() +
    facet_wrap(~country, scales = "free_y") +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    labs(
        title = "Three-year rolling averages for unemployment in Major Economies",
        x = "Year",
        y = "Unemployment"
    )
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'South Korea') %>%
    ggplot(aes(x = year, y = gov_debt_pct)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 1997, linetype = "dashed", color = "red") +
    annotate(
        "text",
        x = 1998,
        y = 6,
        label = "IMF Bailout",
        color = "red",
        hjust = 0
    ) +
    labs(
        title = "Graph showing the effects of the IMF Bailout",
        x = "Year",
        y = "Government Debt as a Percentage of GDP"
    )
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(year >= 2005) %>%
    drop_na(inflation_rate, unemployment_rate) %>%
    ggplot(aes(x = inflation_rate, y = unemployment_rate, color = country)) +
    geom_point() +
    theme_light() +
    theme(legend.position = "bottom")
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(year >= 2008) %>%
    filter(country %in% c('China', 'Japan', 'South Korea')) %>%
    drop_na(gov_debt_pct, fdi_bln) %>%
    ggplot(aes(x = gov_debt_pct, y = fdi_bln, color = country)) +
    geom_point() +
    scale_y_continuous(labels = scales::label_dollar()) +
    theme(legend.position = "top") +
    theme_minimal()
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(year < 2020) %>%
    filter(country %in% c('United States', 'China', 'South Korea')) %>%
    drop_na(tech_exports_bln, unemployment_rate) %>%
    ggplot(aes(x = tech_exports_bln, y = unemployment_rate, color = country)) +
    scale_x_continuous(labels = scales::label_dollar()) +
    scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    #Decided that I would also make sure the unit we are placing in the y category is accurate since we are already doing the same for the x axis
    geom_point() +
    theme_classic() +
    labs(
        title = "Technology Exports to Unemployment Dynamics in North America and Asia",
        x = "Tech Exports (Billions USD)",
        y = "Unemployment Rate (%)"
    )
#
#
#
#
#
#
advanced_macro_data %>%
filter(country == 'Japan') %>%
drop_na(year, inflation_rate) %>%
ggplot(aes(x = year, y = inflation_rate)) +
geom_line() +
geom_point() +
geom_hline(yintercept = 2.5, x = 2000, color = "red")
#
#
#
#
#
#
advanced_macro_data %>%
filter(country %in% c('China', 'Japan', 'South Korea')) %>%
select(year, country, tech_exports_bln) %>%
pivot_wider(
    names_from = "country",
    values_from = "tech_exports_bln"
) %>%
pivot_longer(
    cols = -year,
    names_to = "country",
    values_to = "tech_exports_bln"
) %>%
group_by(country) %>%
arrange(year) %>%
mutate(threeyear = (tech_exports_bln + lag(tech_exports_bln) + lag(tech_exports_bln, 2)) / 3) %>%
ungroup() %>%
drop_na(threeyear) %>%
ggplot(aes(x = year, y = threeyear)) +
geom_line() +
facet_wrap(~country, scales = "free_y") +
scale_y_continuous(labels = scales::label_dollar())
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country == 'United States') %>%
    ggplot(aes(x = year, y = gov_debt_pct)) +
    geom_line() +
    theme_classic() +
    geom_vline(xintercept = 2008.5, linetype = "dashed", color = "red") +
    annotate(
        "rect",
        xmin = 2007, xmax = 2009,
        ymin = -Inf, ymax = Inf,
        fill = "red",
        alpha = 0.2
    )
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('China', 'Japan', 'South Korea')) %>%
    filter(year >= 2008) %>%
    drop_na(gov_debt_pct, fdi_bln) %>%
    ggplot(aes(x = fdi_bln, y = gov_debt_pct, color = country)) +
    scale_y_continuous(labels = scales::label_dollar()) +
    geom_point() +
    theme_minimal() +
    theme(legend.position = "Top")
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('United States', 'China', 'South Korea')) %>%
    filter(year < 2020) %>%
    drop_na(tech_exports_bln, unemployment_rate) %>%
    ggplot(aes(x = tech_exports_bln, y = unemployment_rate, color = country)) +
    scale_x_continuous(labels = scales::label_dollar()) +
    theme_classic() +
    geom_point() +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
        title = "Technology Exports and Unemployment Trade-Off",
        x = "Tech Exports (Billions in USD)",
        y = "Unemployment Rate (%)"
    )
```
#
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('Taiwan', 'South Korea', 'Japan')) %>%
    select(year, country, gov_debt_pct) %>%
    pivot_wider(
        names_from = "country",
        values_from = "gov_debt_pct"
    ) %>%
    pivot_longer(
        cols = -year,
        names_to = "country",
        values_to = "gov_debt_pct"
    ) %>%
    ggplot(aes(x = year, y = gov_debt_pct, color = country)) +
    scale_y_continuous(labels = scales::label_percent(scale =1)) +
    theme_minimal() +
    geom_point()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('China', 'United States')) %>%
    select(year, country, tech_exports_bln) %>%
    pivot_wider(
        names_from = "country",
        values_from = "tech_exports_bln"
    ) %>%
    pivot_longer(
        cols = -year,
        names_to = "country",
        values_to = "tech_exports_bln"
    ) %>%
    arrange(year) %>%
    group_by(country) %>%
    mutate(threeyear = (tech_exports_bln + lag(tech_exports_bln) + lag(tech_exports_bln, 2)) /3) %>%
    ungroup(country) %>%
    ggplot(aes(x = year, y = threeyear)) +
    geom_line() +
    facet_wrap(~country, scales = "free_y") +
    scale_y_continuous(labels = scales::label_dollar())
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    # First we must convert the country category as a whole to a factor rather than a normal text string
    mutate(country = factor(country)) %>%
    mutate(country = stats::relevel(country, ref = "China")) %>%
    mutate(country = stats::relevel(country, ref = "South Korea")) %>%
    # We must put South Korea last because that command simply immediately moves whatever you put down to the top
    ggplot(aes(x = country, y = tech_exports_bln)) +
    geom_boxplot() +
    scale_y_continuous(labels = scales::label_dollar()) +
    theme_minimal()
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    mutate(country = factor(country)) %>%
    mutate(country = stats::relevel(country, ref = "Japan")) %>%
    mutate(country = stats::relevel(country, ref = "United States")) %>%
    drop_na(fdi_bln) %>%
    ggplot(aes(x = country, y = fdi_bln)) +
    scale_y_continuous(labels = scales::label_dollar()) +
    geom_boxplot() +
    theme_minimal()
#
#
#
#
#
#
#
advanced_macro_data %>%
    filter(country %in% c('China', 'United States', 'Japan', 'South Korea')) %>%
    mutate(country = factor(country, levels = c("South Korea", "Japan", "China", "United Staes"))) %>%
    drop_na(inflation_rate) %>%
    ggplot(aes(x = year, y = inflation_rate)) +
    facet_wrap(~ country) +
    geom_line()
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    mutate(
        str_replace_all(analyst_note, "_", " "),
        str_to_lower(analyst_note),
        str_trim(analyst_note)
    )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    mutate(
        country = str_replace_all(country, "-", " "),
        country = str_trim(country),
        country = str_to_title(country)
    ) %>%
    filter(country %in% c('United States', 'China', 'South Korea')) %>%
    ggplot(aes(x = year, y = unemployment_rate, color = country)) +
    geom_line() +
    # going to add in a rectangle to practice all concepts
    annotate(
        "rect",
        xmin = 2000, xmax = 2002,
        ymin = -Inf, ymax = Inf,
        fill = "red",
        alpha = 0.2
    ) +
    annotate(
        "text",
        x = 2003,
        y = 7.5,
        label = "The Great Data Purge",
        color = "red",
        hjust = 0
    )
#
#
#
#
#
#
#
#
#
#
#
#
#
advanced_macro_data %>%
    mutate(
        analyst_note = str_to_upper(analyst_note) %>%
        str_replace_all("\\.", " ") %>%
        str_replace_all("_", " ") %>%
        str_trim()
    ) %>%
    drop_na(analyst_note) %>%
    group_by(analyst_note) %>%
    mutate(avgfdi_bln = mean(fdi_bln)) %>%
    ggplot(aes(x = analyst_note, y = avgfdi_bln)) +
    geom_col(fill = "steelblue") +
    scale_y_continuous(labels = scales::label_dollar()) +
    theme_minimal()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
