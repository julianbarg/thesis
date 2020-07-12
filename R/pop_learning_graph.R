library(readxl)
library(tidyverse)

data <- read_xlsx("data/api_spilldata.xlsx")
data <- drop_na(data)
data <- select(data, -c(...2, original))
data <- pivot_longer(data, -year, names_to = "commodity")

filter <- c("spill_crude_per_barrel_mile", 
            "spill_refined_per_barrel_mile", 
            "spill_total_per_barrel_mile")

recode_key = c(spill_crude_per_barrel_mile = "Crude", 
               spill_refined_per_barrel_mile = "Refined", 
               spill_total_per_barrel_mile = "Total")
data <- data %>%
    filter(commodity %in% c(filter)) %>%
    mutate(commodity = recode(commodity, !!! recode_key))

caption_1 = "
Blue line: line of best fit, quadratic, with confidence interval.

Source: http://www.api.org/environment-health-and-safety/clean-water/oil-spill-prevention-and-response/\n~/media/93371EDFB94C4B4D9C6BBC766F0C4A40.ashx, p. 38"

data %>%
    ggplot(aes(x=year, y=value)) +
        facet_wrap(~ commodity) +
        geom_point() +
        geom_smooth(method="lm", formula = y ~ poly(x, 2)) +
        scale_y_continuous(limits=c(0, NA), oob=scales::rescale_none) +
        labs(x = "Year", 
             y = "Bbl spilled per Billion Barrel-Miles Transport", 
             caption = caption_1) +
        theme(strip.text = element_text(size=12, family = "Times New Roman"), 
              axis.title = element_text(size=12, family = "Times New Roman"),
              plot.caption = element_text(size=12, family = "Times New Roman"))

ggsave("illustrations/population_learning_1.png", width = 9, height = 6)



# Post 2004

library(oildata)

pipelines_y_c <- pipelines %>%
    filter(on_offshore == "onshore") %>%
    filter(commodity %in% c("crude", "rpp")) %>%
    group_by(year, commodity) %>%
    summarize(!!! oildata::pipelines_consolidation)

y_c_total <- pipelines_y_c %>%
    group_by(year) %>%
    summarize(incidents_volume = sum(incidents_volume), 
              estimate_volume_all = sum(estimate_volume_all))
y_c_total$commodity <- "total"

data_new <- bind_rows(pipelines_y_c, y_c_total) %>%
    mutate(volume_per_volume = incidents_volume/estimate_volume_all) %>%
    pivot_longer(cols = -c(year, commodity), names_to = "variable", values_to = "value")

data_new <- filter(data_new, variable == "volume_per_volume")

data_new <- select(data_new, -variable)
data_new$value <- data_new$value * 1000000000

recode_key_2 <- c(crude = "Crude", 
                  rpp = "Refined", 
                  total = "Total")
data_new <- mutate(data_new, commodity = recode(commodity, !!! recode_key_2))

caption_2 = "
Blue line: line of best fit, quadratic, with confidence interval.

Source: https://github.com/julianbarg/oildata"

data_new %>%
    ggplot(aes(x=year, y=value)) +
    facet_wrap(~ commodity) +
    geom_point() +
    geom_smooth(method="lm", formula = y ~ poly(x, 2)) +
    scale_y_continuous(limits=c(0, NA), oob=scales::rescale_none) +
    labs(x = "Year", 
         y = "Bbl spilled per Billion Barrel-Miles Transport", 
         caption = caption_2) +
    theme(strip.text = element_text(size=12, family = "Times New Roman"), 
          axis.title = element_text(size=12, family = "Times New Roman"),
          plot.caption = element_text(size=12, family = "Times New Roman"))

ggsave("illustrations/population_learning_2.png", width = 9, height = 6)



# Combined
data_new$Source = "New"
data$Source = "Historic"

data_combined <- bind_rows(data_new, data)

caption_3 = "
Blue line: line of best fit, quadratic, with confidence interval.

Source (new data):\t\thttps://github.com/julianbarg/oildata

Source (historic data):\thttp://www.api.org/environment-health-and-safety/clean-water/oil-spill-prevention-and-response/\n\t~/media/93371EDFB94C4B4D9C6BBC766F0C4A40.ashx, p. 38"

data_combined %>%
    ggplot(aes(x=year, y=value)) +
    facet_wrap(~ commodity) +
    geom_point(aes(shape=Source, color=Source)) +
    geom_smooth(method="lm", formula = y ~ poly(x, 2)) +
    scale_y_continuous(limits=c(0, NA), oob=scales::rescale_none) +
    labs(x = "Year", 
         y = "Bbl spilled per Billion Barrel-Miles Transport", 
         caption = caption_3) +
    theme(strip.text = element_text(size=12, family = "Times New Roman"), 
          axis.title = element_text(size=12, family = "Times New Roman"),
          plot.caption = element_text(size=12, family = "Times New Roman", hjust=0))

ggsave("illustrations/population_learning_3.png", width = 9, height = 6)
