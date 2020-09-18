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
Blue line: \tLine of best fit, quadratic, with confidence interval.

Source:\t\thttp://www.api.org/environment-health-and-safety/clean-water/oil-spill-prevention-\n\t\t\tand-response/~/media/93371EDFB94C4B4D9C6BBC766F0C4A40.ashx, p. 38"

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
              plot.caption = element_text(size=12, family = "Times New Roman", hjust=0))

ggsave("illustrations/population_learning_1.png", width = 7.5, height = 5)



# Post 2004

library(oildata)

data_new <- pipelines_ungrouped %>%
    filter(on_offshore == "onshore") %>%
    filter(commodity %in% c("crude", "rpp")) %>%
    select(ID, year, commodity, incidents_volume, estimate_volume_all) %>%
    group_by(year, commodity) %>%
    summarize(incidents_volume = sum(incidents_volume, na.rm = T),
              estimate_volume_all = sum(estimate_volume_all, na.rm = T)) %>%
    pivot_wider(names_from = commodity, 
                values_from = c(incidents_volume, estimate_volume_all)) %>%
    mutate(incidents_volume_total = incidents_volume_crude + incidents_volume_rpp, 
           estimate_volume_all_total = estimate_volume_all_crude + 
               estimate_volume_all_rpp) %>%
    pivot_longer(matches("crude$|rpp$|total$"),
                 names_to = c(".value", "commodity"),
                 # Fortunately, goes to the last underscore bc. greedy first .* but could be more explicit, i.e., ""(.*)_([^_]*)"
                 names_pattern = "(.*)_(.*)") %>%
    mutate(value = incidents_volume / estimate_volume_all * 1e9)

recode_key_2 <- c(crude = "Crude", 
                  rpp = "Refined", 
                  total = "Total")
data_new <- mutate(data_new, commodity = recode(commodity, !!! recode_key_2))

caption_2 = "
Blue line: \tLine of best fit, quadratic, with confidence interval.

Source: \t\thttps://github.com/julianbarg/oildata"

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
              plot.caption = element_text(size=12, family = "Times New Roman", hjust=0))

ggsave("illustrations/population_learning_2.png", width = 7.5, height = 5)



# Combined
data_new$Source = "New"
data$Source = "Historic"

data_combined <- bind_rows(data_new, data)

caption_3 = "
Blue line: \t\tLine of best fit, quadratic, with confidence interval.

Source (new):\t\thttps://github.com/julianbarg/oildata

Source (historic):\thttp://www.api.org/environment-health-and-safety/clean-water/oil-spill-prevention-\n\t\t\t\tand-response/~/media/93371EDFB94C4B4D9C6BBC766F0C4A40.ashx, p. 38"

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

ggsave("illustrations/population_learning_3.png", width = 7.5, height = 5)

caption_4 = "
Blue line: \t\tLocal regression (Loess), with confidence interval.

Source (new):\t\thttps://github.com/julianbarg/oildata

Source (historic):\thttp://www.api.org/environment-health-and-safety/clean-water/oil-spill-prevention-\n\t\t\t\tand-response/~/media/93371EDFB94C4B4D9C6BBC766F0C4A40.ashx, p. 38"

data_combined %>%
    filter(!(Source =="New" & year <= 2007)) %>%
    ggplot(aes(x=year, y=value)) +
        facet_wrap(~ commodity) +
        geom_point(aes(shape=Source, color=Source)) +
        geom_smooth() +
        # geom_smooth(method="lm", formula = y ~ poly(x, 2)) +
        scale_y_continuous(limits=c(0, NA), oob=scales::rescale_none) +
        labs(x = "Year", 
             y = "Bbl spilled per Billion Barrel-Miles Transport", 
             caption = caption_4) +
        theme(strip.text = element_text(size=12, family = "Times New Roman"), 
              axis.title = element_text(size=12, family = "Times New Roman"),
              plot.caption = element_text(size=12, family = "Times New Roman", hjust=0))

ggsave("illustrations/population_learning_4.png", width = 7.5, height = 5)

caption_4 = "
Blue line: \t\tQuadratic curve of best fit, with confidence interval.

Source (new):\t\thttps://github.com/julianbarg/oildata

Source (historic):\thttp://www.api.org/environment-health-and-safety/clean-water/oil-spill-prevention-\n\t\t\t\tand-response/~/media/93371EDFB94C4B4D9C6BBC766F0C4A40.ashx, p. 38"

data_combined %>%
    filter(!(Source =="New" & year <= 2007)) %>%
    filter(commodity == "Refined") %>%
    ggplot(aes(x=year, y=value)) +
        facet_wrap(~ commodity) +
        geom_point(aes(shape=Source, color=Source)) +
        geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
    labs(x = "Year", 
         y = "Bbl spilled per Billion Barrel-Miles Transport", 
         caption = caption_4) +
    theme(strip.text = element_text(size=12, family = "Times New Roman"), 
          axis.title = element_text(size=12, family = "Times New Roman"),
          plot.caption = element_text(size=12, family = "Times New Roman", hjust=0))

ggsave("illustrations/population_learning_5.png", width = 7.5, height = 5)
