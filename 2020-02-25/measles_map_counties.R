# WSJ Tidy Tuesdays Measels Data Set

# How many counties on a map have a mean MMR below the ~90% needed for herd immunity?

# Load packages -----------------------------------------------

library(tidyverse) # for wrangling and plotting

# Load data from sources ---------------------------------------

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')
counties <- map_data("county")

# Clean and wrangle ---------------------------------------

clean_measles <- measles %>%
  dplyr::distinct(name, state, type, .keep_all=TRUE) %>% # remove duplicates 
  dplyr::mutate(county = tolower(county)) %>% # plann to analyse by county make sure all same case for grouping later
  dplyr::filter(mmr != -1) %>% # remove missing value -1
  dplyr::filter(lng != -1) %>% # remove missing value -1 
  dplyr::mutate(state = tolower(state)) %>%
  dplyr::group_by(county, state) %>% # group by name and state to account for duplicate county names
  dplyr::summarise(county_mean_mmr = mean(mmr, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(counties, by = c("county" = "subregion", "state" = "region")) %>%
  mutate(
    mmr_color = case_when(
      county_mean_mmr > 89.999 ~ "#275D8E", # blue
      is.na(county_mean_mmr) ~ "#F7EBD3", # white
      TRUE ~ "#DB3A2F" # red
    ))

sum_table <- clean_measles %>% summarise(unvac = sum(county_mean_mmr <= 89.99, na.rm = TRUE),
                                                     vac = sum(county_mean_mmr >= 90.00, na.rm = TRUE),
                                         total = n(),
                                         percent_below_herd = (unvac/total) * 100)

# map clean data on a map of the US with counties as an outline
ggplot(clean_measles, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = mmr_color), color = "#0B0C0B", size = 0.1) +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5) + scale_fill_identity() +
  labs(title = "1 in [placeholder] counties* have a mean vaccination rate for MMR <span style = 'color:#DB3A2F;'>below 90%</span>") +
  ggsave(here::here("measles.png"), dpi = 320, width = 12, height = 8)



