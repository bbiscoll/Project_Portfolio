# United States Visualization Project
 install.packages("usmap")
library(tidyverse)
library(usmap)


e_path <- "C:\\Users\\Brett\\Desktop\\Personal Projects\\Presidential_Election_Data\\potus_election_dataverse_files\\countypres_2000-2024.csv"

e <- read_csv(e_path)

e %>% summary()

e %>% print(n = 5)
# dataframe e contains all votes per county with a breakdown by mode of voting.

e2 <- e %>% 
    filter(
    #    state == 'PENNSYLVANIA' & 
    #    county_name == 'DELAWARE'
        ) %>% 
group_by(state, county_name, year, office, candidate, party, county_fips) %>%
  summarise(
    total_candidate_votes = sum(candidatevotes, na.rm = TRUE),
    .groups = "drop"
  ) 

# dataframe e2 contains all votes per county without a breakdown by mode of voting.

e3 <- e2 %>%
  group_by(state, year, office, candidate, party) %>%
  summarise(
    state_candidate_votes = sum(total_candidate_votes, na.rm = TRUE),
    .groups = "drop"
  ) 
  
# dataframe e3 contains all votes per state without a breakdown by county.

e4 <- e3 %>% 
#  filter(state == "PENNSYLVANIA") %>% 
  group_by(state, year) %>% 
  mutate(
    vote_share_percentage = state_candidate_votes / sum(state_candidate_votes, na.rm = TRUE)
  ) %>% 
  ungroup()

# dataframe e4 contains all votes per state with a candidate percentage.

e5 <- e4 %>%
  group_by(state, party, year) %>%
  summarize(
    state_candidate_votes = sum(state_candidate_votes, na.rm = TRUE),
    vote_share_percentage = sum(vote_share_percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(state, party),
    names_from = year,
    values_from = c(state_candidate_votes, vote_share_percentage),
    names_glue = "{year}_{.value}"
  )

# e5 contains state level calculation

# Map of the US Geographic Data


# Prep the data - filter to Libertarian 2020 vote share
lib_2024 <- e5 %>%
  filter(party == "LIBERTARIAN") %>%
  select(state, `2020_vote_share_percentage`) %>%
  rename(vote_share = `2020_vote_share_percentage`) %>%
  mutate(state = str_to_title(state))  # usmap expects "Alabama" not "ALABAMA"

# Plot
plot_usmap(data = lib_2024, values = "vote_share", regions = "states") +
  scale_fill_gradient(low = "white", high = "yellow", name = "Vote Share") +
  labs(title = "2020 Libertarian Presidential Vote Share by State") +
  theme_minimal()

# Prep the data - filter to Republican 2024 vote share
gop_2024 <- e5 %>%
  filter(party == "REPUBLICAN") %>%
  select(state, `2024_vote_share_percentage`) %>%
  rename(vote_share = `2024_vote_share_percentage`) %>%
  mutate(state = str_to_title(state))  # usmap expects "Alabama" not "ALABAMA"

# Plot
plot_usmap(data = gop_2024, values = "vote_share", regions = "states") +
  scale_fill_gradient(low = "white", high = "red", name = "Vote Share") +
  labs(title = "2024 Republican Presidential Vote Share by State") +
  theme_minimal()


# Prep Data- 

# Prep the data
margin_2024 <- e5 %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  select(state, party, `2024_vote_share_percentage`) %>%
  pivot_wider(
    names_from = party,
    values_from = `2024_vote_share_percentage`
  ) %>%
  mutate(
    margin = DEMOCRAT - REPUBLICAN,
    state = str_to_title(state)
  )

# Plot
plot_usmap(data = margin_2024, values = "margin", regions = "states") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0,
    breaks = seq(-30, 30, by = 15),
    name = "D - R Margin"
  )  +
  labs(title = "2024 Presidential Election: Democrat vs Republican Vote Share Margin") +
  theme_minimal()


#### Repeatable Function - State Level

plot_party_margin <- function(data, year, party1, party2, color1 = "blue", color2 = "red") {
  
  year_col <- paste0(year, "_vote_share_percentage")
  
  margin_data <- data %>%
    filter(party %in% c(party1, party2)) %>%
    select(state, party, all_of(year_col)) %>%
    pivot_wider(names_from = party, values_from = all_of(year_col)) %>%
    mutate(
      margin = .data[[party1]] - .data[[party2]],
      state = str_to_title(state)
    )
  
  plot_usmap(data = margin_data, values = "margin", regions = "states") +
    scale_fill_gradient2(
      low = color2, mid = "white", high = color1,
      midpoint = 0,
      name = paste(party1, "-", party2, "Margin")
    ) +
    labs(title = paste(year, "Presidential Election:", party1, "vs", party2, "Vote Share Margin")) +
    theme_minimal()
}

# Usage
plot_party_margin(e5, 2024, "DEMOCRAT", "REPUBLICAN", color1 = "blue4", color2 = "red4")
