# 2024 Presidential Election Simulator

library(tidyverse)
library(readxl)
library(grid)        # For annotation_custom()
library(gridExtra)   # For tableGrob()
library(glue)

# Map Libraries:
# install.packages("maps")
library(maps)
library(mapdata)

data_path <- "C://Users//Brett//Desktop//RDataSets//District_and_State_Leans//2024_partisan_lean.xlsx"

# ?read_excel()

e <- readxl::read_xlsx(path = data_path, sheet = 'cgptLeans')

# e %>% head()

e2 <- e %>% 
  mutate(electoral_votes = `Electoral Votes`,
         partisan_lean = `Partisan Lean (2024)`
         ) %>% 
  mutate(favored_party = case_when(
    Direction == 'Dem' ~ 'Democrat',
    Direction == 'Rep' ~ 'Republican',
    Direction == 'EVEN' ~ 'Even',
    TRUE ~ Direction  # Keep the original value if it doesn't match any of the above
  )) %>% select(
    State, electoral_votes, partisan_lean, favored_party
  ) %>% mutate(mathematical_lean = ifelse(favored_party == 'Democrat', partisan_lean * -1, partisan_lean))

e2

### Election Simulator

simulate_election <- function(national_swing_mean, national_swing_sd, state_swing_mean, state_swing_sd, df) {
  
  # National swing
  national_swing <- rnorm(1, mean = national_swing_mean, sd = national_swing_sd)
  
  # State swings
  df$state_swing <- rnorm(n = nrow(df), mean = state_swing_mean, sd = state_swing_sd)
  
  # Final margin
  df$final_margin <- df$mathematical_lean + national_swing + df$state_swing
  
  # Winners
  df$winner <- ifelse(df$final_margin < 0, "Democrat", "Republican")
  
  # Electoral votes
  dem_electors <- sum(df$electoral_votes[df$winner == "Democrat"])
  rep_electors <- sum(df$electoral_votes[df$winner == "Republican"])
  
  # Result
  result <- if (dem_electors > rep_electors) {
    "Democrat"
  } else if (rep_electors > dem_electors) {
    "Republican"
  } else {
    "Draw"
  }
  
  # Record results
  election_result <- data.frame(
    Democrat_Electors = dem_electors,
    Republican_Electors = rep_electors,
    Result = result
  )
  
  simulation_results <- df %>%
    select(State, electoral_votes, final_margin, winner) %>%
    mutate(simulation = 1)
  
  return(list(
    simulation_summary = election_result,
    state_results = simulation_results
  ))
}

### Single Election Sim

simulate_election(
  national_swing_mean = 1,
  national_swing_sd = 1,
  state_swing_mean = 1,
  state_swing_sd = 1,
  df = e2
)

############################ Election Sim Loop #################################

# Initialize an empty list to store results
all_simulations <- list()

# Set the number of simulations
num_simulations <- 1000

# Run the simulation 1000 times
for (i in 1:num_simulations) {
  
  # Run the election simulation
  simulation <- simulate_election(
    national_swing_mean = -3.3,
    national_swing_sd = 5,
    state_swing_mean = 0,
    state_swing_sd = 3,
    df = e2
  )
  
  # Add the simulation number to the state results
  simulation$state_results$simulation <- i
  
  # Store the results in the list
  all_simulations[[i]] <- simulation$state_results
}

# Combine all the individual simulation results into a single data frame
final_simulation_results <- do.call(rbind, all_simulations)

### Total Wins:

# Calculate the winner for each simulation based on the electoral votes
election_outcomes <- final_simulation_results %>%
  group_by(simulation) %>%
  summarise(
    Democrat_Electors = sum(electoral_votes[winner == "Democrat"]),
    Republican_Electors = sum(electoral_votes[winner == "Republican"]),
    .groups = 'drop'
  ) %>%
  mutate(overall_winner = case_when(
    Democrat_Electors > Republican_Electors ~ "Democrat",
    Republican_Electors > Democrat_Electors ~ "Republican",
    TRUE ~ "Electoral College Tie"
  ))


############################ End of Election Sim Loop ##########################

############################ Outcome Odds             ##########################


#### Outcome Odds

win_probability_counts <- election_outcomes %>% 
  count(overall_winner) %>%
  mutate(n = n/num_simulations) %>%
  rename("Probability" = n)
   
print(win_probability_counts)
#### Histogram

median_dem_electors <- election_outcomes %>%
  select(Democrat_Electors) %>%
  pull() %>%
  as.numeric() %>% median()

dem_win_pct <- election_outcomes %>% 
  summarise(count = sum(overall_winner == "Democrat")) / num_simulations

electoral_college_draw_pct <- election_outcomes %>% 
  summarise(count = sum(overall_winner == "Electoral College Tie")) / num_simulations


# Plot histogram

colors <- c(
  'Democrat' = 'darkblue', 
  'Republican' = 'darkred', 
  'Electoral College Tie' = 'grey')

# Plot histogram with manual colors
h <- ggplot(
  data = election_outcomes, 
  aes(x = Democrat_Electors, fill = overall_winner)
) + 
  geom_histogram(
    binwidth = 3, 
    position = 'identity') +
  scale_fill_manual(values = colors) +  # Apply the colors
  labs(title = "Electoral College Result",
       x = 'Electors won by the Democratic Candidate',
       y = glue('Occurances in {num_simulations} simulations')) +
  theme_grey()

h2 <- h + annotate("text", 
             x = Inf, 
             y = Inf, 
             label = glue("Based on {num_simulations} simulations, the Democratic \ncandidate is forecasted to win \n{dem_win_pct} of the time with a median of {median_dem_electors} electors.\nThere is a {electoral_college_draw_pct} chance of an electoral college tie."),
             hjust = 1.0, 
             vjust = 1.1, 
             size = 3, 
             color = "black", 
             fontface = "italic") + theme(plot.title = element_text(hjust = 0.5))

############################ Mapping Odds             ##########################

f <- final_simulation_results

# Calculate the required metrics
state_summary <- f %>%
  group_by(State) %>%
  summarise(
    median_final_margin = median(final_margin, na.rm = TRUE),
    percent_dem_win = mean(final_margin < 0, na.rm = TRUE) * 100,
    percent_rep_win = mean(final_margin > 0, na.rm = TRUE) * 100,
    percent_difference_in_winprob = abs(percent_rep_win - percent_dem_win)
  )

# Print the results
print(state_summary %>% 
        filter(abs(median_final_margin) < 3) %>%
        arrange(percent_dem_win))

state <- map_data("state")

ggplot(data=state, aes(x=long, y=lat, fill=region, group=group)) + 
  geom_polygon(color = "white") + 
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3) + theme(plot.title = element_text(hjust = 0.5))

state_summary_lower <- state_summary %>%
  mutate(State = tolower(State))

state_summary_lower %>% 
  select(State) %>% print(n = 100)

# Adjust Maine/Nebraska At-Larges:
state_summary_lower <- state_summary_lower %>%
  mutate(State = gsub(" \\(at-large\\)", "", State)) %>%
  mutate(State = tolower(State))  # Convert to lowercase if necessary

# Join the Data
state_map_data <- state %>%
  left_join(state_summary_lower, by = c("region" = "State"))

# Missing States
missing_states <- 
  c('maine (cd1)',         
  'maine (cd2)',                   
  'nebraska (cd1)',      
  'nebraska (cd2)',     
  'nebraska (cd3)',
  'hawaii',
  'alaska')

# Missing State List:
state_summary_lower_offmap <- state_summary_lower %>% 
  filter(
    State == 'maine (cd1)'  |
    State == 'maine (cd2)'  |
    State =='nebraska (cd1)'|      
    State =='nebraska (cd2)'|     
    State =='nebraska (cd3)'|
    State =='hawaii'        |
    State =='alaska'
    ) 
## NOTE: I was not able to add Hawaii/Alaska/ME & NE Congressional districts- project for a later date

m <- ggplot(data = state_map_data, 
       aes(
         x = long, 
         y = lat, 
         fill = percent_dem_win,
#         label = percent_dem_win,
         group = group)) +
  geom_polygon(color = "white") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ggtitle('U.S. Map with States Colored by Win Frequency') +
  coord_fixed(1.3) + scale_fill_gradient2(
    high = "darkblue", 
    mid = "white", 
    low = "darkred", 
    midpoint = 50, 
    name = "Win Percentage") + theme(plot.title = element_text(hjust = 0.5))

h2 
m

