# check to see if the required packages are installed and if not install them
list.of.packages <- c('tidyverse', 'tidycensus', 'furrr', 'janitor')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load the required packages
lapply(list.of.packages, library, character.only = TRUE)


# This checks to see if the user already has a census API key
# and if not then it will install one and reload the R environment
if(is.na(Sys.getenv('CENSUS_API_KEY'))){
  census_api_key("19f8500f78c8baa62f59b7e6974647d454ef55f6", install = TRUE)
  readRenviron("~/.Renviron")
}
library(tidycensus)
library(tidyverse)
library(devtools)
require(siverse)
library(lubridate)
library(plotly)
library(readxl)
library(janitor)
require(dplyr)

source("C:/Users/allso/Documents/Sorenson Impact Internship/UCC Stuff/census_acs/get_acs_combo.R")


edu <- get_acs_combo(
  table = "B06009",
  year = 2018,
  geography = "county",
  state = "US",
  survey = "acs5",
  use.parallel = T
)

#Filter data to only include education levels
edu <- edu %>% 
  filter(level == 2) %>% 
  filter(levlab != c("Born in state of residence", "Born in other state in the United States",
                     "Native; born outside the United States", "Foreign born"))


#Create dataset of just those that did not finish high school per state
less_than_hs <- filter(edu, levlab == "Less than high school graduate") %>% 
  group_by(state) %>% 
  summarise_at(vars(estimate), funs(sum(estimate, na.rm = TRUE)))


#find each state population
state_pops <- edu %>% 
  group_by(state) %>% 
  summarise_at(vars(estimate), funs(sum(estimate, na.rm = TRUE))) %>% 
  rename(population = estimate)

#join to get the percentage to make it easier to compare
less_than_hs <- less_than_hs %>% left_join(state_pops, by = "state") %>% 
  mutate(proportion = estimate / population)

#plot the proportion that did not graduate hs by state
ggplot(less_than_hs) +
  geom_bar(aes(proportion, reorder(state, -proportion)), stat = "identity") +
  ggtitle("Proportion of Less than High School Grads by State") +
  xlab("Proportion that did not Complete High School") +
  ylab("State")


ggplot(less_than_hs, aes(proportion, population)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Less than High School Proportion by State Population") +
  xlab("Proportion from each State that didn't Complete High School") +
  ylab("State Population Size")


#Look at just those that graduated high school
hs <- filter(edu, levlab == "High school graduate (includes equivalency)") %>% 
  group_by(state) %>% 
  summarise_at(vars(estimate), funs(sum(estimate, na.rm = TRUE))) %>% 
  left_join(state_pops, by = "state") %>% 
  mutate(proportion = estimate / population)


#plot the proportion that graduated hs by state
ggplot(hs) +
  geom_bar(aes(proportion, reorder(state, -proportion)), stat = "identity") +
  ggtitle("Proportion of High School Graduates by State") +
  xlab("Proportion of High School Graduates") +
  ylab("State")





#Look at just those with bachelor degrees per state
bachelor <- filter(edu, levlab == "Bachelor's degree") %>% 
  group_by(state) %>%
  summarise_at(vars(estimate), funs(sum(estimate, na.rm=TRUE))) %>% 
  left_join(state_pops, by = "state") %>% 
  mutate(proportion = estimate / population)

#plot the proportion that graduated hs by state
ggplot(bachelor) +
  geom_bar(aes(proportion, reorder(state, -proportion)), stat = "identity") +
  ggtitle("Proportion of People with Bachelor's Degrees by State") +
  ylab("State") +
  xlab("Proportion of People with Bachelor's Degrees")







#Look at just those graduate degrees per state and join to get proportion
graduate <- filter(edu, levlab == "Graduate or professional degree") %>% 
  group_by(state) %>%
  summarise_at(vars(estimate), funs(sum(estimate, na.rm=TRUE))) %>% 
  left_join(state_pops, by = "state") %>% 
  mutate(proportion = estimate / population)

#plot the proportion that have graduate degrees by state
ggplot(graduate) +
  geom_bar(aes(proportion, reorder(state, -proportion)), stat = "identity") +
  ggtitle("Proportion of Graduate Degrees by State") +
  ylab("State") +
  xlab("Proportion of Graduate Degrees")



#Any college degree (bachelors and graduate)

#Look at just those graduate degrees
at_least_bachelors <- filter(edu, levlab == c("Bachelor's degree", "Graduate or professional degree")) %>% 
  group_by(state) %>%
  summarise_at(vars(estimate), funs(sum(., na.rm=TRUE))) %>% 
  left_join(state_pops, by = "state") %>% 
  mutate(proportion = estimate / population)


ggplot(at_least_bachelors) +
  geom_bar(aes(proportion, reorder(state, -proportion)), stat = "identity") +
  ggtitle("Proportion of at least bachelor degrees by State") +
  ylab("State") +
  xlab("Proportion of at least Bachelor Degrees")



#Pull income and poverty data to compare to education
inc <- get_acs_combo(
  table = "B19013",
  year = 2018,
  geography = "state",
  state = "US",
  survey = "acs5",
  use.parallel = T
)

inc <- rename(inc, inc_estimate = estimate)

poverty <- get_acs_combo(
  table = "B23024",
  year = 2018,
  geography = "state",
  state = "US",
  survey = "acs5",
  use.parallel = T
)

#find each state population
state_pov_pops <- poverty %>% 
  filter(level == 1) %>% 
  rename(pov_population = estimate)

pov <- poverty %>% 
  filter(levlab == "Income in the past 12 months below poverty level") %>% 
  rename(pov_estimate = estimate) %>% 
  left_join(state_pov_pops %>% dplyr::select(state, pov_population), by = "state") %>% 
  mutate(pov_prop = pov_estimate / pov_population)

education <- edu %>% 
  group_by(state, levlab) %>% 
  summarise_at(vars(estimate), funs(sum(estimate, na.rm = TRUE))) %>% 
  left_join(state_pops, by = "state") %>% 
  mutate(proportion = estimate / population)


edu_pov_inc <-
  left_join(pov,
            inc %>% dplyr::select(state, inc_estimate),
            by = "state") %>% 
  left_join(education,
            pov_inc %>% dplyr::select(state, inc_estimate, pov_prop),
            by = "state") %>% 
  rename(edu_estimate = estimate)


#Look at just those with bachelor degrees
bachelor <- edu_pov_inc %>% 
  filter(levlab.y == "Bachelor's degree")

#plot the proportion that have bachelor degrees by income
ggplot(bachelor) +
  geom_point(aes(proportion, inc_estimate, color = state)) +
  geom_smooth(aes(proportion, inc_estimate)) +
  ggtitle("Bachelor's Degrees by Income") +
  xlab("Proportion that have bachelor's degrees") +
  ylab("Median income")
#The trend shows that states with higher income have a higher proportion
#of bachelor's degrees


#plot the proportion that have bachelor degrees by poverty
ggplot(bachelor, aes(proportion, pov_prop)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Bachelor's Degrees by Poverty") +
  xlab("Proportion that have bachelor's degrees") +
  ylab("Proportion below poverty level")



#Look at just those that didn't graduate hs
less_than_hs <- filter(edu_pov_inc, levlab.y == "Less than high school graduate")

ggplot(less_than_hs, aes(proportion, inc_estimate)) +
  geom_point() +
  geom_smooth()


ggplot(less_than_hs, aes(proportion, pov_prop)) +
  geom_point() +
  geom_smooth() +
  xlab("Proportion that didn't graduate high school") +
  ylab("Proportion below poverty level")











marital_status <- get_acs_combo(
  table = "B12001",
  year = 2018,
  geography = "state",
  state = "US",
  survey = "acs5",
  use.parallel = T
)

#Find each states overall population
marital_pop <- marital_status %>% 
  select(state, estimate, level) %>%
  filter(level == 1) %>% 
  rename(marital_pop = estimate)



male <- marital_status %>% 
  filter(label == "Estimate!!Total!!Male!!Now married") %>% 
  rename(male_est = estimate)

female <- marital_status %>% 
  filter(label == "Estimate!!Total!!Female!!Now married") %>% 
  rename(female_est = estimate)

married <- male %>% 
  left_join(female %>% dplyr::select(state, female_est),
            by = "state") %>% 
  mutate(total_est = female_est + male_est) %>% 
  left_join(marital_pop %>% dplyr::select(state, marital_pop),
            by = "state") %>% 
  mutate(married_prop = total_est / marital_pop)







ggplot(married) +
  geom_bar(aes(married_prop, reorder(state, -married_prop)), fill = "darkblue", stat = "identity")


marital_edu <- married %>% 
  left_join(bachelor %>% dplyr::select(state, edu_estimate, proportion),
            by = "state") %>% 
  rename(bachelor_estimate = edu_estimate) %>% 
  rename(bachelor_prop = proportion)





age <- get_acs_combo(
  table = "B01002",
  year = 2018,
  geography = "state",
  state = "US",
  survey = "acs5",
  use.parallel = T
)

age <- filter(age, levlab == "Total") %>% 
  rename(age_est = estimate)

ggplot(age) +
  geom_bar(aes(age_est, y = reorder(state, -age_est)), stat = "identity")




master <- age %>% 
  left_join(married %>% dplyr::select(state, married_prop),
            by = "state") %>% 
  left_join(inc %>% dplyr::select(state, inc_estimate),
            by = "state") %>% 
  left_join(pov %>% dplyr::select(state, pov_prop),
            by = "state")

ggplot(master) +
  geom_point(aes(inc_estimate, pov_prop)) +
  geom_smooth(aes(inc_estimate, pov_prop))

