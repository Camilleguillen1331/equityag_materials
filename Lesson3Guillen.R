# title: "Lesson 3: Workflow and Statistical Test in R"
# author: Camille Guillen 
# date: July 18 2025

# load packages
# for reading parquet files
library(arrow)

# for data manipulation and visualization
library(tidyverse)

# load in the data set using the arrow package 
merged_data <- read_parquet(file = "merged_data.parquet", stringsAsFactors = TRUE)

# initial exploration with visualization 
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

# remove anomalies
merged_data <- merged_data[!(merged_data$STATE_NAME_2010SVI %in% "06"), ]

# re plot the box plot to confirm the issue is resolved 
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

# box plot to explore total population by state 
boxplot(formula = M_TOTPOP_2010SVI ~ STATE_NAME_2010SVI, data = merged_data) 
# this blox plot gives us the overview of the total population and can be a good thing to visualize to compare total population with the different upward mobility rates
# i feel like the patterns between these can be investigated further in order to see what specific variables we may want to look in to further

? boxplot

# introduction to tidyverse and advanced data handling

# throwing an error 
boxplot(formula = M_TOTPOP_2010SVI ~ state_NAME_2010SVI, data = merged_data) # this doesnt work because of the capitalization

# using tidyverse for summarizing data 
# traditional base r methods 

#1. First, create two new datasets by filtering the rows based on the state abbreviation.
subset_data_ca <- subset(merged_data, STATE_ABBR_2010SVI == "CA")
subset_data_az <- subset(merged_data, STATE_ABBR_2010SVI == "AZ")

#2.Calculate the average upward mobility rate in 2010 for each state using the mean() function.
print(upward_mean_2010_ca <- mean(subset_data_ca$upward_mobility_rate_2010))
print(upward_mean_2010_az <- mean(subset_data_az$upward_mobility_rate_2010))

#3. Once we have both means, we store them in a new data frame so they’re easier to use later.
upward_mobility_means_2010 <- data.frame(State = c("CA", "AZ"),
                                         up_2010_mean = c(upward_mean_2010_ca, upward_mean_2010_az))
# This process works, but it’s long and repetitive. Imagine doing this for all 50 states!
# Let’s use tidyverse to make this more efficient.

# tidyverse method 
# Group the full dataset by state abbreviation, then calculate the mean upward mobility rate for each group.
state_group <- merged_data %>%
  group_by(STATE_ABBR_2010SVI)

state_mob_means <- state_group %>%
  summarise(up_mean = mean(upward_mobility_rate_2010, na.rm = TRUE))

# Often, there are missing values (NA) that we don't want in our results.
# Let’s remove any rows where the state abbreviation is missing.
state_mob_means <- state_mob_means %>%
  filter(!is.na(STATE_ABBR_2010SVI))
#This is much faster and cleaner than calculating each state manually. Now state_mob_means is the same as upward_mobility_means_2010

# pipe operator example 
upward <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarise(means_2010 = mean(upward_mobility_rate_2010, na.rm = TRUE))
# removes rows where the state is NA
upward <- upward %>% filter(!is.na(STATE_ABBR_2010SVI))

# adding error plots with ggplot2
# visualize means and standard error 
upward_stats <- merged_data %>%
  group_by(STATE_ABBR_2010SVI, COUNTY_2010SVI) %>%
  summarise(up_means = mean(upward_mobility_rate_2010, na.rm = TRUE),
            up_se = sd(upward_mobility_rate_2010, na.rm = TRUE)/sqrt(n()))
# basic ggplot with error bars
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means))
                                                                  
# adding components to plot 
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point()

# add error bars
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se))
# make error bars thinner
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se), 
                width = 0.30)
# the graph looks busy because there's a lot of data points and lines running through the data. we could clean it up by possibly making is viusally easier to read by adding color or even thinning out the bars more. 

# lets clean it up by state 
upward_stats_st <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarize(up_means = mean(upward_mobility_rate_2010),
            up_se = sd(upward_mobility_rate_2010)/sqrt(n()))

# remember to drop all of the N/A
upward_stats_st <- upward_stats_st %>% filter(!is.na(STATE_ABBR_2010SVI))

# re doing the graph to check the changes
ggplot(data = upward_stats_st, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se), 
                width = 0.30)
# this gives us much cleaner and easier to read visualization
# more todyverse tricks 
# create a new working copy of the data set 
merged <- merged_data

# selecting columns 
merged %>% 
  dplyr:: select(!contains("_"), starts_with ("upward"))

# reorder columns 
merged <- merged %>%
  dplyr:: relocate(contains("STATE"), .after = upward_mobility_rate_2020)

# create and place a unique identifier 
unique_id <- merged %>%
  dplyr:: group_by(STATE_FIPS_2010SVI, COUNTY_2010SVI, CENSUSAREA_2010SVI) %>%
  dplyr:: mutate(uniqueid = row_number(), .before = contains("_"))

# pivoting from wide to long
#Wide to long
# merged_long <- unique_id %>%
#tidyr::pivot_longer(contains("_"),
#names_to = c("upward"),
#names_sep =("_"))


#this is general synthax, but doesn't make sense for what we're doing right now

# Long to wide

# merged_wide <- _merged_long %>%
#tidyr::pivot_wider(names_from = c("upward),
#values_from = "value",
#names_sep = "_")


#again, as we didn't change, muted code


# summarize across multiple columns 
merged_stats_up <- merged %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"),
                          list(~mean(.x, na.rm = TRUE), 
                               ~sd(.x, na.rm = TRUE)))) 

# rename columns when summarizing
merged_stats_up <- merged %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"), 
                          list(mean = ~mean(.x, na.rm = TRUE), 
                               sd = ~sd(.x, na.rm = TRUE)),
                          .names = "{gsub('_', '', col)}_{fn}")) 

# modeling and nesting 
upward_models <- merged %>%
 group_by(STATE_ABBR_2010SVI) %>% summarise(model list(lm(upward_mobility_rate_2010 ~ POP2010)))

# add nest within data
merged <- nest_by(state_group)

# running a basic t test
merged <- merged_data 

# t test upward mobility AZ and CA
az <- merged_data[merged_data$STATE_NAME_2010SVI == "Arizona", ]
ca <- merged_data[merged_data$STATE_NAME_2010SVI == "California", ]

#we can see how many rows and columns are in a data.frame with the dim() command
dim(merged_data)

#this returns all columns for the third row in merged_data:
merged_data[3,]

#or the third column:
merged_data[,3]

# check how many observations we have in each
print(nrow(az)) 
print(nrow(ca))

# is the upward mobility rate in these two states significantly different?
t.test(x = az$upward_mobility_rate_2010, y = ca$upward_mobility_rate_2010)

# example interpreatation
#t = -4.92, df = 1700, p-value = 9.2e-07
#95 percent confidence interval: -0.035 to -0.015
#mean of x: 0.120
#mean of y: 0.145

# ANOVA
aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

mobility_rate_az_aov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

# open the model that we have saved
summary(object = mobility_rate_az_aov)

# save output to file??? use sink
sink(file = "output/az_mobility_anova.txt")
summary(object = mobility_rate_az_aov)
sink()

# linear regression

# take a quick look at our data
summary(merged_data[, 1:10], 6) 

# plot to visualize instead
plot(x = merged_data$upward_mobility_rate_2010, y = merged_data$M_TOTPOP_2010SVI)

# make a new plot with the transformed data 
merged$logpop <- log10(merged$M_TOTPOP_2010SVI)

# plot this again
plot(x = merged$upward_mobility_rate_2010, 
     y = merged$logpop, 
     xlab = "Upward Mobility", 
     ylab = "log10(Population)")

# run a linear model
mobility_v_pop <- lm(upward_mobility_rate_2010 ~ logpop, data = merged)

# let’s look at the results:
summary(mobility_v_pop)

# save to file instead of printing them 
sink(file = "output/mobility-pop-regression.txt")
summary(mobility_v_pop)
sink()

# add another variable to analysis 
merged$az <- ifelse(merged$STATE_NAME_2010SVI == "Arizona", 1, 0)
merged$ca <- ifelse(merged$STATE_NAME_2010SVI == "California", 1, 0)

# add AZ as a predictor and run analysis
mobility_v_pop_state <- lm(formula = upward_mobility_rate_2010 ~ logpop + az
                           , data = merged)
summary(mobility_v_pop_state)

# save it
sink(file = "output/mobility-pop-state-regression.txt")
summary(mobility_v_pop)
sink()

# student activity 


# variable exploration
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)
boxplot(formula = M_TOTPOP_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)
# made one with the census data 
boxplot(formula = CENSUSAREA_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)

# student exploration 
# I am going to do upward mobility rate for 2020 

# filter the data for AZ
az_data <- merged %>% filter(STATE_ABBR_2010SVI == "AZ")

# doing more random investigation 
census_area_az_aov <- aov(formula = CENSUSAREA_2010SVI ~ COUNTY_2010SVI, data = az)

summary(census_area_az_aov)

# create the box plot 
ggplot(az_data, aes(x = COUNTY_2010SVI, y = upward_mobility_rate_2020)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    title = "Upward Mobility Rate (2020) by County in Arizona",
    x = "County",
    y = "Upward Mobility Rate (2020)"
  )
# this tells us the upward mobility rates for all the different counties in arizona for 2020.
# i was curious as to the differences in the different counties in arizona and how they vary, also interested in how that may change since each county is different and has different opoulation levels and economoc status. 
 
# test if upward mobilty varies by county in arizona
anova_vary_az <- aov(upward_mobility_rate_2020 ~ COUNTY_2010SVI, data = az_data)
summary(anova_vary_az)

# test the same for california 
# merge the data 
ca_data <- merged %>% filter(STATE_ABBR_2010SVI == "CA")
# test if upward mobility varies by county in california 
anova_vary_ca <- aov(upward_mobility_rate_2020 ~ COUNTY_2010SVI, data = ca_data)
summary(anova_vary_ca)
ca_data <- merged %>% filter(STATE_ABBR_2010SVI == "CA")

anova_ca <- aov(upward_mobility_rate_2020 ~ COUNTY_2010SVI, data = ca_data)
summary(anova_ca)
    
# upward mobility varies significantly across the counties 
# this could be due to different changes in communities as far as education and housing or other major factors

plot(x = merged_data$upward_mobility_rate_2020, y = merged_data$M_TOTPOP_2010SVI) + 

  mobility_v_pop_state <- lm(formula = upward_mobility_rate_2010 ~ logpop + az + CENSUSAREA_2010SVI
                             , data = merged)
summary(mobility_v_pop_state)

# Get a tidy data frame of model results
model_coefs <- tidy(mobility_v_pop_state, conf.int = TRUE)

# Remove intercept to focus on actual predictors (optional)
model_coefs <- model_coefs %>% filter(term != "(Intercept)")

# Plot coefficients with confidence intervals
ggplot(model_coefs, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "darkblue") +
  labs(
    title = "Regression Coefficients with 95% Confidence Intervals",
    x = "Predictor",
    y = "Estimate"
  ) +
  theme_minimal() +
  coord_flip()  # flip to make it horizontal for better readability


# reflect and extend 
# i definitely need more time to practice but I am happy with how this turned out. I think I did the student activity mostly correct, but I did play around with a lot of things so I am not sure if its exaclty to the directions.
# practice will make me more confident!!
# today w got a lot of information on how to read the exact code and that helped a lot, I really enjoyed that.
# i still have a few questions on how to look for help when stuck. 

# Get a tidy data frame of model results
model_coefs <- tidy(mobility_v_pop_state, conf.int = TRUE)

# Remove intercept to focus on actual predictors (optional)
model_coefs <- model_coefs %>% filter(term != "(Intercept)")

# Plot coefficients with confidence intervals
ggplot(model_coefs, aes(x = term, y = estimate)) +
  geom_point(size = 4, color = "purple") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.7, color = "pink") +
  labs(
    title = " CG Regression Coefficients with 95% Confidence Intervals",
    x = "Predictor",
    y = "Estimate"
  ) +
  theme_grey() +
  coord_flip()  # flip to make it horizontal for better readability



# random 
boxplot(formula = CENSUSAREA_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)
boxplot(CENSUSAREA_2010SVI ~ STATE_NAME_2010SVI, 
        data = merged_data,
        col = rainbow(length(unique(merged_data$STATE_NAME_2010SVI))),
        main = "Census Area by State",
        xlab = "State",
        ylab = "Census Area")

library(ggplot2)

ggplot(merged_data, aes(x = STATE_NAME_2010SVI, y = CENSUSAREA_2010SVI, fill = STATE_NAME_2010SVI)) +
  geom_boxplot() +
  labs(title = "Census Area by State",
       x = "State",
       y = "Census Area") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend if color is redundant with x-axis

library(ggplot2)

# Get the number of unique states
num_states <- length(unique(merged_data$STATE_NAME_2010SVI))

ggplot(merged_data, aes(x = STATE_NAME_2010SVI, y = CENSUSAREA_2010SVI, fill = STATE_NAME_2010SVI)) +
  geom_boxplot() +
  scale_fill_manual(values = rainbow(num_states)) +  # Apply rainbow colors
  labs(title = "Census Area by State",
       x = "State",
       y = "Census Area") +
  theme_tufte() +
  theme(legend.position = "none") +
  coord_flip()  # Optional: makes it easier to read

install.packages("ggthemes")
library(ggthemes)

library(ggplot2)

num_states <- length(unique(merged_data$STATE_NAME_2010SVI))

ggplot(merged_data, aes(x = STATE_NAME_2010SVI, y = CENSUSAREA_2010SVI, fill = STATE_NAME_2010SVI)) +
  geom_boxplot() +
  scale_fill_manual(values = rainbow(num_states)) +
  labs(title = "🌈 Census Area by State",
       x = "State",
       y = "Census Area") +
  theme_classic() +
  theme(
    plot.title = element_text(color = "pink", size = 16, face = "bold"),
    axis.text.x = element_text(color = "darkblue", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "darkgreen"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "lavender", color = NA)
  ) +
  coord_flip()



