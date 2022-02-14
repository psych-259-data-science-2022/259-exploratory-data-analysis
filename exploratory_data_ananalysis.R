# Load packages
library(tidyverse)
library(DataExplorer)

# Set up the file we'll be using

ds <- read_csv("data-raw/raw_data.csv")
demo <- readxl::read_excel("data-raw/participant_demo.xlsx", sheet = "Data") %>% rename_all(janitor::make_clean_names)
ds <- ds %>% pivot_longer(-id, names_to = "variable", values_to = "value") %>% 
  mutate(condition = ifelse(str_detect(variable, "walk"), "walk", "search"),
         variable = str_remove_all(variable, "walk_"),
         variable = str_remove_all(variable, "search_"))
ds <- ds %>% pivot_wider(id_cols = c("id", "condition"), names_from = "variable", values_from = "value")
ds <- left_join(ds, demo, by = c("id" = "subject_number"))
ds <- ds %>% relocate(test_date:humidity, .after = condition)

# Example data for today

glimpse(ds)

# Things we already know how to do

# Filter based on expected ranges
ds %>% select(id, age) %>% filter(age < 18 | age > 22)
# Filter based a set of possible values
ds %>% select(id, handedness) %>% filter(!(handedness %in% c("Right", "Left")))

# Get summary stats
ds %>% summarize(age_min = min(age), mean_age = mean(age), max_age = max(age))
ds %>% drop_na(age) %>% summarize(age_min = min(age), mean_age = mean(age), max_age = max(age))
summary(ds$age) #Also a nice option

# Check the number of participant ids
length(unique(ds$id)) # Should be 30
# Count
count(ds, sex)

## Data Explorer: Histograms

ds %>% select(condition, gazex_std:posy_speed) %>% plot_histogram()

## Data Explorer: Density plots

ds %>% select(condition, gazex_std:posy_speed) %>% plot_density()

## Data Explorer: Boxplots by condition

ds %>% select(condition, gazex_std:posy_speed) %>% plot_boxplot(by = "condition")

## Data Explorer: Plot missing

ds %>% select(id:humidity) %>% plot_missing()

## Data Explorer: Correlations

ds %>% select(condition, gazex_std:posy_speed) %>% plot_correlation()

## Data Explorer: Create Report

ds %>% create_report()
# Saves an html report to your working directory

# What type of aesthetic mapping depends on what type of geom

ggplot(ds, aes(x = posy_std)) + 
  geom_histogram() + 
  xlab("Average vertical head position")

ggplot(ds, aes(x = posy_std)) + 
  geom_density() + 
  xlab("Average vertical head position")

ggplot(ds, aes(x = posy_std)) + 
  geom_boxplot() + 
  xlab("Average vertical head position")

# Add a category

ggplot(ds) + geom_boxplot(aes(y = posy_std)) 
ggplot(ds) + geom_boxplot(aes(x = condition, y = posy_std))
ggplot(ds) + geom_boxplot(aes(x = condition, y = posy_std, fill = condition))
ggplot(ds) + geom_boxplot(aes(x = condition, y = posy_std, fill = target)) 

# Simple scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean)) + 
  geom_point() + 
  xlim(-20, 20) + 
  ylim(-20, 20) + 
  xlab("Mean horizontal head position") + 
  ylab("Mean vertical head position") +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0)

# Add category to scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean, color = condition)) + 
  geom_point()

# Facetting a scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean)) + 
  geom_point() + 
  facet_wrap("condition", ncol = 1)

# Saving plots

ggplot(ds, aes(x = posx_mean, y = posy_mean)) + geom_point()
#ggsave will save the last plot to file
ggsave("eda/head-position-scatter.jpg")

#You can also save plots to objects (they are lists)
p1 <- ggplot(ds, aes(x = posx_mean, y = posy_mean)) + geom_point()
ggsave("eda/head-position-scatter.jpg", plot = p1)

# Automation + plotting

two_var_scatter <- function(df, var1, var2) {
  df_to_plot <- df %>% 
    select(c(var1, var2)) %>% 
    rename(var_1 = 1, var_2 = 2)
  p <- ggplot(df_to_plot, aes(x = var_1, y = var_2)) + 
    geom_point() + 
    xlab(var1) + 
    ylab(var2)
  ggsave(str_glue("eda/{var1}_{var2}.jpg"), plot = p)
  return(p)
}
two_var_scatter(ds, "posx_std", "posy_std")

# Automation + plotting

pred_x <- colnames(select(ds, posx_mean:eyey_speed)) %>% keep(str_detect, "x_")
pred_x
pred_y <- colnames(select(ds, posx_mean:eyey_speed)) %>% keep(str_detect, "y_")
pred_y
pred_x_by_y <- map2(pred_x, pred_y, ~ two_var_scatter(ds, .x, .y))
pred_x_by_y
