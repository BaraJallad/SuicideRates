
#############################################################################
# The final project of the 9th course 'Capstone' in the Data Science Program.
# Suicide Rates Overview 1985 to 2016 #######################################
#############################################################################


# Note: this process could take a couple of minutes

#installing the packges needed in the solution
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

#load the libraries required
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(lubridate)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%






# Set path to the directory that contains the dataset
path <- "."
filename <- "master.csv"
fullpath <- file.path(path, filename)
Suicide_Rates <- read.csv(fullpath)

# Lets find the structure of the data frame
str (Suicide_Rates)

# Rename the column
Suicide_Rates <- rename (Suicide_Rates, "rate" = "suicides.100k.pop",
                         "country" = "ï..country"                   )


# Lets find the header and the contant of the data frame
head (Suicide_Rates)
nrow(Suicide_Rates)

n_distinct(Suicide_Rates$country ) 
n_distinct(Suicide_Rates$year    )
n_distinct(Suicide_Rates$generation    )
n_distinct(Suicide_Rates$age     )

#Replace the NA values with 0’s using replace() in R
Suicide_Rates[is.na(Suicide_Rates)]<-0


# Analyst Data

# Plot between years and suicides no (sex)

df_sex <- Suicide_Rates %>%  group_by(year, sex) %>% summarise(suicides_no = sum (suicides_no), population=sum(population))
df_sex %>%  
  ggplot(aes(year,suicides_no*100/population, col = sex)) +
  geom_line()


#No matter which year it is, the suicides number of male are about three times higher than of female. 


# Plot between years and suicides no (age) 


df_sum <- Suicide_Rates %>%  group_by(year, age) %>% summarise(suicides_no = sum (suicides_no), population=sum(population))
df_sum %>%  
  ggplot(aes(year,suicides_no*100/population, col = age)) +
  geom_line()

#Obviously, the suicide rate is getting higher when the age is higher. That is, age is a factor of suicide.

df_generation <- Suicide_Rates %>%  group_by(year, generation) %>% summarise(suicides_no = sum (suicides_no), population=sum(population))
df_generation %>%  
  ggplot(aes(year,suicides_no*100/population, col = generation)) +
  geom_line()
#Before 2000, we can see that the highest suicide rate is G.I. generation, and this generation is also known as WW2 generation. They suffered from the worldwide great depression before WW2, at this time, the income, profit, taxes are decreased seriously, so this generation experienced economic and social turmoil.



#Obviously, the suicide rate is getting higher when the age is higher. That is, age is a factor of suicide.

df_new_country <- Suicide_Rates %>%  group_by( country) %>% summarise(rate = sum(rate))

df_new_country %>% 
  ggplot(aes(rate,country),  width = 8, height = 300, res=36) + 
  geom_bar(stat="identity", color = "black")


which.max(df_new_country$rate)
df_new_country[76,]
which.min(df_new_country$rate)
df_new_country[28,]





# Validation set will be 10% of Suicide Rates data
#set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

ds <- Suicide_Rates %>% select (country,year,sex,age,rate,generation)
set.seed(1)
test_index <- createDataPartition(y = ds$rate, times = 1, p = 0.1, list = FALSE)
edx <- ds[-test_index,]
temp <- ds[test_index,]

# Make sure country,sex and age in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "country") %>%
  semi_join(edx, by = "age") %>%  
  semi_join(edx, by = "sex")


# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)



#rm is used to delete the unnecessary data to focus on what we are working on
rm( test_index, temp, ds, removed, plot1)


##-------------------

#create function that computes the RMSE for vectors 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



#create data frame to save the results on it
the_final_results <- tibble ()
#find the mean of raiting
mu <- mean(edx$rate)  
mu


##### Model 1 - Average #####
#Simplest possible model

# calculate the average rating
mu_hat <- mean(edx$rate)
mu_hat
# calculate rmse for model
average_rmse <- RMSE(validation$rate, mu_hat)
average_rmse

predictions <- rep(12, nrow(validation))
RMSE(validation$rate, predictions)
# create a table to display all the calculated rmses
the_final_results <- tibble(method = "Just the average", RMSE = average_rmse)






##### Model 2 - Country Effect #####
#we are going to group the data by country to find  c_m for each country with the equation mean(rate - average)
country_effect <- edx %>% 
  group_by(country) %>% 
  summarize(c_m = mean(rate - mu_hat)) 

#take test dataset and calculate predicted rating
country_pred_rate <- validation %>% 
  left_join(country_effect, by ="country") %>% 
  mutate(predicted_rating = mu_hat + c_m) %>% 
  pull(predicted_rating)  
# calculate rmse for model
rmse_country_effect <- RMSE(validation$rate, country_pred_rate) 
#Add the rmse results to the data frame
the_final_results <- bind_rows(the_final_results,
                               tibble(method = "Average + Country Effect", RMSE = rmse_country_effect)) 
rmse_country_effect








##### Model 3 - age Effect #####
#we are going to group the data by age to find  a_m for each age with the equation mean(rate - average)
age_effect <- edx %>% 
  group_by(age) %>% 
  summarize(a_m = mean(rate - mu_hat)) 

#take test dataset and calculate predicted rating
age_pred_rate <- validation %>% 
  left_join(age_effect, by ="age") %>% 
  mutate(predicted_rating = mu_hat + a_m) %>% 
  pull(predicted_rating)  
# calculate rmse for model
rmse_age_effect <- RMSE(validation$rate, age_pred_rate) 
#Add the rmse results to the data frame
the_final_results <- bind_rows(the_final_results,
                               tibble(method = "Average + Age Effect", RMSE = rmse_age_effect)) 
rmse_age_effect







##### Model 4 - year Effect #####
#we are going to group the data by year to find  y_m for each year with the equation mean(rate - average)
year_effect <- edx %>% 
  group_by(year) %>% 
  summarize(y_m = mean(rate - mu_hat)) 

#take test dataset and calculate predicted rating
year_pred_rate <- validation %>% 
  left_join(year_effect, by ="year") %>% 
  mutate(predicted_rating = mu_hat + y_m) %>% 
  pull(predicted_rating)  
# calculate rmse for model
rmse_year_effect <- RMSE(validation$rate, year_pred_rate) 
#Add the rmse results to the data frame
the_final_results <- bind_rows(the_final_results,
                               tibble(method = "Average + Year Effect", RMSE = rmse_year_effect)) 
rmse_year_effect



##### Model 5 - Country Regularization ##### 

lambdas <- seq(0, 10, 0.25) # define a set of lambdas to test


#calculate rmses for all defined lambdas by creating a function that predicte the rating and return rmses for each lambda
reg_country_rmses <- sapply(lambdas, function(l){  
  e_c <- edx %>% 
    group_by(country) %>% 
    summarize(e_c = sum(rate - mu_hat) / (n() + l))  
  predicted_ratings <- validation %>% 
    left_join(e_c, by = "country") %>%
    mutate(pred = mu_hat + e_c) %>% 
    pull(pred) 
  return(RMSE(validation$rate, predicted_ratings))  
})

# return minimum rmse
rmse_reg_country_effect <- min(reg_country_rmses) 
# add calculated rmse to rmse table
the_final_results <- bind_rows(the_final_results,
                               tibble(method = "Average + Country Effect + Regularization", RMSE = rmse_reg_country_effect)) 
rmse_reg_country_effect





##### Model 6 - generation Effect #####
#we are going to group the data by generation to find  g_m for generation year with the equation mean(rate - average)
generation_effect <- edx %>% 
  group_by(generation) %>% 
  summarize(g_m = mean(rate - mu_hat)) 

#take test dataset and calculate predicted rating
generation_pred_rate <- validation %>% 
  left_join(generation_effect, by ="generation") %>% 
  mutate(predicted_rating = mu_hat + g_m) %>% 
  pull(predicted_rating)  
# calculate rmse for model
rmse_generation_effect <- RMSE(validation$rate, generation_pred_rate) 
#Add the rmse results to the data frame
the_final_results <- bind_rows(the_final_results,
                               tibble(method = "Average + Generation Effect", RMSE = rmse_generation_effect)) 
rmse_generation_effect

the_final_results

#export the data as csv 
write.csv(validation %>% select(country, year,sex,age,  rate,      generation) %>% mutate(rating = generation_pred_rate),
          "Predicted.csv", na = "", row.names=FALSE)