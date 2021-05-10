install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)
installed.packages("ggplot2")
library(ggplot2)
install.packages("ggplot")
library(ggplot2)
install.packages("ggplot2", dependencies = TRUE)

NBA_salaries = subset(NBA_salaries, select = -c(1, 4) )

NBAsalaries <- NBA_2018_2019_Salaries[-c(11, 22, 33, 44, 55, 66, 77, 88, 99, 110, 121, 142, 153, 164, 175, 186, 197, 208, 219, 230, 241, 252, 263, 274, 285, 296, 307, 318, 329, 340, 351, 362, 373, 384, 395, 406, 417, 428, 439, 450, 461, 472, 483, 494, 505, 516, 527, 538, 549),]

NBAplayerstats2 <- NBAplayerstats %>% 
  rename(
   "NAME" = 1, "TEAM" = 2, "Position" =  3, "Age" = 4, "GP" = 5, "MPG" = 6,  "Percentage of Team Minutes Used" = 7, "Usage Rate" = 8, "Turnover Rate" = 9, "FTA" = 10, "FT%" = 11, "2PA" = 12, "2P%" = 13, "3PA" = 14, "3P%" = 15, "Effective Shooting %" = 16, "True Shooting %" = 17, "Points Per Game" = 18, "Rebounds Per Game"= 19, "Total Rebound Percentage" = 20, "Assists Per Game" = 21, "Assists %" = 22, "Steals Per Game" = 23, "Blocks Per Game" = 24, "Turnovers Per Game" = 25, "Versatility Index" = 26, "Offensive Rating" = 27, "Defensive Rating" = 28)  

colnames(NBAplayerstats)

NBAsalaries <- NBA_salaries %>% 
  rename("NAME" = 1, "Salary ($)" = 2)

NBAplayerstats <- subset (X2018_2019_NBA_Player_Stats, select = -1)

#join datasets

NBA1 <- NBAsalaries %>% 
  full_join(NBAplayerstats2, by = c("NAME"))

library(tidyr)
install.packages("tidyr")

NBAclean <- NBA1 %>% drop_na()


#summary statistics 

NBAteams <- NBA2 %>% group_by(TEAM) %>% summarise(mean(`Salary ($)`))

filter(NBA2, species == "Human") %>% summarise(salary)

mean(NBA2$`Points Per Game`, na.rm = TRUE)   

str(NBA2&'Points Per Game')

as.numeric(NBA2$`Points Per Game`)

i <- c(2, 5:29)




mean("Points Per Game")

NBA2 %>% mutate_if(is.character, as.numeric)

NBA3 %>% summarise(mean(`Points Per Game`, na.rm = TRUE))
NBA3 %>% summarise(mean(`Age`, na.rm = TRUE))

young_and_high_scoring <- filter(NBA3, "Points Per Game" > 9.97) %>% filter()

NBA3 <- NBAclean %>%
  mutate_all(type.convert) %>%
  mutate_if(is.factor, as.character) 

is.numeric(NBA2$`Points Per Game`)

NBA3 <- data.frame(apply(NBA2, function(x) as.numeric(as.character(x))))


NBA4 <- NBA3 %>%
  filter(`Points Per Game` > 9.97) %>% arrange(desc(Age))

install.packages("kableExtra")
library(kable)  

library(kableExtra)

install.packages("kableExtra", dependencies = TRUE)
install.packages("hms")
install.packages("kableExtra", dependencies = TRUE)
(Yes)
yes
library(kable)
devtools::install_github("haozhu233/kableExtra")
cor()

ggplot()

NBA42 <- NBA3 %>%
  mutate(
    Points_Per_Minute = `Points Per Game`/`MPG`)

NBA42 %>% group_by(TEAM) %>% 

mean(NBA_grouped$`Salary ($)`)

%>% summarise(mean(`Salary ($)`)) 
NBA42 %>% group_by(TEAM) %>% summarise(sd(`Salary ($)`)) 

%>% summarise(var(`Salary ($)`))

NBA42 %>%                                        # Specify data frame
  group_by(TEAM) %>%                         # Specify group indicator
  summarise_at(vars(`Salary ($)`),              # Specify column
               list(name = mean))


NBA42 %>%                                        # Specify data frame
  group_by(TEAM) %>%                         # Specify group indicator
  summarise_at(vars(`Salary ($)`),              # Specify column
               list(name = sd))

NBA42 %>% summarise(group_by(TEAM),
          mean=mean(value), sd=sd(value))


NBA42 %>% group_by(TEAM) %>% summarise(sd(`Salary ($)`))

mean(NBA42$`Salary ($)`)
sd(NBA42$`Salary ($)`)
var(NBA42$`Salary ($)`)
n(NBA42$`Salary ($)`)
quantile(NBA42$`Salary ($)`)                                                  
n_distinct(NBA42$`Salary ($)`)
cor(NBA42$`Salary ($)`, NBA42$Age)


our_summary1 <-
  list("Salary" =
         list("mean"       = ~ mean(NBA42$`Salary ($)`),
              "sd"       = ~ sd(NBA42$`Salary ($)`),
              "variance" = ~ var(NBA42$`Salary ($)`)), 
       
       summary_table(NBA42, summaries = our_summary1 )      
 
       
             
       "Displacement" =
         list("min"       = ~ min(disp),
              "median"    = ~ median(disp),
              "max"       = ~ max(disp),
              "mean (sd)" = ~ qwraps2::mean_sd(disp)),
       "Weight (1000 lbs)" =
         list("min"       = ~ min(wt),
              "max"       = ~ max(wt),
              "mean (sd)" = ~ qwraps2::mean_sd(wt)),
       "Forward Gears" =
         list("Three" = ~ qwraps2::n_perc0(gear == 3),
              "Four"  = ~ qwraps2::n_perc0(gear == 4),
              "Five"  = ~ qwraps2::n_perc0(gear == 5))
       
       install.packages("reshape2")
       library(reshape2)
       melted_cormat <- melt(cormat)
       head(melted_cormat)
       
       
       cormat <- (cor(NBA42))
       head(cormat)
       
       NBA42 <- data.frame(apply(NBA3, function(x) as.numeric(as.character(x))))\
       
       NBA5 <- NBA3 %>%
         mutate_all(type.convert) %>%
         mutate_if(is.factor, as.character) 
       
       res <- cor(NBA42)
       round(res, 2)
       
       
       cor(NBA_numeric) %>%
         # Save as a data frame
         as.data.frame %>%
         # Convert row names to an explicit variable
         rownames_to_column %>%
         # Pivot so that all correlations appear in the same column
         pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>%
         # Specify variables are displayed alphabetically from top to bottom
         ggplot(aes(rowname, factor(other_var, levels = rev(levels(factor(other_var)))), fill=correlation)) +
         # Heatmap with geom_tile
         geom_tile() +
         # Change the scale to make the middle appear neutral
         scale_fill_gradient2(low="red",mid="white",high="blue") +
         # Overlay values
         geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +
         # Give title and labels
         labs(title = "Correlation matrix for Stats", x = "", y = "") 
       
       NBA_numeric1 <- NBA3 %>% 
         #drop ID and registration column
         select(-c(1, 4, 9, 10, 11, 12,13, 24, 25,26 )) %>% 
         #keep only numeric variables
         mutate_if(is.numeric, scale)     



NBA_numeric <- NBA3 %>% 
  #drop ID and registration column
  select(-c(1,3, 4, 9, 10, 11, 12,13, 24, 25,26 )) %>% 
  #keep only numeric variables
  mutate_if(is.numeric, scale)
# calculate the mean 

ggplot(data = yearly_counts, aes(x = year, y = n, color = genus)) +
  geom_line()

yearly_counts_graph <- surveys_complete %>%
  count(year, genus) %>% 
  ggplot(mapping = aes(x = year, y = n, color = genus)) +
  geom_line()

yearly_counts_graph

ggplot() + geom_point(data = NBA3, aes(x = "Salary ($)", y = "MPG")) 

ggplot() + geom_point(data = NBA3, aes(x = NBA3$`Salary ($)`, y = NBA3$Age, color = NBA3$TEAM))

str(NBA3)



