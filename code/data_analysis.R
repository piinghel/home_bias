
# clean environment
rm(list=ls())
# load libraries
library(tidyverse)
library(readxl)
library(lubridate)

# plotting
library(patchwork)
library(viridis)
library(ggthemes)
library(RColorBrewer)



#---------------------------
# Global parameters
cbpalette <- RColorBrewer::brewer.pal(n = 8,name = "Dark2")




# TODO total number of firms per region is constant (ask professor)
# also I think that using counts is more informative than % since it gives
# you more information

df <- read_excel("data/data_rating.xlsx",sheet = "Long") %>%
  # make everything lowercase
  dplyr::rename(date = Date,
                pd = PD,
                moodys = Moodys,
                sp = SP,
                region = Region,
                sector = Sector)

df <- df %>% mutate(
  # compute the log of probability of default (pd)
  pd_log = log(pd),
  year = lubridate::year(date),
  month = lubridate::month(date)
)

#----------------------------------------
# 1) PROBABITLITY OF DEFAULT (PD)
#----------------------------------------
# overall distribution of PD
ggplot(df, aes(x = pd_log)) + 
  geom_density(alpha=.7, fill=cbpalette[1]) + 
  labs(x = "Probability of default (PD)") 


# condition  by region
ggplot(df, aes(x = pd_log, fill = region)) + 
  geom_density(alpha=.7) + labs(x = "Probability of default (PD)") +
  scale_fill_brewer(palette="Dark2") 

# condition by sector
ggplot(df, aes(x = pd_log, fill = sector)) + 
  geom_density(alpha=.7) + labs(x = "Probability of default (PD)") +
  scale_fill_brewer(palette="Dark2") 




# visualize over time by sector
df %>% dplyr:: group_by(date,sector,region) %>%
  dplyr::summarise(
  central_measure_pd = median(pd,na.rm = TRUE)) %>%
  # plot
  ggplot(.,aes(x = date,y = central_measure_pd, color = sector)) + 
  geom_line() + geom_point() +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(trans = "log10") + 
  facet_grid(region~.,scales = "free_y")


#----------------------------------------
# 2) Standards and Poor (sp) and Moodys
#----------------------------------------

p1 <- df %>% select(id,sp,region,year) %>% distinct(id, year, region) %>% 
  dplyr::group_by(year, region) %>%
  tally() %>% ggplot(.,aes(x = year, y = n, color = region)) + 
  geom_point() + geom_line() + labs(x = "",y = "Unique firms") + 
  ggtitle("S&P") 


p2 <- df %>% select(id, moodys, region, year) %>% distinct(id, year, region) %>% 
  dplyr::group_by(year, region) %>%
  tally() %>% ggplot(.,aes(x = year, y = n, color = region)) + 
  geom_point() + geom_line() + labs(x = "",y = "") +
  ggtitle("Moody's")



# number of observations per year
p3 <- df %>% select(id,sp,region,year) %>% dplyr::group_by(year,region) %>%
  tally() %>% ggplot(.,aes(x = year, y = n, color = region)) + 
  geom_point() + geom_line() + labs(x = "Year",y = "Observations") +
  ggtitle("S&P") 



# number of observations per year
p4 <- df %>% select(id,moodys,region,year) %>% dplyr::group_by(year,region) %>%
  tally() %>% ggplot(.,aes(x = year, y = n, color = region)) + 
  geom_point() + geom_line() + labs(x = "Year",y = "") +
  ggtitle("Moody's") 


(p1 + p2)/(p3 + p4) + plot_layout(guides = "collect")


# sp
p5 <- df %>% select(id,sp,year) %>% distinct(id,sp,year) %>%
  group_by(sp,year) %>% tally() %>% na.omit() %>% 
  ggplot(., aes(x = year, y = sp)) +
  geom_tile(aes(fill=n)) + 
  scale_x_continuous(breaks = c(2000:2021),
  expand = c(0,0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  labs(y = "Rating category") + ggtitle("sp")
  
# Moodys
p6 <-df %>% select(id,moodys,year) %>% distinct(id,moodys,year) %>%
  group_by(moodys,year) %>% tally() %>% na.omit() %>% 
  ggplot(., aes(x = year, y = moodys)) +
  geom_tile(aes(fill=n)) + 
  scale_x_continuous(breaks = c(2000:2021),
  expand = c(0,0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  labs(y = "") + ggtitle("Moodys")


p5 + p6



# Do the same but make distinction between Europe and US






