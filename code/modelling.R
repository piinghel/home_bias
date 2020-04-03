# clear environment
rm(list = ls())
# load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(plyr)
# plotting
library(ggpubr)
library(patchwork)

#own functions
source("code/functions/compare_means.R")

#---------------------------
# Global parameters
PALETTE <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
STYLE <- "steelblue"

#---------------------------


df <- read_excel("data/data_rating.xlsx", sheet = "Long") %>%
  # make everything lowercase
  dplyr::rename(
    date = Date,
    pd = PD,
    moodys = Moodys,
    sp = SP,
    region = Region,
    sector = Sector
  )

df <- df %>% mutate(
  # compute the log of probability of default (pd)
  pd_log = log(pd + 0.000001),
  # prevent inf values
  year = lubridate::year(date),
  month = lubridate::month(date),
  sp = as.factor(ifelse(sp > 17, 17, sp)),
  moodys = as.factor(ifelse(moodys > 17, 17, moodys)),
  region = as.factor(region),
  sector = as.factor(sector),
  year = as.factor(year),
  month = as.factor(month)
)



# recode S&P
df$sp <- as.factor(mapvalues(
  df$sp,
  from = c("1","2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"
  ),
  to = c( "AAA","AA+","AA","AA-","A+","A","A-","BBB+","BBB","BBB-","BB+","BB","BB-","B+","B","B-","CCC+..."
  )
))
# recode Moodys
df$moodys <- as.factor(mapvalues(
  df$moodys,
  from = c("1","2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"
  ),
  to = c("Aaa","Aa1","Aa2","Aa3","A1","A2","A3","Baa1","Baa2","Baa3","Ba1","Ba2","Ba3","B1","B2","B3","Caa3..."
)
))


# 1) Visualize differences in mean by means of a boxplot

# 1)
compare_means(data = df,credit_agency = "sp",category = "AAA")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Aaa", title = "Moodys")$figure + 
  plot_layout(guides="collect")
# 2)
compare_means(data = df,credit_agency = "sp",category = "AA+")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Aa1", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 3)
compare_means(data = df,credit_agency = "sp",category = "AA")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Aa2", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 4)
compare_means(data = df,credit_agency = "sp",category = "AA-")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Aa3", title = "Moodys")$figure  +
  plot_layout(guides="collect")
# 5)
compare_means(data = df,credit_agency = "sp",category = "A+")$figure +
compare_means(data = df,credit_agency = "moodys",category = "A1", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 6)
compare_means(data = df,credit_agency = "sp",category = "A")$figure +
compare_means(data = df,credit_agency = "moodys",category = "A2", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 7)
compare_means(data = df,credit_agency = "sp",category = "A-")$figure +
compare_means(data = df,credit_agency = "moodys",category = "A3", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 8) 
compare_means(data = df,credit_agency = "sp",category = "BBB+")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Baa1", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 9)
compare_means(data = df,credit_agency = "sp",category = "BBB")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Baa2", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 10)
compare_means(data = df,credit_agency = "sp",category = "BBB-")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Baa3", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 11)
compare_means(data = df,credit_agency = "sp",category = "BB+")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Ba1", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 12)
compare_means(data = df,credit_agency = "sp",category = "BB")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Ba2", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 13)
compare_means(data = df,credit_agency = "sp",category = "BB-")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Ba3", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 14)
compare_means(data = df,credit_agency = "sp",category = "B+")$figure +
compare_means(data = df,credit_agency = "moodys",category = "B1", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 15)
compare_means(data = df,credit_agency = "sp",category = "B")$figure +
compare_means(data = df,credit_agency = "moodys",category = "B2", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 16)
compare_means(data = df,credit_agency = "sp",category = "B-")$figure +
compare_means(data = df,credit_agency = "moodys",category = "B3", title = "Moodys")$figure +
  plot_layout(guides="collect")
# 17)
compare_means(data = df,credit_agency = "sp",category = "CCC+...")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Caa3...", title = "Moodys")$figure +
  plot_layout(guides="collect")


# sp
for (rating in levels(df$sp)) {
  if (rating == "AAA") {
    # first time
    sp_statistics = compare_means(
      data = df,
      return_statistic =
        TRUE,
      credit_agency = "sp",
      category = rating
    )$statistics
  }
  else{
    out_sp <-
      compare_means(
        data = df,
        return_statistic = TRUE,
        credit_agency = "sp",
        category = rating
      )$statistics
    # update
    sp_statistics <- rbind(sp_statistics, out_sp)
    
  }
}

# moodys
for (rating in levels(df$moodys)) {
  if (rating == "Aaa") {
    # first time
    moodys_statistics = compare_means(
      data = df,
      return_statistic = TRUE,
      credit_agency = "moodys",
      category = rating
    )$statistics
  }
  else{
    out_moodys <- compare_means(
      data = df,
      return_statistic = TRUE,
      credit_agency = "moodys",
      category = rating
    )$statistics
    # update
    moodys_statistics <- rbind(moodys_statistics, out_moodys)
    
  }
}

# make summary figure
p_sp <- ggplot() +
  geom_errorbarh(
    data = sp_statistics,
    mapping = aes(y = rating_category,
                  xmin = conf_lower_bound,
                  xmax = conf_upper_bound),
    height = 0.5,
    size = 1,
    color = PALETTE[1]
  ) + geom_point(
    data = sp_statistics,
    mapping = aes(y = rating_category, x = difference_mean),
    size = 2,
    shape = 21,
    fill = PALETTE[1]
  ) + geom_vline(xintercept = 0, linetype = 2, color = "gray50") +
  labs(x = "95% Confidence Interval Difference in mean probability of default",
           y = "Credit Rating Category") + ggtitle("Standard & Poor's (S&P)") 


p_moodys <- ggplot() +
  geom_errorbarh(
    data = moodys_statistics,
    mapping = aes(y = rating_category,
                  xmin = conf_lower_bound,
                  xmax = conf_upper_bound),
    height = 0.5,
    size = 1,
    color = PALETTE[2]
  ) + geom_point(
    data = moodys_statistics,
    mapping = aes(y = rating_category, x = difference_mean),
    size = 2,
    shape = 21,
    fill = PALETTE[2]
  ) + geom_vline(xintercept = 0, linetype = 2, color = "gray50") +
  labs(x = "95% Confidence Interval Difference in mean probability of default",
           y = "") + ggtitle("Moodys") 

# plot figure
p_sp + p_moodys

ggsave("output/modelling/figure1.png",  
       units = "cm",
       height = 14,
       width = 30)
