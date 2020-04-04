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


# 1) Visualize differences in mean by means of a boxplot and add t.test p-value

# to give an example (by default I assume variance are not equal which is mostly the case)
df %>% select(pd_log, region, moodys) %>% na.omit() %>%
  filter(moodys == "Aa1") %>% t.test(pd_log~ region, data=., var.equal=FALSE)

# the code below does this for all ratings (both S&P and Moodys)
# 1)
compare_means(data = df,credit_agency = "sp",category = "AAA")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Aaa", title = "Moodys")$figure + 
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure1.png",  units = "cm",height = 10,width = 20)
# 2)
compare_means(data = df,credit_agency = "sp",category = "AA+", add="jitter")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Aa1", title = "Moodys", add="jitter")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure2.png",  units = "cm",height = 10,width = 20)
# 3)
compare_means(data = df,credit_agency = "sp",category = "AA")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Aa2", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure3.png",  units = "cm",height = 10,width = 20)
# 4)
compare_means(data = df,credit_agency = "sp",category = "AA-")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Aa3", title = "Moodys")$figure  +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure4.png",  units = "cm",height = 10,width = 20)
# 5)
compare_means(data = df,credit_agency = "sp",category = "A+")$figure +
compare_means(data = df,credit_agency = "moodys",category = "A1", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure5.png",  units = "cm",height = 10,width = 20)
# 6)
compare_means(data = df,credit_agency = "sp",category = "A")$figure +
compare_means(data = df,credit_agency = "moodys",category = "A2", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure6.png",  units = "cm",height = 10,width = 20)
# 7)
compare_means(data = df,credit_agency = "sp",category = "A-")$figure +
compare_means(data = df,credit_agency = "moodys",category = "A3", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure7.png",  units = "cm",height = 10,width = 20)
# 8) 
compare_means(data = df,credit_agency = "sp",category = "BBB+")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Baa1", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure8.png",  units = "cm",height = 10,width = 20)
# 9)
compare_means(data = df,credit_agency = "sp",category = "BBB")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Baa2", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure9.png",  units = "cm",height = 10,width = 20)
# 10)
compare_means(data = df,credit_agency = "sp",category = "BBB-")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Baa3", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure10.png",  units = "cm",height = 10,width = 20)
# 11)
compare_means(data = df,credit_agency = "sp",category = "BB+")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Ba1", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure11.png",  units = "cm",height = 10,width = 20)
# 12)
compare_means(data = df,credit_agency = "sp",category = "BB")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Ba2", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure12.png",  units = "cm",height = 10,width = 20)
# 13)
compare_means(data = df,credit_agency = "sp",category = "BB-")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Ba3", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure13.png",  units = "cm",height = 10,width = 20)
# 14)
compare_means(data = df,credit_agency = "sp",category = "B+")$figure +
compare_means(data = df,credit_agency = "moodys",category = "B1", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure14.png",  units = "cm",height = 10,width = 20)
# 15)
compare_means(data = df,credit_agency = "sp",category = "B", add="jitter")$figure +
compare_means(data = df,credit_agency = "moodys",category = "B2", title = "Moodys", add="jitter")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure15.png",  units = "cm",height = 10,width = 20)
# 16)
compare_means(data = df,credit_agency = "sp",category = "B-", add="jitter")$figure +
compare_means(data = df,credit_agency = "moodys",category = "B3", title = "Moodys", add="jitter")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure16.png",  units = "cm",height = 10,width = 20)
# 17)
compare_means(data = df,credit_agency = "sp",category = "CCC+...")$figure +
compare_means(data = df,credit_agency = "moodys",category = "Caa3...", title = "Moodys")$figure +
  plot_layout(guides="collect")
ggsave("output/modelling/part2/figure17.png",  units = "cm",height = 10,width = 20)


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

moodys_statistics$order <- 1:17
sp_statistics$order <- 1:17

reorder(sp_statistics$order, sp_statistics$difference_mean)

moodys_statistics
sp_statistics

# make summary figure
p_sp <- ggplot() +
  geom_errorbarh(
    data = sp_statistics,
    mapping = aes(y = reorder(rating_category,order),
                  xmin = conf_lower_bound,
                  xmax = conf_upper_bound),
    height = 0.5,
    size = 1,
    color = PALETTE[1]
  ) + geom_point(
    data = sp_statistics,
    mapping = aes(y = reorder(rating_category,order), x = difference_mean),
    size = 2,
    shape = 21,
    fill = PALETTE[1]
  ) + geom_vline(xintercept = 0, linetype = 2, color = "gray50") +
  labs(x = "Difference in Probability of Default (Logarithmic)",
           y = "Credit Rating Category") + ggtitle("Standard & Poor's (S&P), 95% Confidence Inteval") 


p_moodys <- ggplot() +
  geom_errorbarh(
    data = moodys_statistics,
    mapping = aes(y = reorder(rating_category,order),
                  xmin = conf_lower_bound,
                  xmax = conf_upper_bound),
    height = 0.5,
    size = 1,
    color = PALETTE[2]
  ) + geom_point(
    data = moodys_statistics,
    mapping = aes(y = reorder(rating_category,order), x = difference_mean),
    size = 2,
    shape = 21,
    fill = PALETTE[2]
  ) + geom_vline(xintercept = 0, linetype = 2, color = "gray50") +
  labs(x = "Difference in Probability of Default (Logarithmic)",
           y = "") + ggtitle("Moodys, 95% Confidence Inteval") 

# plot figure
p_sp + p_moodys

ggsave("output/modelling/part2/figure18.png",  
       units = "cm",
       height = 14,
       width = 30)

