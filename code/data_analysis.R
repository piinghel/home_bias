

# clean environment
rm(list = ls())
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
cbpalette <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")




# TODO total number of firms per region is constant (ask professor)
# also I think that using counts is more informative than % since it gives
# you more information

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
  pd_log = log(pd),
  year = lubridate::year(date),
  month = lubridate::month(date),
  sp = ifelse(sp>17,17,sp),
  moodys = ifelse(moodys>17,17,moodys)
)


#----------------------------------------
# 1) PROBABITLITY OF DEFAULT (PD)
#----------------------------------------
# overall distribution of PD
ggplot(df, aes(x = pd_log)) +
  geom_density(alpha = .7, fill = cbpalette[1]) +
  labs(x = "Probability of default (PD) logarithmic ")


# condition  by region
ggplot(df, aes(x = pd_log, fill = region)) +
  geom_density(alpha = .7) + labs(x = "Probability of Default (PD) Logarithmic ") +
  scale_fill_brewer(palette = "Dark2")

# condition by sector
ggplot(df, aes(x = pd_log, fill = sector)) +
  geom_density(alpha = .7) + labs(x = "Probability of Default (PD) Logarithmic ") +
  scale_fill_brewer(palette = "Dark2")

# condition by sector and region
ggplot(df, aes(x = pd_log, fill = region)) +
  geom_density(alpha = .7) + labs(x = "Probability of Default (PD) Logarithmic") +
  scale_fill_brewer(palette = "Dark2") + 
  facet_grid(sector ~ ., scales = "free_y")


# visualize over time by sector
df %>% dplyr::group_by(date, sector, region) %>%
  dplyr::summarise(central_measure_pd = median(pd, na.rm = TRUE)) %>%
  # plot
  ggplot(., aes(x = date, y = central_measure_pd, color = sector)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(trans = "log10") +
  facet_grid(region ~ ., scales = "free_y") +
  labs(x = "Date", y = "Median Probability of Default (MPD) Logarithmic ", fill = "Sector")


#----------------------------------------
# 2) Standards and Poor (sp) and Moodys
#----------------------------------------

p1 <-
  df %>% select(id, sp, region, year) %>% distinct(id, year, region) %>%
  dplyr::group_by(year, region) %>%
  tally() %>% ggplot(., aes(x = year, y = n, color = region)) +
  geom_point() + geom_line() + labs(x = "", y = "Unique firms") +
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("S&P")


p2 <-
  df %>% select(id, moodys, region, year) %>% distinct(id, year, region) %>%
  dplyr::group_by(year, region) %>%
  tally() %>% ggplot(., aes(x = year, y = n, color = region)) +
  geom_point() + geom_line() + labs(x = "", y = "") +
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Moody's")



# number of observations per year
p3 <-
  df %>% select(id, sp, region, year) %>% dplyr::group_by(year, region) %>%
  tally() %>% ggplot(., aes(x = year, y = n, color = region)) +
  geom_point() + geom_line() + labs(x = "Year", y = "Observations") +
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("S&P")



# number of observations per year
p4 <-
  df %>% select(id, moodys, region, year) %>% dplyr::group_by(year, region) %>%
  tally() %>% ggplot(., aes(x = year, y = n, color = region)) +
  geom_point() + geom_line() + labs(x = "Year", y = "") +
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Moody's")


(p1 + p2) / (p3 + p4) + plot_layout(guides = "collect")


# sp
p5 <- df %>% select(id, sp, year) %>% distinct(id, sp, year) %>%
  group_by(sp, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = sp)) +
  geom_tile(aes(fill = n)) +
  scale_x_continuous(breaks = c(2000:2021),
                     expand = c(0, 0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  labs(x="Year", y = "Rating category", fill="Observations") + ggtitle("Standard & Poor's (S&P)")

# Moodys
p6 <- df %>% select(id, moodys, year) %>% distinct(id, moodys, year) %>%
  group_by(moodys, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = moodys)) +
  geom_tile(aes(fill = n)) +
  scale_x_continuous(breaks = c(2000:2021),
                     expand = c(0, 0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  labs(x="Year", y = "", fill="Observations") + ggtitle("Moodys")


p5 + p6


# SP: Europe and US
df %>% select(id, sp, year, region) %>% distinct(id, sp, year, region) %>%
  group_by(sp,region, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = sp)) +
  geom_tile(aes(fill = n)) +
  scale_x_continuous(breaks = c(2000:2021),
                     expand = c(0, 0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(.~region) +
  labs(x="Year", y = "", fill="Observations") + ggtitle("Standard & Poor's (S&P)")


# Moodys: Europe and US
df %>% select(id, moodys, year, region) %>% distinct(id, moodys, year, region) %>%
  group_by(moodys,region, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = moodys)) +
  geom_tile(aes(fill = n)) +
  scale_x_continuous(breaks = c(2000:2021),
                     expand = c(0, 0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(.~region) +
  labs(x="Year", y = "", fill="Observations") + ggtitle("Moodys")



# SP: Sector
df %>% select(id, sp, year, sector) %>% distinct(id, sp, year, sector) %>%
  group_by(sp,sector, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = sp)) +
  geom_tile(aes(fill = n)) +
  scale_x_continuous(breaks = c(2000:2021),
                     expand = c(0, 0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(.~sector) +
  labs(x="Year", y = "", fill="Observations") + ggtitle("Standard & Poor's (S&P)")


# Moodys: Europe and US
df %>% select(id, moodys, year, sector) %>% distinct(id, moodys, year, sector) %>%
  group_by(moodys,sector, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = moodys)) +
  geom_tile(aes(fill = n)) +
  scale_x_continuous(breaks = c(2000:2021),
                     expand = c(0, 0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(.~sector) +
  labs(x="Year", y = "", fill="Observations") + ggtitle("Moodys")





# SP: Sector
df %>% select(id, sp, year, sector, region) %>% distinct(id, sp, year, sector, region) %>%
  group_by(sp,sector, region, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = sp)) +
  geom_tile(aes(fill = n)) +
  scale_x_continuous(breaks = c(2000:2021),
                     expand = c(0, 0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(sector~region) +
  labs(x="Year", y = "", fill="Observations") + ggtitle("Standard & Poor's (S&P)")


# Moodys: Europe and US
df %>% select(id, moodys, year, sector, region) %>% distinct(id, moodys, year, sector, region) %>%
  group_by(moodys,sector, region, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = moodys)) +
  geom_tile(aes(fill = n)) +
  scale_x_continuous(breaks = c(2000:2021),
                     expand = c(0, 0)) + coord_equal(ratio = 1) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(sector~region) +
  labs(x="Year", y = "", fill="Observations") + ggtitle("Moodys")







