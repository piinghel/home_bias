# clear environment
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
PALETTE <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
STYLE <- "steelblue"

#---------------------------

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
  sp = ifelse(sp > 17, 17, sp),
  moodys = ifelse(moodys > 17, 17, moodys)
)


#----------------------------------------
# 1) PROBABITLITY OF DEFAULT (PD)
#----------------------------------------

# overall distribution of PD
ggplot(df, aes(x = pd_log)) +
  geom_density(alpha = .7, fill = PALETTE[1]) +
  labs(x = "Probability of default (PD) Logarithmic", y = "Density")
ggsave("output/figure1.png", device = "png")

# condition  by region
ggplot(df, aes(x = pd_log, fill = region)) +
  geom_density(alpha = .7) +
  labs(x = "Probability of Default (PD) Logarithmic", y = "Density") +
  scale_fill_brewer(palette = "Dark2")
ggsave("output/figure2.png", device = "png")


# condition by sector
ggplot(df, aes(x = pd_log, fill = sector)) +
  geom_density(alpha = .7) + 
  labs(x = "Probability of Default (PD) Logarithmic", y ="Density") +
  scale_fill_brewer(palette = "Dark2")
ggsave("output/figure3.png", device = "png")

# condition by sector and region
ggplot(df, aes(x = pd_log, fill = region)) +
  geom_density(alpha = .7) + 
  labs(x = "Probability of Default (PD) Logarithmic", y ="Density") +
  scale_fill_brewer(palette = "Dark2") +
  facet_grid(sector ~ ., scales = "free_y")
ggsave("output/figure4.png", device = "png")


# visualize over time by sector
df %>% dplyr::group_by(date, sector, region) %>%
  dplyr::summarise(central_measure_pd = median(pd_log, na.rm = TRUE)) %>%
  # plot
  ggplot(., aes(x = date, y = central_measure_pd, color = sector)) +
  geom_line() + geom_point() +
  scale_color_brewer(palette = "Dark2") +
  geom_vline(xintercept=as.POSIXct("2009-03-01"), size=1, color="red", linetype="dashed") +
  annotate("text", x = as.POSIXct("2014-07-01"), y = -4, 
           label = "March 6, 2009: The Dow Jones hit its lowest level") +
  #scale_y_continuous(trans = "log10") +
  facet_grid(region ~ ., scales = "free_y") +
  labs(x = "Date", y = "Median Probability of Default (MPD) Logarithmic ", fill = "Sector") 

ggsave("output/figure5.png", device = "png", 
       units = "cm", height = 18, width = 24)


#----------------------------------------
# 2) Standards and Poor (sp) and Moodys
#----------------------------------------

#-----------------------------------------------------------
# 2.1) Statitic visualisation (don't take time into account)
#-----------------------------------------------------------

# include missing values (NA)
ggplot(df, aes(as.factor(sp))) +
  geom_bar(fill = "steelblue") + labs(x = "Rating category", y = "Count") +
  ggtitle("Standard & Poor's (S&P) (missing values (NA) included)")
ggsave("output/figure6.png", device = "png")

ggplot(df, aes(as.factor(moodys))) +
  geom_bar(fill = "steelblue") + labs(x = "Rating category", y = "Count") +
  ggtitle("Moodys (missing values (NA) included)")
ggsave("output/figure7.png", device = "png")

# ignore missing values (NA)
df %>% group_by(sp) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = sp, y = n)) + geom_bar(stat = "identity", fill = PALETTE[2]) +
  labs(x = "Rating category", y = "Count") + ggtitle("Standard & Poor's (S&P)") +

df %>% group_by(moodys) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = moodys, y = n)) + geom_bar(stat = "identity", fill = PALETTE[1]) +
  labs(x = "Rating category", y = "") + ggtitle("Moodys")
ggsave("output/figure8.png", device = "png")

# condition on region
df %>% group_by(sp, region) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = sp, y = n, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Rating category", y = "Count") + ggtitle("Standard & Poor's (S&P)") +

df %>% group_by(moodys, region) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = moodys, y = n, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Rating category", y = "") + ggtitle("Moodys") +
  plot_layout(guides = 'collect')
ggsave("output/figure9.png", device = "png",
       units = "cm", height = 14, width = 20)

# condition on sector
df %>% group_by(sp, sector) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = sp, y = n, fill = sector)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Rating category", y = "Count") +
  ggtitle("Standard & Poor's (S&P)") +
  
df %>% group_by(moodys, sector) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = moodys, y = n, fill = sector)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Rating category", y = "") +
  ggtitle("Moodys") + 
  plot_layout(guides = 'collect')
ggsave("output/figure10.png", device = "png",
       units = "cm", height = 14, width = 20)

# condition both on region and sector
# 1) Standard & Poor's (S&P)
df %>% group_by(sp, sector, region) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = sp, y = n)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           fill = "steelblue") +
  labs(x = "Rating category", y = "Count") +
  facet_grid(sector ~ region, scales = "free_y") +
  ggtitle("Standard & Poor's (S&P)")
ggsave("output/figure11.png", device = "png")

# 2) Moodys
df %>% group_by(moodys, sector, region) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = moodys, y = n)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           fill = "steelblue") +
  labs(x = "Rating category", y = "Count") +
  facet_grid(sector ~ region, scales = "free_y") + 
  ggtitle("Moodys")
ggsave("output/figure12.png", device = "png")


#-----------------------------------------------------------
# 2.1) Dynamic visualisation (take time into account)
#-----------------------------------------------------------

# check the number of observations over time
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

# group together in one plot
(p1 + p2) / (p3 + p4) + plot_layout(guides = "collect")
ggsave("output/figure13.png", device = "png")


# Look at the number of categories of rating over time: Standard & Poor's (S&P)
p5 <- df %>% select(id, sp, year) %>% distinct(id, sp, year) %>%
  group_by(sp, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = sp)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  labs(x = "Year", y = "Rating category", fill = "Observations") + ggtitle("Standard & Poor's (S&P)")

# Look at the number of categories of rating over time: Moodys
p6 <-
  df %>% select(id, moodys, year) %>% distinct(id, moodys, year) %>%
  group_by(moodys, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = moodys)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  labs(x = "Year", y = "", fill = "Observations") + ggtitle("Moodys")

# group together in one figure
p5 + p6
ggsave("output/figure14.png", device = "png",
       units = "cm", height = 12, width = 24)

# SP: Europe and US
df %>% select(id, sp, year, region) %>% distinct(id, sp, year, region) %>%
  group_by(sp, region, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = sp)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(. ~ region) +
  labs(x = "Year", y = "", fill = "Observations") + ggtitle("Standard & Poor's (S&P)")
ggsave("output/figure15.png", device = "png",
       units = "cm", height = 12, width = 24)


# Moodys: Europe and US
df %>% select(id, moodys, year, region) %>% distinct(id, moodys, year, region) %>%
  group_by(moodys, region, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = moodys)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(. ~ region) +
  labs(x = "Year", y = "Rating category", fill = "Observations") + ggtitle("Moodys")
ggsave("output/figure16.png", device = "png",
       units = "cm", height = 12, width = 24)




# SP: Sector
df %>% select(id, sp, year, sector) %>% distinct(id, sp, year, sector) %>%
  group_by(sp, sector, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = sp)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(. ~ sector) +
  labs(x = "Year", y = "Rating category", fill = "Observations") + ggtitle("Standard & Poor's (S&P)")
ggsave("output/figure17.png", device = "png",
       units = "cm", height = 12, width = 24)



# Moodys: Europe and US
df %>% select(id, moodys, year, sector) %>% distinct(id, moodys, year, sector) %>%
  group_by(moodys, sector, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = moodys)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(. ~ sector) +
  labs(x = "Year", y = "Rating category", fill = "Observations") + ggtitle("Moodys")
ggsave("output/figure18.png", device = "png",
       units = "cm", height = 12, width = 24)

# SP: Sector and Region
df %>% select(id, sp, year, sector, region) %>% distinct(id, sp, year, sector, region) %>%
  group_by(sp, sector, region, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = sp)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(sector ~ region) +
  labs(x = "Year", y = "Rating category", fill = "Observations") + 
  ggtitle("Standard & Poor's (S&P)") 
ggsave("output/figure19.png", device = "png",
       units = "cm", height = 12, width = 24)


# Moodys: Sector and Region
df %>% select(id, moodys, year, sector, region) %>% distinct(id, moodys, year, sector, region) %>%
  group_by(moodys, sector, region, year) %>% tally() %>% na.omit() %>%
  ggplot(., aes(x = year, y = moodys)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis(option = "magma") + theme_tufte(base_family = "Helvetica") +
  facet_grid(sector ~ region) +
  labs(x = "Year", y = "Rating category", fill = "Observations") + ggtitle("Moodys")

ggsave("output/figure20.png", device = "png",
       units = "cm", height = 12, width = 24)




