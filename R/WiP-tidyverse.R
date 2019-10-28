## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE) #, knitr.table.format = 'latex')
library(data.table)
library(ggplot2)
library(knitr)
library(kableExtra)
options(width=45)
knitr::opts_chunk$set(fig.pos = 'H')

# Reset the data.table print options.
options(datatable.print.topn=4, digits=3)

# Finding files using here package
library(here)
here()

# Making some aesthetic changes for this document
theme_set(theme_gray(base_size = 9))
update_geom_defaults("point", list(size = 0.5))
update_geom_defaults("boxplot", list(outlier.size = 0.5))


## head -n 4 ../data/WB-WiP.csv | cat -n |  sed 's/^[[:blank:]]*/ /g'


## ----loadPackages, message=FALSE, warning=FALSE--------------------------
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gghighlight)


## ----readData, collapse=TRUE, message=FALSE------------------------------
wip <- read_csv(here("data", "WB-WiP.csv"), 
             skip = 4)


## ----colNames, collapse=TRUE---------------------------------------------
head(names(wip))
tail(names(wip))


## ----fixColNames, collapse=TRUE------------------------------------------
names(wip) <- make.names(names(wip))
head(names(wip))
tail(names(wip))


## head -n 5 ../data/WB-WiP.csv | tail -c 31


## ----checkX64, collapse=TRUE, message=FALSE, warning=FALSE---------------
wip %>% pull(X64) %>% is.na(.) %>% all(.)


## ----rmCols, collapse=TRUE-----------------------------------------------
wip2 <- wip %>% 
  select(-Indicator.Name, -Indicator.Code, 
         -X64) %>% 
  rename(Country=Country.Name, Code=Country.Code)
head(names(wip2))
tail(names(wip2))


## ----gatherWip2, collapse=TRUE, message=FALSE, warning=FALSE-------------
WP <- wip2 %>% 
  gather(key=YearC, value=pctWiP,starts_with("X"),
         na.rm=TRUE) %>% 
  mutate(Year = parse_number(YearC),
         # pctWiP = as.numeric(pctWiP),
         Ratio = (100-pctWiP)/pctWiP) %>% 
  select(Country, Code, Year, pctWiP, Ratio) %>% 
  arrange(Country, Year)
# Look at the contents of WP
glimpse(WP)


## ----PTTable-------------------------------------------------------------
# Reset tibble print option to see more rows
options(tibble.print_max = 25)
WP %>% filter(Country=="Portugal")


## ----PTplot, fig.width=3, fig.height=2.3---------------------------------
WP %>% 
  filter(Country=="Portugal") %>% 
  ggplot(aes(Year, pctWiP)) +
  geom_line() + geom_point() +
  scale_y_continuous(limits=c(0, 50)) +
  ylab("% Women in Parliament")


## ----euPctPlot, fig.width=3.5, fig.height=3, cache=FALSE-----------------
WP %>%
  filter(Country %in% c("Portugal", "Sweden", 
      "Spain", "Hungary", "Romania", "Finland", 
      "Germany", "European Union")) %>%
  ggplot(aes(Year, pctWiP, colour=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  scale_y_continuous(limits=c(0, 50), 
                     breaks=seq(0, 50, by=10)) +
  ggtitle("Women in Parliament: EU Countries") +
  ylab("% Women in Parliament")


## ----allTopPct-----------------------------------------------------------
WP %>% 
  arrange(-pctWiP) %>% 
  head(10)


## ----allTopPctYear, collapse=TRUE----------------------------------------
WP %>% 
  group_by(Year) %>% 
  arrange(Year, -pctWiP) %>% 
  filter(row_number()==1)


## ----mergeContinent------------------------------------------------------
# Ensure that 'countrycode' package is installed.
# install.packages("countrycode")
library(countrycode)
cl <- codelist %>% 
  select(continent, wb) %>% 
  rename(Code = wb, Continent = continent)
cWP <- WP %>% 
  left_join(cl, by = "Code")


## ----allTopPctYearContinent, collapse=TRUE-------------------------------
cWP %>% 
  filter(Year %in% c(1990, 2018) & 
           !is.na(Continent)) %>% 
  group_by(Continent, Year) %>% 
  arrange(Continent, Year, -pctWiP) %>% 
  filter(row_number()==1) %>% 
  select(Continent, Country, Year, pctWiP, Ratio)


## ----declinePct, collapse=TRUE-------------------------------------------
dWP <- cWP %>% 
  group_by(Country) %>% 
  arrange(Country, Year) %>%
  filter(row_number()==1 | row_number()==n()) %>% 
  mutate(pctDiff = pctWiP - 
           lag(pctWiP, order_by=Country)) %>% 
  filter(pctDiff<0  & !is.na(Continent)) %>% 
  arrange(pctDiff)
dWP %>% select(Country, pctDiff)


## ----decline5pct, fig.width=3.5, fig.height=2.4--------------------------
# Select the countries to plot
dclpct <- dWP %>% 
  filter(!is.na(Continent) & pctDiff <= -5) %>% 
  pull(Country)

WP %>%
  filter(Country %in% dclpct) %>% 
  ggplot(aes(Year, pctWiP, colour=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  scale_y_continuous(limits=c(0, 40),
  breaks=seq(0, 40, by=10)) +
  ggtitle("Women in Parliament: Decline >=5%") +
  ylab("% Women in Parliament")


## ----globalRank , collapse=TRUE------------------------------------------
cWPrankG <- cWP %>% 
  filter(!is.na(Continent)) %>% 
  group_by(Year) %>% 
  mutate(RankG = rank(-pctWiP), 
         TotalG = n()) 


## ----globalRankPT , collapse=TRUE----------------------------------------
cWPrankG %>% 
  filter(Country=="Portugal") %>% 
  select(Country, Year, pctWiP, Ratio, RankG, 
         TotalG) %>% 
  arrange(Year)


## ----continentRank , collapse=TRUE---------------------------------------
cWPx <- cWPrankG %>% 
  filter(!is.na(Continent)) %>% 
  group_by(Continent, Year) %>% 
  mutate(RankC = rank(-pctWiP), 
         TotalC = n()) 


## ----continentRankPT , collapse=TRUE-------------------------------------
cWPx %>% 
  ungroup() %>% 
  filter(Country=="Portugal") %>% 
  select(Country, Year, pctWiP, Ratio, RankC, 
         TotalC) %>% 
  arrange(Year)


## ----euRankplot, fig.width=3.5, fig.height=2.7---------------------------
cWPx %>% 
  filter(Country %in% c("Portugal", "Sweden", 
    "Spain", "Hungary", "Romania", "Finland", 
    "Germany")) %>%
  ggplot(aes(Year, RankC, colour=Country)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  scale_y_continuous(limits=c(0, 45), 
                     breaks=seq(0, 45, by=10)) +
  ggtitle("Women in Parliament: Ranked") +
  ylab("Rank in Europe")


## ----allTopRankYearContinent, collapse=TRUE, echo=2----------------------
options(tibble.print.rownames=FALSE)
cWPx %>% 
  filter(Year %in% c(1990, 2018) & RankC==1) %>% 
  arrange(Continent, Year) %>% 
  select(Continent, Year, Country, pctWiP, Ratio) 
options(tibble.print.rownames=TRUE)


## ----globalTrends, message=FALSE, fig.width=3, fig.height=2.5------------
cWP %>% 
  filter(is.na(Continent)) %>%
  ggplot(aes(Year, pctWiP, group=Country)) +
  geom_line() +
  gghighlight(Country=="World", 
              use_direct_label = FALSE, use_group_by = FALSE) +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  scale_y_continuous(limits=c(0, 40), 
                     breaks=seq(0, 40, by=10)) +
  ggtitle("Women in Parliament: Global Trends") +
  ylab("% Women in Parliament")


## ----addWiPrect, echo=FALSE, out.width="100%"----------------------------
include_graphics(here("images", "Women_in_Parliament_rect.png"))

