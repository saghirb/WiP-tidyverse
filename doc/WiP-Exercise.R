# Exercises for "Women in Parliament - data.table"
# Source: https://github.com/saghirb/WiP-rdatatable

# Author: _Your Name Here_
#   Date: DD MON YYYY

# Load the pacakges needed
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gghighlight)

# Importing the Women in Parliament (WiP) data
wip <- read_csv(here("data", "WB-WiP.csv"), skip = 4)
# Look at the data
wip
glimpse(wip)

# Fix column (variable names)
head(names(wip))
tail(names(wip))
names(wip) <- make.names(names(wip))
head(names(wip))
tail(names(wip))

# Continue from here....

