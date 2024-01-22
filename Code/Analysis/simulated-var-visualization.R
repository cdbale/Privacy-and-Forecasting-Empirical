# compare the yearly micro series to the VAR simulated version

library(tidyverse)
library(ggplot2)

up <- read_csv("../../Data/Cleaned/M3/quarterly-FINANCE_h1_train.csv") %>%
  mutate(series = 1:n()) %>%
  gather(key="time", value="value", -series) %>%
  mutate(time = as.numeric(time),
         type = 'unprotected') 

vs <- read_csv("../../Outputs/VAR Simulated/M3/quarterly-FINANCE_h1_train.csv") %>%
  mutate(series = 1:n()) %>%
  gather(key="time", value="value", -series) %>%
  mutate(time = as.numeric(time),
         type = 'var-simulated')

up %>%
  filter(series==1) %>%
  ggplot(aes(x=time, y=value)) +
  geom_line()

vs %>%
  filter(series==1) %>%
  ggplot(aes(x=time, y=value)) +
  geom_line()

up %>%
  bind_rows(vs) %>%
  ggplot(aes(x = time, y = value, color=type)) +
  geom_line() +
  facet_wrap(~series)

################################################################################

up <- read_csv("../../Data/Cleaned/M3/yearly-MACRO_h1_train.csv") %>%
  mutate(series = 1:n()) %>%
  gather(key="time", value="value", -series) %>%
  mutate(time = as.numeric(time),
         type = 'unprotected') 

vs <- read_csv("../../Outputs/VAR Simulated/M3/yearly-MACRO_h1_train.csv") %>%
  mutate(series = 1:n()) %>%
  gather(key="time", value="value", -series) %>%
  mutate(time = as.numeric(time),
         type = 'var-simulated')

up %>%
  filter(series==1) %>%
  ggplot(aes(x=time, y=value)) +
  geom_line()

vs %>%
  filter(series==1) %>%
  ggplot(aes(x=time, y=value)) +
  geom_line()

up %>%
  bind_rows(vs) %>%
  ggplot(aes(x = time, y = value, color=type)) +
  geom_line() +
  facet_wrap(~series)








