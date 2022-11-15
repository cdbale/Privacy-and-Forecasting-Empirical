## Script to analyze how data protection changes exponential smoothing model
# coefficients.

# Author: Cameron Bale

library(tidyverse)
library(gridExtra)
library(ggpubr)

##### Examine changes in SES alpha parameter.

ses_params <- read_csv("../../Outputs/Results/Model Parameters/ses_params.csv")

ses_params <- ses_params %>%
  gather(key="name", value="values") %>%
  mutate(name = gsub("k-nts", "knts", name)) %>%
  separate(name, c("Protection", "Privacy_Parameter", "Model_Parameter"), sep="_")


# Visualize changes in alpha value.

dp_alpha <- ses_params %>%
  filter(Protection %in% c("DP")) %>%
  mutate(Privacy_Parameter = factor(Privacy_Parameter, levels=c("20", "10", "4.6", "1", "0.1"), labels=c("\u03f5 = 20", "\u03f5 = 10", "\u03f5 = 4.6", "\u03f5 = 1", "\u03f5 = 0.1"))) %>%
  ggplot(aes(x=Privacy_Parameter, y=values)) +
  geom_boxplot() +
  ylab("SES Alpha") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

top_alpha <- ses_params %>%
  filter(Protection %in% c("Top")) %>%
  mutate(Privacy_Parameter = factor(Privacy_Parameter, levels=c("0.1", "0.2", "0.4"), labels = c("Top 0.1", "Top 0.2", "Top 0.4")))  %>%
  ggplot(aes(x=Privacy_Parameter, y=values)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

knts_alpha <- ses_params %>%
  filter(Protection %in% c("knts")) %>%
  mutate(Privacy_Parameter = factor(Privacy_Parameter, levels=c("5", "10", "15"), labels = c("k = 5", "k = 10", "k = 15")))  %>%
  ggplot(aes(x=Privacy_Parameter, y=values)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

plt <- ggarrange(dp_alpha, top_alpha, knts_alpha,
                 # labels=c("k = 5", "k = 10", "k = 15"),
                 ncol=3)

annotate_figure(plt, top=text_grob("SES Alpha Parameter Comparison", face="bold", size=14))

#################################################################

## Examining changes in DES parameters

des_params <- read_csv("../../Outputs/Results/Model Parameters/des_params.csv")

des_params <- des_params %>%
  gather(key="name", value="values") %>%
  mutate(name = gsub("k-nts", "knts", name)) %>%
  separate(name, c("Protection", "Privacy_Parameter", "Model_Parameter"), sep="_")

des_params <- des_params %>%
  group_by(Protection, Privacy_Parameter, Model_Parameter) %>%
  mutate(keep_val = (values <= quantile(values, 0.95)) & (values >= quantile(values, 0.05))) %>%
  filter(keep_val == TRUE)

des_params %>%
  filter(Protection %in% c("DP")) %>%
  mutate(Privacy_Parameter = factor(Privacy_Parameter, levels=c("20", "10", "4.6", "1", "0.1"), labels=c("\u03f5 = 20", "\u03f5 = 10", "\u03f5 = 4.6", "\u03f5 = 1", "\u03f5 = 0.1"))) %>%
  ggplot(aes(x=Privacy_Parameter, y=values)) +
  geom_boxplot() +
  facet_wrap(~Model_Parameter, scales='free') %>%
  ylab("SES Alpha") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

################################################

## Examine changes in TES parameters
tes_params <- read_csv("../../Outputs/Results/Model Parameters/tes_params.csv")
  
tes_params <- tes_params %>%
  gather(key="name", value="values") %>%
  mutate(name = gsub("k-nts", "knts", name)) %>%
  separate(name, c("Protection", "Privacy_Parameter", "Model_Parameter"), sep="_")

tes_params <- tes_params %>%
  group_by(Protection, Privacy_Parameter, Model_Parameter) %>%
  mutate(keep_val = (values <= quantile(values, 0.95)) & (values >= quantile(values, 0.05))) %>%
  filter(keep_val == TRUE)
  
tes_params %>%
  filter(Protection %in% c("DP")) %>%
  mutate(Privacy_Parameter = factor(Privacy_Parameter, levels=c("20", "10", "4.6", "1", "0.1"), labels=c("\u03f5 = 20", "\u03f5 = 10", "\u03f5 = 4.6", "\u03f5 = 1", "\u03f5 = 0.1"))) %>%
  ggplot(aes(x=Privacy_Parameter, y=values)) +
  geom_boxplot() +
  facet_wrap(~Model_Parameter, scales='free') %>%
  ylab("SES Alpha") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))





tes_params %>%
  filter(Protection %in% c("Top", "original")) %>%
  mutate(Privacy_Parameter = factor(Privacy_Parameter, levels=c("none", "0.1", "0.2", "0.4"), labels = c("Original", "Top 0.1", "Top 0.2", "Top 0.4"))) %>%
  ggplot(aes(x=Privacy_Parameter, y=values)) +
  geom_boxplot() +
  facet_wrap(~Model_Parameter, scales='free') %>%
  ylab("SES Alpha") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


