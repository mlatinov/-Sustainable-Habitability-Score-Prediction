
#### Libraries ####
library(tidyverse)
library(psych)
library(corrplot)

# Load the data
clean_train <- read_rds("clean_train")

#### Overview: Visualizing All Features ####

## Plot numerical distributions

# VB Plot
clean_train %>%
  select(-id) %>%
  select_if(is.numeric)%>%
  na.omit() %>%
  pivot_longer(cols = everything(),names_to = "feature",values_to = "value") %>%
  ggplot(aes(x = feature,y = value)) +
  geom_violin(fill = "grey",alpha = 0.5)+
  geom_boxplot()+
  facet_wrap(~feature,scales = "free")+
  coord_flip()+
  theme_minimal()+
  labs(
    title = "VB Plot of Numerical Features",
    x = "",
    y = "Value")+
  theme(
    title = element_text(size = 16,face = "bold"),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 10,face = "italic"),
    axis.text.y = element_blank())

## Corr Plot
clean_train %>%
  select_if(is.numeric) %>%
  select(-id) %>%
  na.omit() %>%
  cor() %>%
  corrplot(
    method = "color",
    order = "hclust",
    addrect = 4,
    tl.col = "grey30",
    col = colorRampPalette(c("white", "gray85", "black"))(200),
    addCoef.col = "red4",
    tl.srt = 40
  )
# Add title with adjustments
title("Correlation Matrix of Features", cex.main = 2.5, line = 2.5)

# Bar Plot for Categorical Features
clean_train %>%
  select_if(is.factor) %>%
  na.omit() %>%
  pivot_longer(cols = everything(),names_to = "feature",values_to = "value")%>%
  ggplot(aes(x = fct_rev(fct_infreq(value)),fill = feature))+
  geom_bar()+
  scale_fill_brewer(palette="Spectral")+
  facet_wrap(~feature,scales = "free")+
  coord_flip()+
  theme_minimal()+
  labs(
    title = "Distribution of Categorical Features",
    x = "",
    y ="Count",
    fill = "Feature")+
  theme(
    title = element_text(size = 15,face = "bold"),
    strip.text = element_text(size = 13,face = "italic"),
    axis.title = element_text(size = 13)
  )

#### Hypothesis Testing ####















