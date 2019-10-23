# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(strex)

library(devtools)
library(roxygen2)

DRG_data <- read.csv("C:/Users/aaphs/OneDrive/Desktop/Data.Sci.1/package_ap/dgr.boxplot/DRG_data.csv")

drg.boxplot<- function(callx ,geom_boxplot ){




df<-DRG_data%>%
  rename(drg_code="DRG.Definition")%>% #renaming it
  mutate(drg_code=stri_extract_first(drg_code, regex="\\d+"))


names(df) <- names(df)%>% make.names()

ggplot(df, mapping= aes(x=drg_code, y= get(callx) ) )+
  geom_boxplot( col= 'red', outlier.shape = NA)+ #cancelling the outliers
  scale_y_continuous(trans='log10')+
  labs(title="Box plot",
       subtitle= paste(callx, "grouped by DRG code"), #all necessary titles
       caption="Source: DRG_data",
       x="DRG code",
       y=callx)+
  theme(axis.text.x = element_text(angle = 90, size = 6,hjust = 1))


}

