#                         This code is revised from https://cran.r-project.org/web/packages/GD/vignettes/GD.html
#                         Cited: Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) “An optimal parameters-based geographical detector model enhances geographic characteristics of explanatory variables for spatial heterogeneity analysis: Cases with different types of spatial data”, GIScience & Remote Sensing. 57(5), 593-610. doi: 10.1080/15481603.2020.1760434.

library(GD)
library(tidyverse)
library(basicPlotteR)
library(ggplot2)
library(ggrepel)
library(gghighlight)

data1 <- read.csv("city_data.csv")

ab_2001_variable <- colnames(data1)[c(1,4:9,12,22,23,25)]

data1 <- data1 %>% select(ab_2001_variable)

change_re <- read.csv("change_variable_name.csv")

colnames(data1) <- dplyr::recode(
  colnames(data1), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

ab_2001_continues_variable <- colnames(data1)[c(2:7,9:11)]

discmethod <- c("equal","quantile","geometric")

discitv <- c(4:30)

data2 <- na.omit(data1)

model_adm2_ab2001 <- gdm(ab_2001~., continuous_variable = ab_2001_continues_variable,
                         data = data2,
                         discmethod = discmethod, discitv = discitv)

plot(model_adm2_ab2001)

data3 <- read.csv("province_data.csv")

ab_adm1_v <- colnames(data3)[c(1,3:8,11,21,22,25)]

data3 <- data3 %>% select(ab_adm1_v)

colnames(data3) <- dplyr::recode(
  colnames(data3), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

data3 <- na.omit(data3)

model_adm1_ab2001 <- gdm(ab_2001~., continuous_variable = ab_2001_continues_variable,
                         data = data3,
                         discmethod = discmethod, discitv = discitv)


g1 <- factor_plot(model_adm1_ab2001)
g2 <- interaction_plot(model_adm1_ab2001)

ggsave("./graph2/factor_plot_model_adm1_ab2001.jpg", g1,width = 16, height = 8, dpi = 300,limitsize = FALSE)
ggsave("./graph2/interaction_plot_model_adm1_ab2001.jpg", g2,width = 16, height = 8, dpi = 300,limitsize = FALSE)

adm3 <- read.csv("district_data.csv")

adm3_ab_2001 <- colnames(adm3)[c(1,2,4:9,16,17,22)]

adm3_ab2001 <- adm3 %>% select(adm3_ab_2001)

colnames(adm3_ab2001) <- dplyr::recode(
  colnames(adm3_ab2001), 
  !!!setNames(as.character(change_re$new), change_re$old)
)


adm3_ab2001 <- na.omit(adm3_ab2001)

adm3_ab2001 <- adm3_ab2001 %>% mutate_at(vars(ab_2001_continues_variable),funs(round(.,3)))


model_adm3_ab2001 <- gdm(ab_2001~., continuous_variable =ab_2001_continues_variable,
                         data = adm3_ab2001,
                         discmethod = discmethod, discitv = discitv)

su <- c(1,2,3)
x_name <- c("Province-Level","City-Level","District-Level")

mod_list <- list(model_adm1_ab2001,model_adm2_ab2001,model_adm3_ab2001)


sesu_change(mod_list, su,x_name)

compare_graph <- sesu_change(mod_list, su,x_name)
ggsave("./graph1/compare_graph_ab1960.jpg", compare_graph,width = 13, height = 8, dpi = 300,limitsize = FALSE)



##                                                       Ag   2001
data1 <- read.csv("city_data.csv")
ag_2001_variable <- colnames(data1)[c(1,4:9,13,22,23,25)]



ag_2001 <- data1 %>% select(ag_2001_variable)

colnames(ag_2001) <- dplyr::recode(
  colnames(ag_2001), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

ag_2001 <- na.omit(ag_2001)

model_adm2_ag2001 <- gdm(ag_2001~., continuous_variable = ab_2001_continues_variable,
                         data = ag_2001,
                         discmethod = discmethod, discitv = discitv)


data3 <- read.csv("province_data.csv")
ag_2001_2 <- data3 %>% select(ag_2001_variable)

colnames(ag_2001_2) <- dplyr::recode(
  colnames(ag_2001_2), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

ag_2001_2 <- na.omit(ag_2001_2)

model_adm1_ag2001 <- gdm(ag_2001~., continuous_variable = ab_2001_continues_variable,
                         data = ag_2001_2,
                         discmethod = discmethod, discitv = discitv)

g1 <- factor_plot(model_adm1_ag2001)
g2 <- interaction_plot(model_adm1_ag2001)
ggsave("./graph2/factor_plot_model_adm1_ag2001.jpg", g1,width = 16, height = 8, dpi = 300,limitsize = FALSE)
ggsave("./graph2/interaction_plot_model_adm1_ag2001.jpg", g2,width = 16, height = 8, dpi = 300,limitsize = FALSE)



adm3_ag_2001 <- colnames(adm3)[c(1,2,4:9,16,17,23)]

adm3_ag2001 <- adm3 %>% select(adm3_ag_2001)

colnames(adm3_ag2001) <- dplyr::recode(
  colnames(adm3_ag2001), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

adm3_ag2001 <- na.omit(adm3_ag2001)




model_adm3_ag2001 <- gdm(ag_2001~., continuous_variable =ab_2001_continues_variable,
                         data = adm3_ag2001,
                         discmethod = discmethod, discitv = discitv)



mod_list2 <- list(model_adm1_ag2001,model_adm2_ag2001,model_adm3_ag2001)
su <- c(1,2,3)


compare_graph <- sesu_change(mod_list2, su,x_name)
ggsave("./graph1/compare_graph_ag2001.jpg", compare_graph,width = 13, height = 8, dpi = 300,limitsize = FALSE)


##                                                       Ab 1960
data1 <- read.csv("city_data.csv")
ab_1960_variable <- colnames(data1)[c(1,14:21,10,24)]



ab_1960_adm2 <- data1 %>% select(ab_1960_variable)


ab_1960_continues_variable <- ab_2001_continues_variable

colnames(ab_1960_adm2) <- dplyr::recode(
  colnames(ab_1960_adm2), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

ab_1960_adm2 <- na.omit(ab_1960_adm2)

model_adm2_ab1960 <- gdm(ab_1960~., continuous_variable = ab_1960_continues_variable,
                         data = ab_1960_adm2,
                         discmethod = discmethod, discitv = discitv)

plot(model_adm2_ab1960)


data3 <- read.csv("province_data.csv")

ab_1960_adm1 <- data3 %>% select(ab_1960_variable)

colnames(ab_1960_adm1) <- dplyr::recode(
  colnames(ab_1960_adm1), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

ab_1960_adm1 <- na.omit(ab_1960_adm1)



model_adm1_ab1960 <- gdm(ab_1960~., continuous_variable = ab_1960_continues_variable,
                         data = ab_1960_adm1,
                         discmethod = discmethod, discitv = discitv)


g1 <- factor_plot(model_adm1_ab1960)
g2 <- interaction_plot(model_adm1_ab1960)
ggsave("./graph2/factor_plot_model_adm1_ab1960.jpg", g1,width = 16, height = 8, dpi = 300,limitsize = FALSE)
ggsave("./graph2/interaction_plot_model_adm1_ab1960.jpg", g2,width = 16, height = 8, dpi = 300,limitsize = FALSE)

adm3_ab_1960 <- colnames(adm3)[c(1,3,10:15,18,19,20)]

adm3_ab1960 <- adm3 %>% select(adm3_ab_1960)

colnames(adm3_ab1960) <- dplyr::recode(
  colnames(adm3_ab1960), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

adm3_ab1960 <- na.omit(adm3_ab1960)




model_adm3_ab1960 <- gdm(ab_1960~., continuous_variable =ab_1960_continues_variable,
                         data = adm3_ab1960,
                         discmethod = discmethod, discitv = discitv)



mod_list2 <- list(model_adm1_ab1960,model_adm2_ab1960,model_adm3_ab1960)
su <- c(1,2,3)

compare_graph <- sesu_change(mod_list2, su,x_name)
ggsave("./graph1/compare_graph_ag2001.jpg", compare_graph,width = 13, height = 8, dpi = 300,limitsize = FALSE)


##                                                       Ag 1960
data1 <- read.csv("city_data.csv")

ag_1960_variable <- colnames(data1)[c(1,14:21,11,24)]

ag_1960_adm2 <- data1 %>% select(ag_1960_variable)

colnames(ag_1960_adm2) <- dplyr::recode(
  colnames(ag_1960_adm2), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

ag_1960_adm2 <- na.omit(ag_1960_adm2)

model_adm2_ag1960 <- gdm(ag_1960~., continuous_variable = ab_1960_continues_variable,
                         data = ag_1960_adm2,
                         discmethod = discmethod, discitv = discitv)



data3 <- read.csv("province_data.csv")

ag_1960_adm1 <- data3 %>% select(ag_1960_variable)

colnames(ag_1960_adm1) <- dplyr::recode(
  colnames(ag_1960_adm1), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

ag_1960_adm1 <- na.omit(ag_1960_adm1)

ab_1960_adm1 <- ab_1960_adm1 %>% mutate_at(vars(ab_1960_continues_variable,ab_1960), funs(round(., 5)))

model_adm1_ag1960 <- gdm(ag_1960~., continuous_variable = ab_1960_continues_variable,
                         data = ag_1960_adm1,
                         discmethod = discmethod, discitv = discitv)


g1 <- factor_plot(model_adm1_ag1960)
g2 <- interaction_plot(model_adm1_ag1960)
ggsave("./graph2/factor_plot_model_adm1_ag1960.jpg", g1,width = 16, height = 8, dpi = 300,limitsize = FALSE)
ggsave("./graph2/interaction_plot_model_adm1_ag1960.jpg", g2,width = 16, height = 8, dpi = 300,limitsize = FALSE)

adm3_ag_1960 <- colnames(adm3)[c(1,3,10:15,18,19,21)]

adm3_ag1960 <- adm3 %>% select(adm3_ag_1960)

colnames(adm3_ag1960) <- dplyr::recode(
  colnames(adm3_ag1960), 
  !!!setNames(as.character(change_re$new), change_re$old)
)

adm3_ag1960 <- na.omit(adm3_ag1960)

model_adm3_ag1960 <- gdm(ag_1960~., continuous_variable =ab_1960_continues_variable,
                         data = adm3_ag1960,
                         discmethod = discmethod, discitv = discitv)

mod_list2 <- list(model_adm1_ag1960,model_adm2_ag1960,model_adm3_ag1960)
su <- c(1,2,3)

compare_graph <- sesu_change(mod_list2, su,x_name)

ggsave("./graph1/compare_graph_ag1960.jpg", compare_graph,width = 13, height = 8, dpi = 300,limitsize = FALSE)