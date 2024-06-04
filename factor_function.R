library(GD)
library(tidyverse)
library(basicPlotteR)
library(ggplot2)
library(ggrepel)
library(gghighlight)

factor_plot <- function(model_result){

fac_data <- data.frame(model_result$Factor.detector$Factor) %>% arrange(desc(qv))%>%select(variable,qv)
fac_data$variable <- as.factor(fac_data$variable)
fac_data_final <- fac_data %>% mutate(max_qv=ifelse(qv==max(qv), "1", "0"))

ps <- ggplot(fac_data_final,aes(x=reorder(variable,qv),y=qv,fill=max_qv))+
  geom_col(width=0.5)+
  scale_fill_manual( values = c( "1"="red", "0"="darkgray" ), guide = FALSE )+
  coord_flip()+
  geom_text(aes(label = round(qv,2),fontface=2), hjust = 0.5,size=5)+
  labs(x="Variable",y="Q value")+
  theme_bw() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size=20,face="bold",margin = margin(t = 8, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size=20,face="bold",margin = margin(t = 0, r = 8, b = 0, l = 0)),
    legend.title = element_blank(),
    axis.text.x = element_text(size =20,face = "bold"),
    axis.text.y = element_text(size = 20,face = "bold"),
    legend.key.width = unit(3, "line"),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent"),
    
    legend.text = element_text(size = 10))+
  guides(color = guide_legend(override.aes = list(size = 3, lwd = 1)))+
  theme(aspect.ratio = 1)
print(ps)
return(ps)

}


