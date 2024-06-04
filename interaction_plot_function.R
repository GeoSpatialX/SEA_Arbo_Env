library(GD)
library(tidyverse)
library(basicPlotteR)
library(ggplot2)
library(ggrepel)
library(gghighlight)

interaction_plot <- function(model_result){
  
intera <- data.frame(model_result$Interaction.detector$Interaction)
intera_final <- intera %>% mutate_at(vars(var1,var2,interaction),funs(factor))
intera_final$var1 <- fct_infreq(intera_final$var1)
intera_final$var2 <- fct_infreq(intera_final$var2)
largest_val <-  intera_final  %>% filter(qv12 == max(qv12))
largest_val$qv12 <- round(largest_val$qv12,3)


ps2 <- ggplot(intera_final,aes(x=var2,y=var1))+
  coord_flip()+
  geom_point(aes(fill=interaction,size=qv12), shape = 21)+
  ggrepel::geom_text_repel(data = largest_val,aes(x= var2,y=var1,label=qv12),max.overlaps=Inf,min.segment.length = 0,
  direction="y", hjust = "left",color = "black",box.padding = 0.3,size = 6)+
  scale_fill_manual("Interaction",values = c("Enhance, bi-" = "darkorange", "Enhance, nonlinear" = "red",
                                             "Weaken, uni-" = "lightblue", "Weaken, nonlinear" = "darkblue"))+
  scale_size_continuous("Q value")+
  labs(x="Variable",y=NULL)+
  theme_classic() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size=20,face="bold",margin = margin(t = 8, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size=20,face="bold",margin = margin(t = 0, r = 8, b = 0, l = 0)),
    axis.text.x = element_text(angle=90,vjust = 0.5,hjust=0.3,size =20,face = "bold"),
    axis.text.y = element_text(size = 20,face = "bold"),
    legend.key.size = unit(9, "mm"),
    legend.title = element_text(face="bold"),
    legend.position = c(0.8,0.7),
    legend.background = element_rect(fill = "transparent",colour = 'black',linewidth=0.2),
    legend.text = element_text(size = 13,face="bold"))+
    guides(fill = guide_legend(override.aes = list(size=4)))+
  theme(aspect.ratio = 1)
 # 
  print(ps2)
  return(ps2)
}
