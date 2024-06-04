library(GD)
library(tidyverse)
library(basicPlotteR)
library(ggplot2)
library(ggrepel)
library(gghighlight)

sesu_change <- function (gdlist, su,label=su)
{
  nsu <- length(su)
  if (nsu < 2) {
    stop("At least two sizes of spatial unit are required for comparison.\n\n")
  }
  su <- su
  nsu <- length(su)
  var <- as.character(gdlist[[1]]$Factor.detector$Factor$variable)
  qv <- t(sapply(gdlist, function(x) x$Factor.detector$Factor$qv))
  sig <- t(sapply(gdlist, function(x) x$Factor.detector$Factor$sig))
  #qv[which(sig >= 0.05)] <- NA
  
  qv_1 <- data.frame(qv)
  
  label <- label
  
  colnames(qv_1) <- var
  
  qv90 <- apply(qv, 1, function(x) quantile(x, 0.9, na.rm = TRUE))
  
  rnames <- su
  
  cnames <- var
  
  qv2 <- qv_1 %>% gather(key = variable, value = qvalue)
  
  qv2$unit <- rep(seq(1,nsu), nrow(qv2)/length(su))
  
  qv2$variable <- as.factor(qv2$variable)
  
  qv90_2 <- data.frame(qv90)
  
  qv90_2$unit <- seq(1,length(su))
  
  qv90_2$variable <- "90% quantile"
  
  colnames(qv90_2)[1] <- "qvalue"
  
  final_data <- rbind(qv90_2,qv2)
  
  # p <- ggplot(final_data,aes(x= unit,label=variable))+geom_line(aes(y=qvalue,color=variable))+
  #   geom_point(aes(y=qvalue,shape=variable,color=variable))+
  #   scale_shape_manual(values=seq(0,15))+scale_x_continuous(breaks = c(1,2,3))
  
  
  p <- ggplot(subset(final_data, variable != '90% quantile'),aes(x= unit,y=qvalue))+geom_line(aes(color=variable),size=0.8)+
    geom_point(aes(shape=variable,color=variable),size=3)+  
    geom_line(data = subset(final_data, variable == '90% quantile'),size = 1, color = 'black')+
    scale_shape_manual(values=seq(0,15))+
    scale_x_continuous(breaks = seq(1,nsu),labels = label)+
    
    ggrepel::geom_label_repel(data = subset(final_data, variable == '90% quantile')[nsu,],
                              aes(x= unit,y=qvalue,label=variable),max.overlaps=Inf,min.segment.length = 0,
                              direction="y", hjust = "left",color = "red",box.padding = 0.3,size = 5)+
    # xlab("Size of spatial unit")+ylab("Q Value")+
    labs(x="Size of spatial unit",y="Q Value")+
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
      # strip.text.x = element_text(size = 12, face = "bold", color = "black"),
      # strip.background = element_rect(fill = "white", color = "black", size = 1),
      legend.position = "top",
      legend.background = element_rect(fill = "transparent"),
      
      legend.text = element_text(size = 13,face = "bold"))+
    guides(color = guide_legend(override.aes = list(size = 3, lwd = 1)))+
    theme(aspect.ratio = 1)
  print(p)
  return(p)
}

