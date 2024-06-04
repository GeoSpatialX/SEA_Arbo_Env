sesu_change <- function (gdlist, su)
{
  nsu <- length(su)
  if (nsu < 2) {
    stop("At least two sizes of spatial unit are required for comparison.\n\n")
  }
  var <- as.character(gdlist[[1]]$Factor.detector$Factor$variable)
  qv <- t(sapply(gdlist, function(x) x$Factor.detector$Factor$qv))
  sig <- t(sapply(gdlist, function(x) x$Factor.detector$Factor$sig))
  qv[which(sig >= 0.05)] <- NA
  qv90 <- apply(qv, 1, function(x) quantile(x, 0.9, na.rm = TRUE))
  rnames <- su
  cnames <- var
  qv <- data.frame(qv)
  sig <- data.frame(sig)
  ncol.qv <- ncol(qv)
  text.location.y <- apply(qv, 2, function(x) x[!is.na(x)][1])
  p <- ggplot(aes(x=su,y=qv,color=text.location.y))+geom_line()
  # par(mar = c(5.6, 6.3, 2.1, 7))
  # matplot(x = su, y = qv, type = "p", pch = 1:ncol.qv - 1,
  #         col = 1:ncol.qv + 1, xaxt = "n", xlab = "",
  #         ylab = "", las = 1,cex=1.3,cex.lab=1.7,cex.axis=1.7,font=2)#left
  # title(ylab="Q Value", line=4, cex.lab=1.7,font=2)#left
  # title(xlab="Size of spatial unit", line=3.6, cex.lab=1.9,font=6)
  # axis(1, at = rnames, labels = rnames,cex.axis = 1.7,font=2)
  # matlines(x = rnames, y = qv,lwd = 1.8,type = "l", lty = 1:ncol.qv,
  #          col = 1:ncol.qv + 1)
  # text.location.y <- apply(qv, 2, function(x) x[!is.na(x)][1])
  # text(x = rnames[1], y = text.location.y, labels = cnames,
  #      pos = 4, col = 1:ncol.qv + 1,cex=1.2,font = 2) # name of the variable
  # 
  # # addTextLabels(x = rnames[1], y = text.location.y, labels = cnames, cex.label = 0.3,  col.label = 1:ncol.qv + 1,
  # #               cex=1.2)
  # par(new = T)
  # plot(x = su, y = qv90, type = "b", pch = 16, cex = 1.0, axes = F,
  #      xlab = NA, ylab = NA,lwd = 1.6)
  # axis(side = 4, las = 1,cex=1.1,cex.axis = 1.7,font = 2) ## right
  # mtext(side = 4, line = 5,cex=1.7 ,"The 90% quantile of Q values")
  # text(x = rnames[nsu], y = qv90[nsu], labels = "90% quantile",
  #      pos = 2,cex=1.5)
  # par(mar = c(5.1, 4.1, 4.1, 2.1))
}





su <- c(1,2,3)
nsu <- length(su)
var <- as.character(mod_list2[[1]]$Factor.detector$Factor$variable)
qv <- t(sapply(mod_list2, function(x) x$Factor.detector$Factor$qv))
sig <- t(sapply(mod_list2, function(x) x$Factor.detector$Factor$sig))
qv[which(sig >= 0.05)] <- NA

qv_1 <- data.frame(qv)

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
  scale_x_continuous(breaks = seq(1,nsu))+
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
    
    legend.text = element_text(size = 10))+
  guides(color = guide_legend(override.aes = list(size = 3, lwd = 1)))+
  theme(aspect.ratio = 1)


p

# +
#   geom_text_repel(
#     aes(x= unit, y=qvalue), nudge_x = 0.1, direction = "y", hjust = "left"
#   )
  #ggrepel::geom_label_repel(aes(x= unit,y=qvalue,label=variable),data=final_data,max.overlaps=Inf,min.segment.length = 0)



# +geom_line(data=qv90_2,aes(x=unit,y=qvalue))+
# geom_point(data=qv90_2,aes(x=unit,y=qvalue,shape=variable))

#+geom_point(aes(shape=variable))


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

sesu_change(mod_list2, su)
ggsave("./try/my_plot2.jpg", p,width = 13, height = 8, dpi = 300,limitsize = FALSE)
