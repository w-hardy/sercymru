
## Function to plot quantity by month
fn_plot1gp <- function(data, pracid) {
  
  ## Select codes and timeframe
  tmpdata <- data %>%
    filter(PracticeID == pracid) %>%
    group_by(bnfchem,chemname,month2) %>%
    dplyr::summarise(sumq = sum(Quantity)) 
  
  ##
  tmpdata %>%
    ggplot() + 
    geom_line(aes(x=month2, y=sumq, color=chemname)) + 
    annotate("rect", xmin=99, xmax=101, ymin=min(tmpdata$sumq), ymax=max(tmpdata$sumq), alpha=0.2, fill="red") +
    xlab("Month") + 
    ylab("Sum Quantities (monthly)") +
    scale_x_continuous(breaks = c(50:102)[seq(1,53,by=2)],
                       labels = months$datename[seq(1,53,by=2)]) + 
    ggtitle(pracid) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_discrete(name = "Chemical Name") +
    theme_classic() +
    theme(legend.position = "right",
      axis.text.x = element_text(size=10, angle=90, hjust=1, vjust=0.5),
          axis.text.y = element_text(size=10),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10),
          axis.title = element_text(size=10))
}


## Function to plot quantity by month
fn_plotbnf <- function(data, title) {
  
  ## Select codes and timeframe
  tmpdata <- data %>%
    filter(month2 > startmonth) %>%
    group_by(bnfchem,month2) %>%
    dplyr::summarise(sumq = sum(Items),
                     chemname = first(chemname)) 
  ##
  tmpdata %>%
    #filter(sumq>100000) %>%
    ggplot() + 
    geom_line(aes(x=month2, y=sumq, color=chemname)) + 
    annotate("rect", xmin=99, xmax=101, ymin=0, ymax=max(tmpdata$sumq), alpha=0.1, fill="red") +
    #annotate("rect", xmin=99, xmax=101, ymin=min(tmpdata$sumq), ymax=max(tmpdata$sumq), alpha=0.1, fill="red") +
    xlab("Month") + ylab("Sum Items (monthly)") +expand_limits( y = 0) +
    scale_x_continuous(breaks = c(startmonth:103)[seq(1,len,by=2)],
                       labels = months$datename[seq(1,len,by=2)]) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_discrete(name= "Chemical Name") +
    theme_classic() +
    theme(#legend.position = c(0.35,0.4),
          legend.background = element_rect(fill=alpha('white',0.8)),
          legend.position =  "right",
          axis.text.x = element_text(size=10, angle=90, hjust=1, vjust=0.5),
          axis.text.y = element_text(size=10),
          legend.text = element_text(size=7),
          legend.title = element_text(size=7),
          axis.title = element_text(size=10))
}
