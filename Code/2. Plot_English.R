# Packages
library(tidyverse)
library(lubridate)

# Data frame
load("Rdata/Injury.rda")

# Key dates' info 
## Key dates
Key.dates <- 
  as.Date(c(paste(rep(c("2014","2015","2016","2017"), each = 2),"12", c("08","25"), 
                  sep = "-"), paste(c("2015","2016","2017","2018"),"01", "01", sep = "-")))

## Y positions of important dates' text
dates_ypositions <- Injury %>%
  group_by(FEC_CON) %>%
  mutate(TOTAL = n()) 

# Plot
Injury %>%  
  ggplot(aes(x = FEC_CON, fill = "red")) +
  geom_histogram(stat = "count" ,width =1) + 
  geom_text(aes(x = FEC_CON, y = dates_ypositions$TOTAL,
                label = ifelse(FEC_CON %in% Key.dates, 
                               paste(month(FEC_CON, label = TRUE), 
                                     str_match(FEC_CON, "[:digit:]+$"),sep = " "), "")),
            
            color = ifelse(str_match(Injury$FEC_CON, "[:digit:]+$") == "01","white",
                           "lightgray"),
            vjust = ifelse(Injury$FEC_CON == "2016-12-08", 0.4,
                           ifelse(Injury$FEC_CON == "2016-12-25", -2, 0)), 
            hjust = 1.1, fontface = "plain", 
            size = ifelse(str_match(Injury$FEC_CON, "[:digit:]+$") == "01", 4, 3)) +
  ## Text
  annotate("text",
           label = c("bold(New~year)~is~the~day~with~the~highest~number~of~woundeds~'in'~each~year~','",
                     "followed~by~bold(Christmas~and~the~Little~Candles~Night)",
                     "bold(HOLIDAYS~WITH~EXPLOSIVES~'?')"), 
           color = "white",
           x = as.Date(c("2015-10-01", "2015-08-05", "2015-06-07")), 
           y = c(300,315, 280),
           parse = TRUE)  +
  ## Scales
  scale_x_date(limits = as.Date(c("2014-09-01","2018-02-01")),
               position = 'top', date_breaks = "1 year", date_labels = "%Y",
               expand = c(0,0)) +
  scale_y_reverse(expand = c(0,-4), limits =c(350,-7))  +
  scale_fill_manual(values = "darkred") +
  ## Titles
  labs(title = " BLOODLY MARK:\nexplosives and mines in Colombia")+
  ylab("Number of  woundeds  by explosives or mines") +
  theme(plot.title = element_text(color = 'white', hjust = -0.05, vjust = 2, size = 40),
        axis.title.y = element_text(color = "gray", size = 10,angle = 90, hjust = 1, vjust = 2),
        axis.title.x = element_blank(),
        ## Others      
        text = element_text(family = "serif"),
        plot.margin = unit(c(1, 0.4, 0, 0.3), "cm"),
        plot.background = element_rect(color = "black", fill = 'black'),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length= unit(c(0.4),"cm"), 
        axis.ticks = element_line(color ="gray"),
        axis.text = element_text(color ="gray"),
        legend.position = "none") 

# Remove 
rm(list = setdiff(ls(), "Injury"))
