#### Fijian Lasioglossum Elevational Networks ####
# Created by Particia S. Slattery (patricia.slattery@flinders.edu.au)
# Finalised Jun 2024

#### 0.0. Set up ####
# Install packages
install.packages("vegan")
install.packages("bipartite")
install.packages("ggplot2")
# Load packages
library(vegan)
library(bipartite)
library(ggplot2)
# Set working directory and load in matrices
setwd("~/Uni/P8_FijianHomalictusElevation")
FijiLasioWEIGHT <- read.csv("3.0_FijiHomaWeighted.csv", 
                            row.names = 1)
FijiLasioBIN <- read.csv("3.0_FijiHomaBinary.csv", 
                         row.names = 1)

#### 1.0. Weighted Data ####
##### 1.1. Matrix ####
FijiLasioWEIGHTnestmat <- visweb(FijiLasioWEIGHT, 
                                 type = "nested", 
                                 labsize = 4, 
                                 text = "interaction", 
                                 textsize = 3.5, 
                                 textcol = "blue")
##### 1.2. Statistics ####
WEIGHTnetlevelstats <- networklevel(FijiLasioWEIGHT)
capture.output(WEIGHTnetlevelstats, 
               file = "3.0_WEIGHTnetlevelstats.txt")
#oecosimu simulations
oecoWEIGHTnetlevelstats <- oecosimu(FijiLasioWEIGHT,
                                    nestfun = networklevel,
                                    method = "r00",
                                    nsimul = 1000)
capture.output(oecoWEIGHTnetlevelstats, 
               file = "3.0_oecosimuWEIGHTnetlevelstats.txt")
#### 2.0. Binary Data ####
##### 2.1. Network ####
FijiLasioBINweb <- plotweb(FijiLasioBIN, 
                           method = "normal", 
                           text.rot = 90)

##### 2.2. Nestedness Matrix ####
# Nested layout
FijiLasioBINnestmat <- visweb(FijiLasioBIN, type = "nested")
# Normal layout
FijiLasioBINnormalmat <- visweb(FijiLasioBIN, type = "normal")
##### 2.3. Statistics ####
BINnetlevelstats <- networklevel(FijiLasioBIN)
capture.output(BINnetlevelstats, 
               file = "3.0_BINnetlevelstats.txt")
#oecosimu simulations
oecoBINnetlevelstats <- oecosimu(FijiLasioBIN, 
                                 nestfun = networklevel, 
                                 method = "r00", 
                                 nsimul = 1000)
capture.output(oecoBINnetlevelstats, 
               file = "3.0_oecosimuBINnetlevstats.txt")

#### 3.0. Cumulative Species Loss graph ####
CumSppLoss <- read.csv("3.0_CumSppLossData.csv")

gradient <- c(twelve = "#74B5E3",
              ten = "#1D76BB",
              eight = "#2A3B8F",
              six = "#046938",
              four = "#388436",
              two = "#619447",
              zero = "#87B975")

LossFig <- ggplot(CumSppLoss, mapping = aes(Elevation, Species)) +
  labs(x = "Elevation (m above sea level)",
       y = "Cumulative species loss") +
  scale_x_continuous(breaks = c(1200, 1000, 800,
                                600, 400, 200, 0),
                     labels = c(1200, 1000, 800,
                                600, 400, 200, 0), 
                     transform = "reverse") + 
  scale_y_continuous(breaks = c(0, 7, 14, 21, 28)) +
  geom_point(size = 5,
             colour = gradient) +
  theme(axis.line = element_line(),
        axis.title = element_text(size = 12),
        axis.text = element_text(colour = "black",
                                 size = 10),
        panel.background = element_blank(),
        panel.grid = element_blank()) 

# save the final figure 
ggsave("3.1_FijiLasioCumSppLoss.png", LossFig)
ggsave("3.1_FijiLasioCumSppLoss.pdf", LossFig)
