## ROBINS-I 

install.packages("robvis")

library(robvis)

robMVD <- read.csv("C:/Users/lucas/Desktop/risk_nfpitnet.csv", header = TRUE)

rob_summary(data = robMVD, 
            tool = "ROBINS-I",
            overall = T,
            weighted = T)

rob_summary

rob_traffic_light(data = robMVD, 
                  tool = "ROBINS-I",
                  psize = 18,
                  angle = 0)


