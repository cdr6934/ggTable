library(tidyverse)
library(xtable)


# Creates the R structure
ggTable <- structure(list(), class = "ggTableObject")

# Create dummy data
salesData <- tibble(District=c(1,1,1,2,2,2),
                        Name=c("S1","S2","S3","S4","S5","S6"),
                        NetSales=c(1000,1500,3000,2000,1500,3000),
                        Units=c(50,120,52,12,64,52)) %>%
  mutate(AUR = NetSales / Units)


ggTable$data <- salesData


# Generates District Totals
districtTotals <- salesData %>%
  group_by(District) %>%
  summarize(Name = "Total",
            NetSales = sum(NetSales),
            Units = sum(Units),
            AUR = sum(NetSales) / sum(Units) )

ggTable$groupTotals <- districtTotals



# Generates Grand Total
grandTotals <- districtTotals %>%
  summarize(District = "",
    Name = "GrandTotal",
    NetSales = sum(NetSales),
    Units = sum(Units),
    AUR = sum(NetSales)/sum(Units),2)

ggTable$grandTotals <- grandTotals


ggTable

# Binds district totals to frame
totals <- rbind(salesData, salesTotals) %>%
  arrange(District, Name)

#Binds grand total to frame
totals <- rbind(totals, grandTotals)

#Generate LaTeX table to display
xtable(totals)
