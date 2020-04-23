##############################################################################
# Main Script to perfomr the analysis of the paper about Debt Sustainability #
##############################################################################

# 1. Read the data (General Government Debt as a percentage of GDP from OECD countries)
if (!file.exists("data")) {
        dir.create("data")
}

debtdata <- read.csv("./data/debt.csv", colClasses = "character")
debtdata <- data.frame(CountryCode = debtdata$ï..LOCATION, Year = debtdata$TIME,
                       DebtRatio = debtdata$Value, stringsAsFactors = FALSE)

# 2. Doing a boxplot analysis

if(!("ggplot2" %in% rownames(installed.packages()))) {
        install.packages("ggplot2")
}

library(ggplot2)

ggplot(debtdata, aes(x = CountryCode, y = as.numeric(DebtRatio))) + 
        geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) +
        xlab("OECD Countries") + ylab("Debt to GDP Ratio")

# 3. Reshape data
if (!("reshape2" %in% rownames(installed.packages()))) {
        install.packages("reshape2")
}
library(reshape2)

debtdata <- dcast(debtdata, Year ~ CountryCode, value.var = "DebtRatio")

# 4. Transform Year variable into Date format
year <- seq(as.Date("1995/01/01"), as.Date("2019/01/01"), by = "year")
debtdata$Year <- year

# 5. Assign country names to columns in dataset
x <- c("Year", "Austria", "Australia", "Belgium", "Canada", "Switzerland", 
       "Chile", "Czech Republic", "Germany", "Denmark", "Spain", "Estonia", 
       "Finland", "France", "United Kingdom", "Greece", "Hungary",
       "Ireland", "Iceland", "Israel", "Italy", "Japan", "Lithuania", "Luxemburg",
       "Latvia", "Mexico", "Netherlands", "Norway", "Poland", "Portugal",
       "Slovak Republic", "Slovenia", "Sweeden", "Turkey", "United States")
colnames(debtdata) <- x


