##############################################################################
# Main Script to perfomr the analysis of the paper about Debt Sustainability #
##############################################################################

# 1. Read the data (General Government Debt as a percentage of GDP from OECD countries)
if (!file.exists("data")) {
        dir.create("data")
}

debtdata <- read.csv("./data/debt.csv", colClasses = "character")
debtdata <- data.frame(CountryCode = debtdata$ï..LOCATION, Year = debtdata$TIME,
                       DebtRatio = debtdata$Value)