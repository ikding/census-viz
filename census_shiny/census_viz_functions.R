# Load dataframe for list of variables and attributes for population and income sub-plots
censusPopTable <- read.csv("data/census_sexbyage_var_converted.csv", stringsAsFactors = F)
acsPopTable <- read.csv("data/acs_sexbyage_var_converted.csv", stringsAsFactors = F)
acsIncomeTable <- read.csv("data/acs_income_var_converted.csv", stringsAsFactors = F)

# Function that needs to be loaded only once ----

sumsqrt <- function(x){
    # sum of squares, then take square root. This function is used to calculate propagation of error for a + b as we combine variables
    return(sqrt(sum(x^2)))
}

getCensusPopPyramidDF <- function(data, fips = "11001002201"){
    # This function is used to get population pyramid dataframe from a user-specified fips. The returned dataframe is used in plotCensusPopPyramid function below.

    DF <- data@data[,str_sub(names(data@data), start = 1, end = 3) == "P01"]
    DF$fips <- rownames(DF)

    single_fips <- DF[fips,]
    single_fips <- data.frame(t(single_fips))

    names(single_fips) <- "population"
    single_fips$population <- as.numeric(as.character(single_fips$population))

    # Re-binning the age groups
    # 15-17 + 18-19 -> 15-19
    single_fips["P0120007a",] <- single_fips["P0120006",] + single_fips["P0120007",]
    single_fips["P0120031a",] <- single_fips["P0120030",] + single_fips["P0120031",]

    # 20 + 21 + 22-24 -> 20-24
    single_fips["P0120010a",] <- single_fips["P0120008",] + single_fips["P0120009",] + single_fips["P0120010",]
    single_fips["P0120034a",] <- single_fips["P0120032",] + single_fips["P0120033",] + single_fips["P0120034",]

    # 60-61 + 62-64 -> 60-64
    single_fips["P0120019a",] <- single_fips["P0120018",] + single_fips["P0120019",]
    single_fips["P0120043a",] <- single_fips["P0120042",] + single_fips["P0120043",]

    # 65-66 + 67-69 -> 65-69
    single_fips["P0120021a",] <- single_fips["P0120020",] + single_fips["P0120021",]
    single_fips["P0120045a",] <- single_fips["P0120044",] + single_fips["P0120045",]

    single_fips$var <- row.names(single_fips)

    single_fips <- merge(single_fips, censusPopTable, by.x = "var", by.y = "variable")
    single_fips <- subset(single_fips, age != "Total")
    single_fips$age <- factor(single_fips$age, levels = unique(single_fips$age), labels = unique(single_fips$age))
    single_fips$gender <- factor(single_fips$gender, levels = c("Male", "Female"))
    single_fips[single_fips$gender == "Male",]$population = -1 * single_fips[single_fips$gender == "Male",]$population

    return(single_fips)
}

plotCensusPopPyramid <- function(data){

    # This function is used to plot population pyramid, using the dataframe from the previous function, getCensusPopPyramidDF.

    p <- ggplot(data, aes(x = age, y = population, fill = gender))
    # Subsetting arg in geom_bar was deprecated in ggplo2 v2.0.
    # See here: http://stackoverflow.com/questions/34588232/subset-parameter-in-layers-is-no-longer-working-with-ggplot2-2-0-0
    p <- p + geom_bar(data = data[data$gender == "Female",], stat = "identity")
    p <- p + geom_bar(data = data[data$gender == "Male",], stat = "identity")
    p <- p + coord_flip()
    p <- p + scale_fill_brewer(palette = "Set1")
    p <- p + theme_bw() + theme(legend.position="top")
    p
}

getACSPopPyramidDF <- function(data, fips = "11001002201"){

    # This function is used to get population pyramid dataframe from a user-specified fips. The returned dataframe is used in plotACSPopPyramid function below.

    DF <- data@data[,str_sub(names(data@data), start = 1, end = 6) == "B01001"]
    DF$fips <- rownames(DF)

    single_fips <- DF[fips,]
    single_fips <- data.frame(t(single_fips))

    names(single_fips) <- "population"
    single_fips$population <- as.numeric(as.character(single_fips$population))

    # Re-binning of population bins and calculate the new error using propagation of error
    # 15-17 + 18-19 -> 15-19
    single_fips["B01001_007a",] <- single_fips["B01001_006",] + single_fips["B01001_007",]
    single_fips["B01001_031a",] <- single_fips["B01001_030",] + single_fips["B01001_031",]
    single_fips["B01001_007a.err",] <- sumsqrt(c(single_fips["B01001_006.err",], single_fips["B01001_007.err",]))
    single_fips["B01001_031a.err",] <- sumsqrt(c(single_fips["B01001_030.err",], single_fips["B01001_031.err",]))

    # 20 + 21 + 22-24 -> 20-24
    single_fips["B01001_010a",] <- single_fips["B01001_008",] + single_fips["B01001_009",] + single_fips["B01001_010",]
    single_fips["B01001_034a",] <- single_fips["B01001_032",] + single_fips["B01001_033",] + single_fips["B01001_034",]
    single_fips["B01001_010a.err",] <- sumsqrt(c(single_fips["B01001_008.err",], single_fips["B01001_009.err",], single_fips["B01001_010.err",]))
    single_fips["B01001_034a.err",] <- sumsqrt(c(single_fips["B01001_032.err",], single_fips["B01001_033.err",], single_fips["B01001_034.err",]))

    # 60-61 + 62-64 -> 60-64
    single_fips["B01001_019a",] <- single_fips["B01001_018",] + single_fips["B01001_019",]
    single_fips["B01001_043a",] <- single_fips["B01001_042",] + single_fips["B01001_043",]
    single_fips["B01001_019a.err",] <- sumsqrt(c(single_fips["B01001_018.err",], single_fips["B01001_019.err",]))
    single_fips["B01001_043a.err",] <- sumsqrt(c(single_fips["B01001_042.err",], single_fips["B01001_043.err",]))

    # 65-66 + 67-69 -> 65-69
    single_fips["B01001_021a",] <- single_fips["B01001_020",] + single_fips["B01001_021",]
    single_fips["B01001_045a",] <- single_fips["B01001_044",] + single_fips["B01001_045",]
    single_fips["B01001_021a.err",] <- sumsqrt(c(single_fips["B01001_020.err",], single_fips["B01001_021.err",]))
    single_fips["B01001_045a.err",] <- sumsqrt(c(single_fips["B01001_044.err",], single_fips["B01001_045.err",]))

    single_fips$var <- row.names(single_fips)

    single_fips <- merge(single_fips, acsPopTable, by.x = "var", by.y = "variable")
    single_fips <- subset(single_fips, age != "Total")
    single_fips$age <- factor(single_fips$age, levels = single_fips$age, labels = single_fips$age)
    single_fips[single_fips$gender == "Male",]$population = -1 * single_fips[single_fips$gender == "Male",]$population

    single_fips <- cbind(subset(single_fips, measure == "estimate", select = c("var", "population", "gender", "age")), subset(single_fips, measure == "error", select = c("population")))
    names(single_fips) <- c("var", "population", "gender", "age", "error")

    return(single_fips)
}

plotACSPopPyramid <- function(input_DF){
    # This function is used to plot population pyramid, using the dataframe from the previous function, getACSPopPyramidDF.
    p <- ggplot(data = input_DF, aes(x = age, y = population, fill = gender))
    p <- p + geom_bar(data = input_DF[input_DF$gender == "Female",], stat = "identity")
    p <- p + geom_bar(data = input_DF[input_DF$gender == "Male",], stat = "identity")
    p <- p + geom_errorbar(aes(ymax = population + error, ymin = population - error))
    p <- p + coord_flip()
    p <- p + scale_fill_brewer(palette = "Set1")
    p <- p + theme_bw() + theme(legend.position="top")
    p
}

getACSIncomeDF <- function(data, fips = "11001002201"){

    # This function is used to plot population pyramid and error bars, using the dataframe from the previous function, getACSPopPyramidDF.

    DF <- data@data[,str_sub(names(data@data), start = 1, end = 6) == "B19001"]
    DF$fips <- rownames(DF)

    single_fips <- DF[fips,]
    single_fips <- data.frame(t(single_fips))

    names(single_fips) <- "population"
    single_fips$population <- as.numeric(as.character(single_fips$population))

    # Re-binning of income-bins and calculate new error bars.
    # 0-15k
    single_fips["B19001_003a",] <- single_fips["B19001_002",] + single_fips["B19001_003",]
    single_fips["B19001_003a.err",] <- sumsqrt(c(single_fips["B19001_002.err",], single_fips["B19001_003.err",]))

    # 15-30k
    single_fips["B19001_006a",] <- single_fips["B19001_004",] + single_fips["B19001_005",] + single_fips["B19001_006",]
    single_fips["B19001_006a.err",] <- sumsqrt(c(single_fips["B19001_004.err",], single_fips["B19001_005.err",], single_fips["B19001_006.err",]))

    # 30-45k
    single_fips["B19001_009a",] <- single_fips["B19001_007",] + single_fips["B19001_008",] + single_fips["B19001_009",]
    single_fips["B19001_009a.err",] <- sumsqrt(c(single_fips["B19001_007.err",], single_fips["B19001_008.err",], single_fips["B19001_009.err",]))

    # 45-60k
    single_fips["B19001_011a",] <- single_fips["B19001_010",] + single_fips["B19001_011",]
    single_fips["B19001_011a.err",] <- sumsqrt(c(single_fips["B19001_010.err",], single_fips["B19001_011.err",]))

    single_fips$var <- row.names(single_fips)
    single_fips <- merge(single_fips, acsIncomeTable, by.x = "var", by.y = "variable")
    single_fips$income <- factor(single_fips$income, levels = unique(single_fips$income), labels = unique(single_fips$income))

    single_fips <- cbind(subset(single_fips, measure == "estimate", select = c("var", "population", "income")), subset(single_fips, measure == "error", select = c("population")))
    names(single_fips) <- c("var", "population", "income", "error")

    return(single_fips)
}


plotACSIncomeBar <- function(input_DF){

    # This function is used to plot income bin barchart, using the dataframe from the previous function, getACSIncomeDF

    p <- ggplot(data = input_DF, aes(x = income, y = population, fill = income))
    p <- p + geom_bar(stat = "identity")
    p <- p + geom_errorbar(aes(ymax = population + error, ymin = population - error), width = 0.5)
    p <- p + scale_fill_manual(values = c(brewer.pal(9, "Blues")[2:9], "#062553", "#041B3C"))
    p <- p + theme_bw() + theme(legend.position="none")
    p
}

