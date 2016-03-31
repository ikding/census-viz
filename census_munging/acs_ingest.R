# Import libraries ----
library(acs)

# Download ACS (American Community Survey) data ----
# http://eglenn.scripts.mit.edu/citystate/wp-content/uploads/2013/06/wpid-working_with_acs_R3.pdf
# http://eglenn.scripts.mit.edu/citystate/2013/07/acs-r-example-downloading-all-the-tracts-in-a-county-or-state/
# api.key.install(key = '') need to request your own key at Census API: http://api.census.gov/data/key_signup.html

# Tables of interest:
# B01001: Sex by Age
# B01003: Total Population

# Income:
# B19001: Household Income In The Past 12 Months
# B19013: Median Household Income In The Past 12 Months
# B19101: Family Income In The Past 12 Months
# B19113: Median Family Income In The Past 12 Months
# B19201: Nonfamily Household Income In The Past 12 Months
# B19202: Median Nonfamily Household Income In The Past 12 Months
# B19301: Per Capita Income In The Past 12 Months

# Difference between household and family income: http://economistsoutlook.blogs.realtor.org/2014/04/08/median-income-family-vs-household/

acsSave <- function(endyear = 2011, span = 5, state, geo_level = c("county", "tract", "block group"), table.number = "B01003", output_path = file.path("data", "census", "acs")){

    # This function download the data from acs and write the dataframe to tab-delimited text file.

    library(acs)

    # Load state FIPS list and convert to integers
    state_fips <- read.csv("data/fips.csv", stringsAsFactors = F)
    state_code <- state_fips[state_fips$twoletter == state,]$code
    if (length(state_code) != 1){
        print(paste("State" , state, "does not exist."))
        break
    }

    # geo.make geography depending on state and geo_level specified by user

    if (geo_level == "county"){
        geo <- geo.make(state = state_code, county = "*", check = F)
    } else if (geo_level == "tract"){
        geo <- geo.make(state = state_code, county = "*", tract = "*", check = F)
    } else if (geo_level == "block group"){
        geo_interim <- acs.fetch(endyar = endyear, span = span, geography=geo.make(state = state_code, county="*"), table.number="B01003")
        geo <- geo.make(state = state_code, county = as.numeric(geography(geo_interim)$county), tract = "*", block.group = "*", check = F)
    } else {
        print("Supported geo_level: county, tract, block group")
        break
    }

    acsData <- acs.fetch(endyear = endyear, span = span, geography = geo, table.number = table.number)

    # Get fips code from state, county, and block groups, when available,
    # and concatenate to get long-form FIPS for plotting.

    acsDataGeo <- geography(acsData)
    fips <-  paste0(sprintf("%02s", acsDataGeo$state))

    if ("county" %in% names(acsDataGeo)){
        fips <- paste0(fips, sprintf("%03s", acsDataGeo$county))
    }

    if ("tract" %in% names(acsDataGeo)){
        fips <- paste0(fips, sprintf("%06s", acsDataGeo$tract))
    }

    if ("blockgroup" %in% names(acsDataGeo)){
        fips <- paste0(fips, sprintf("%01s", acsDataGeo$blockgroup))
    }

    acsDataGeo$fips <- fips

    # Get estimates from acs data; merge fips codes to the dataframe
    acsDataEst <- data.frame(estimate(acsData))
    acsDataEst$NAME <- rownames(acsDataEst)

    # Get standard errors from acs data; merge fips codes to the dataframe
    acsDataErr <- data.frame(standard.error(acsData))
    acsDataErr$NAME <- rownames(acsDataErr)

    acsDataAll <- merge(acsDataEst, acsDataErr, by = c("NAME"), suffixes = c("", ".err"))
    acsDataAll <- merge(acsDataGeo, acsDataAll)

    # Write output to tab-delimited file in the output_path
    write.table(acsDataAll, file = file.path(output_path, paste0(state, "_", sub(" ", "", geo_level), "_", endyear-span+1, "-", endyear, "_", table.number, ".txt")), sep = "\t", row.names = F)

    return(acsDataAll)

}

# Example use of functions (not run):
# acsSave(endyear = 2012, state = "MD", geo_level = "county", table.number = "B01001")
# acsSave(endyear = 2010, state = "MD", geo_level = "tract", table.number = "B01001")
# acsSave(endyear = 2012, state = "DC", geo_level = "block group", table.number = "B01001")

# Use nested for loops to download every single combination of the data
# Supported year range for 5-year API: http://www.census.gov/data/developers/data-sets/acs-survey-5-year-data.html

# Small test set
# list_year = 2011 # seq(2010, 2013, 1)
# list_state = c("MD") # c("DC", "MD", "VA")
# list_geo = c("county", "tract", "block group")
# list_table = c("B01003") # c("B01001", "B01003", "B19001", "B19013")

# Large test set
# list_year = seq(2010, 2013, 1)
list_year = 2014 # let's just get 2014 data
list_state = c("CA", "DC", "MD", "NY", "VA")
list_geo = c("county", "tract", "block group")
list_table = c("B01001", "B01003", "B19001", "B19013")
output_path = file.path("data", "census", "acs")

start = proc.time()
counter = 0

for (y in list_year){
    for (s in list_state){
        for (g in list_geo){
            for (t in list_table){
                counter = counter + 1
                print(paste(counter, y, s, g, t))
                if (!file.exists((file.path(output_path, paste0(s, "_", sub(" ", "", g), "_", y-4, "-", y, "_", t, ".txt"))))){
                    acsSave(endyear = y, state = s, geo_level = g, table.number = t)
                }
            }
        }
    }
}

end = proc.time()

end-start # It took about 55 minutes on my home internet to download all 144 combinations
