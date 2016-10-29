# Plot counts of zip codes as chloropleth map
require(dplyr)

library(choroplethr)
library(choroplethrZip)
library(choroplethrMaps)

# zip_choropleth(zip_count_2016,
#                msa_zoom="New York-Newark-Jersey City, NY-NJ-PA",
#                title="Respondents by Zip Code",
#                legend="Number of Respondents")

nyc_fips = c(36005, 36047, 36061, 36081, 36085)

zip_choropleth(zip_count_2016, county_zoom = nyc_fips, num_colors = 7,
               msa_zoom="New York-Newark-Jersey City, NY-NJ-PA",
               title="Respondents by Zip Code",
               legend="Number of Respondents")

state_choropleth(state_count_2016,
                 title="Respondents by State: NY, NJ, CT",
                 legend="Number of Respondents",
                 zoom = c('connecticut', 'new jersey', 'new york'))