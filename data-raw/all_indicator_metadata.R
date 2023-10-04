dta <- readxl::read_excel(here::here("inst/extdata/IndicatorInformation.xlsx"), sheet = "all_indicators")
all_indicator_metadata <- jsonlite::toJSON(dta)

usethis::use_data(all_indicator_metadata, overwrite=TRUE)
