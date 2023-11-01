# First read the dataframe from excel worksheet
indicator_information_df <- readxl::read_excel(here::here("inst/extdata/IndicatorInformation.xlsx"), sheet = "all_indicators")
# Then save this as indicator_information_df
# usethis::use_data(indicator_information_df)

# Let's create a list object that is row-first
# The structure should be:
# for each row in indicator_information-df
# create an empty list
# in the empty list create an id object
   # populate the object with id
# in the empty list create a metadata object
   # populate the metadata with everything other than id

rowwise_id_metadata <- make_rowwise_id_metadata(indicator_information_df)

# Now check the contents of metadata$dataless
sapply(rowwise_id_metadata, isDataless)

# If the indicator is dataless, add no data
# Otherwise, add/generate data

set.seed(12)


rowwise_id_metadata_data_fake <- Map(addData, rowwise_id_metadata)

usethis::use_data(rowwise_id_metadata_data_fake)

# # Now create a list object with id and metadata
#
# tmp <- indicator_information_df |>
#   dplyr::nest_by(id)
#
# metadata <- lapply(tmp$data, as.list)
# ids <- tmp$id
#
# id_metadata <- list(
#   id = ids,
#   metadata = metadata
# )
#
# # Save this
# usethis::use_data(id_metadata, overwrite = TRUE)
#

