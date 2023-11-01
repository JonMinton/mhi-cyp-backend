

# To start with, this route will work with the object rowwise_id_metadata_data_fake

# We want to test the following

# Get all records

# Get all records by id

# Get number of indicators


# jsonAsList <- jsonlite::fromJSON(test_indicator_data, simplifyVector = FALSE, flatten = FALSE)

# #* Get all data
# #* @get /
# function(){
#   return(rowwise_id_metadata_data_fake)
# }

#* Get all data which match query elements
#* @get /
search <- function(
    indicator_determinant = "",
    domain = "",
    construct = "",
    source = "",
    dataless = ""
){
  # initially want all indicators, then to winnow down based on further
  # selections
  selector <- rep(TRUE, length(rowwise_id_metadata_data_fake))

  if (
    indicator_determinant != "" &
    indicator_determinant %in% c("indicator", "determinant")
  ) {
    subselector <- sapply(
      rowwise_id_metadata_data_fake,
      function(x) x$metadata$indicator_determinant == indicator_determinant
    )
    selector <- selector & subselector
  }

  if (
    domain != "" &
    domain %in% c(
      "Mental Health Outcomes", "Individual", "Family and Friends",
      "Learning Environment", "Community", "Structural"
    )
  ) {
    subselector <- sapply(
      rowwise_id_metadata_data_fake,
      function(x) x$metadata$domain == domain
    )
    selector <- selector & subselector
  }

  if (
    construct != ""
  ) {
    subselector <- sapply(
      rowwise_id_metadata_data_fake,
      function(x) x$metadata$construct == construct
    )
    selector <- selector & subselector
  }

  if (
    source != ""
  ) {
    subselector <- sapply(
      rowwise_id_metadata_data_fake,
      function(x) x$metadata$source == source
    )
    selector <- selector & subselector
  }

  if (
    dataless != "" &
    dataless %in% c("TRUE", "true", "T", "FALSE", "false", "F")
  ) {
    if (dataless %in% c("TRUE", "true", "T")) {
      subselector <- sapply(
        rowwise_id_metadata_data_fake,
        function(x) isDataless(x)
      )
    } else if (dataless %in% c("FALSE", "false", "F")) {
        subselector <- sapply(
          rowwise_id_metadata_data_fake,
          function(x) !isDataless(x)
        )
      }
    selector <- subselector & selector

    }
  records <- rowwise_id_metadata_data_fake[selector]

  return(records)
}

#* get all indicator data (not determinants)
#* @param indicators whether an indicator
#* @get /<indicators:bool>
function(indicators){
  if (indicators)
    selector <- sapply(rowwise_id_metadata_data_fake,
                       function(x) x$metadata$indicator_determinant == 'indicator'
                )
  else {
    selector <- sapply(rowwise_id_metadata_data_fake,
                       function(x) x$metadata$indicator_determinant == 'determinant')
  }
  records <- rowwise_id_metadata_data_fake[selector]
  return(records)
}


#* Get number of indicators
#* @get /n_indicators
function(){
  return(length(rowwise_id_metadata_data_fake))
}

#* Get number of indicators by dataless status
#* @param dataless boolean flag for whether dataless
#* @get /n_indicators/<dataless:bool>
function(dataless){
  if (dataless){
    selector <- sapply(rowwise_id_metadata_data_fake,
                       function(x) isDataless(x))

  } else {
    selector <- sapply(rowwise_id_metadata_data_fake,
                       function(x) !isDataless(x))

  }
  records <- rowwise_id_metadata_data_fake[selector]
  result <- length(records)
  return(result)
}

#*



#* Get names of indicators
#* @get /names
function(){
  return(
    sapply(rowwise_id_metadata_data_fake, function(x) x$metadata$indicator)
  )
}


#* get name of indicator
#* @get /names/<id>
#* @param id id of indicator
function(id){
  id <- as.numeric(id)
  selector <- sapply(rowwise_id_metadata_data_fake, function(x) x$id == id)
  this_record <- rowwise_id_metadata_data_fake[selector][[1]]
  return(this_record$metadata$indicator)
}


#* check if dataless
#* @get /dataless/<id>
#* @param id id of indicator
function(id){
  id <- as.numeric(id)
  selector <- sapply(rowwise_id_metadata_data_fake, function(x) x$id == id)
  this_record <- rowwise_id_metadata_data_fake[selector][[1]]
  return(isDataless(this_record))
}

#* get all data if it exists
#* @get /data/<id>
#* @param id id of indicator
function(id){
  id <- as.numeric(id)
  selector <- sapply(rowwise_id_metadata_data_fake, function(x) x$id == id)
  this_record <- rowwise_id_metadata_data_fake[selector][[1]]
  result <- ifelse(
    isDataless(this_record),
    NA,
    this_record$data
  )
}

#* get main data if it exists
#* @get /data/main/<id>
#* @param id id of indicator
function(id){
  id <- as.numeric(id)
  selector <- sapply(rowwise_id_metadata_data_fake, function(x) x$id == id)
  this_record <- rowwise_id_metadata_data_fake[selector][[1]]
  result <- ifelse(
    isDataless(this_record),
    NA,
    this_record$data$main
  )
  return(result)
}

#* get interval data if it exists
#* @get data/intervals/<id>
#* @param id id of indicator
function(id){
  id <- as.numeric(id)
  selector <- sapply(rowwise_id_metadata_data_fake, function(x) x$id == id)
  this_record <- rowwise_id_metadata_data_fake[selectpr][[1]]
  result <- ifelse(
    isDataless(this_record),
    NA,
    this_record$data$ci
  )
  return(result)
}

#* get time series data if it exists
#* @get data/series/<id>
#* @param id id of indicator
#* @serializer rds
function(id){
  id <- as.numeric(id)
  selector <- sapply(rowwise_id_metadata_data_fake, function(x) x$id == id)
  this_record <- rowwise_id_metadata_data_fake[selector][[1]]
  result <- ifelse(
    isDataless(this_record),
    NA,
    this_record$data$time_series
  )
  return(result)
}


#* @get /valid_ids
function(){
  sapply(rowwise_id_metadata_data_fake, function(x) x$id)
}

#* @param dataless boolean flag for dataless indicators
#* @get /valid_ids/<dataless:bool>
function(dataless){
  if (dataless){
    selector <- sapply(rowwise_id_metadata_data_fake,
                     function(x) isDataless(x))
  } else {
    selector <- sapply(rowwise_id_metadata_data_fake,
                       function(x) !isDataless(x))
  }
  records <- rowwise_id_metadata_data_fake[selector]
  result <- sapply(records, function(x) x$id)
  result
}


#* @get /metadata
function() {
  ids <- sapply(rowwise_id_metadata_data_fake, function(x) x$id)
  metadata <- sapply(rowwise_id_metadata_data_fake, function(x) x$metadata)
  md_df <- metadata |> t() |> as.data.frame()
  data <- data.frame(id = ids, md_df)

  return(data)
}


#
#
# #* get data for indicator
# #* @get /data/<id>
# #* @param id id of indicator
# function(id){
#   id <- as.numeric(id)
#   return(jsonAsList[[id]]$data)
# }
#
# #* get main data for indicator
# #* @get /data/main/<id>
# #* @param id id of indicator
# function(id){
#   id <- as.numeric(id)
#   return(jsonAsList[[id]]$data$main[[1]])
# }
#
# #* get ci data for indicator
# #* @get /data/interval/<id>
# #* @param id id of indicator
# function(id){
#   id <- as.numeric(id)
#   return(jsonAsList[[id]]$data$cis)
# }
#
# #* get trend data for indicator
# #* @get /data/trend/<id>
# #* @param id id of indicator
# function(id){
#   id <- as.numeric(id)
#   return(jsonAsList[[id]]$data$trend)
# }
#
