# R Utility Functions

# Getting data from a CARTO API
getData_phlCartoApi <- function(query,format = 'CSV'){
  
  
  # construct Request URL.
  service <- 'https://phl.carto.com/api/v2/sql?q='
  query   <- query
  format  <- format
  url     <- URLencode(
    paste(
      service
      , query
      ,"&Format=", format
      , sep=''
    )
  )
  
  # Get Data
  request <- httr::GET(url)
  
  # Header returns four items, the data is in the rows
  # item per https://carto.com/developers/sql-api/reference/#operation/getSQLStatement
  data_raw    <- httr::content(request)$rows
  
  #TODO: add any level of testing or error handling...
  
  # Write data to a data.frame
  data <- do.call(
    rbind
    , data_raw
  ) %>%
    as.data.frame %>%
    # Coerce all data to characters from lists
    mutate_all(as.character) %>%
    # Rewrite nulls to NA
    mutate_all(na_if,"NULL")
  return(data)
}




# capture from Wonder API
# Wonder API Documentation:https://wonder.cdc.gov/wonder/help/WONDER-API.html

getData_wonderAPI <- function(
      filePath_xml
    , database
  )
{
  # Pulled from https://github.com/hrbrmstr/wondr/blob/master/R/wondr.r
  z <- httr::POST(
        sprintf("https://wonder.cdc.gov/controller/datarequest/%s", database)
      , body=list(request_xml=xml2::read_xml(filePath_xml))
      , encode = "form"
    ) %>%
    httr::content(as = "text") %>%
    xml2::read_xml()
  
  # Add the ability to parse this xml into something workable.
  # potentially from: https://github.com/socdataR/wonderapi
  # This beautiful and now defunt repo.
  
  
  return(z)
  
  
}
