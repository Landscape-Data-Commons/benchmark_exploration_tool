#' A function for fetching data from the Landscape Data Commons via API query
#' @description A function for making API calls to the Landscape Data Commons based on the table, key variable, and key variable values. It will return a table of records of the requested data type from the LDC in which the variable \code{key_type} contains only values found in \code{keys}.
#' @param keys Optional character vector. A character vector of all the values to search for in \code{key_type}. The returned data will consist only of records where \code{key_type} contained one of the key values, but there may be keys that return no records. If \code{NULL} then the entire table will be returned. Defaults to \code{NULL}.
#' @param key_type Optional character string. The name of the variable in the data to search for the values in \code{keys}. This must be the name of a variable that exists in the requested data type's table, e.g. \code{"PrimaryKey"} exists in all tables, but \code{"EcologicalSiteID"} is found only in some. If the function returns a status code of 500 as an error, this variable may not be found in the requested data type. If \code{NULL} then the entire table will be returned. Defaults to \code{NULL}.
#' @param data_type Character string. The type of data to query. Note that the variable specified as \code{key_type} must appear in the table corresponding to \doce{data_type}. Valid values are: \code{'gap}, \code{'header}, \code{'height}, \code{'lpi}, \code{'soilstability}, \code{'speciesinventory}, \code{'indicators}, \code{'species}, \code{'dustdeposition}, \code{'horizontalflux}, and \code{'schema'}.
#' @param key_chunk_size Numeric. The number of keys to send in a single query. Very long queries fail, so the keys may be chunked into smaller querieswith the results of all the queries being combined into a single output. Defaults to \code{100}.
#' @param timeout Numeric. The number of seconds to wait for a nonresponse from the API before considering the query to have failed. Defaults to \code{60}.
#' @param verbose Logical. If \code{TRUE} then the function will report additional diagnostic messages as it executes. Defaults to \code{FALSE}.
#' @returns A data frame of records from the requested \code{data_type} which contain the values from \code{keys} in the variable \code{key_type}.
#' @export
fetch_ldc <- function(keys = NULL,
                      key_type = NULL,
                      data_type,
                      key_chunk_size = 100,
                      timeout = 60,
                      verbose = FALSE) {
  user_agent <- "http://github.com/Landscape-Data-Commons/trex"
  base_url <- "https://api.landscapedatacommons.org/api/v1/"
  valid_tables <- data.frame(data_type = c("gap",
                                           "header",
                                           "height",
                                           "lpi",
                                           "soilstability",
                                           "speciesinventory",
                                           "indicators",
                                           "species",
                                           "dustdeposition",
                                           "horizontalflux",
                                           "schema"),
                             table_name = c("dataGap",
                                            "dataHeader",
                                            "dataHeight",
                                            "dataLPI",
                                            "dataSoilStability",
                                            "dataSpeciesInventory",
                                            "geoIndicators",
                                            "geoSpecies",
                                            "dataDustDeposition",
                                            "dataHorizontalFlux",
                                            "tbl-schema/latest"))
  if (!(data_type %in% valid_tables$data_type)) {
    stop(paste0("data_type must be one of the following character strings: ",
                paste(valid_tables$data_type,
                      collapse = ", "),
                "."))
  }
  
  current_table <- valid_tables[["table_name"]][valid_tables$data_type == data_type]
  
  if (!(class(keys) %in% c("character", "NULL"))) {
    stop("keys must be a character string or vector of character strings or NULL.")
  }
  
  if (!(class(key_type) %in% c("character", "NULL"))) {
    stop("key_type must be a character string or NULL.")
  }
  
  if (!is.null(keys) & is.null(key_type)) {
    stop("Must provide key_type when providing keys")
  }
  
  # If there are no keys, grab the whole table
  if (is.null(keys)) {
    if (verbose) {
      message("No keys provided; retrieving all records.")
    }
    if (!is.null(key_type)) {
      warning("No keys provided. Ignoring key_type and retrieving all records.")
    }
    queries <- paste0(base_url,
                      current_table)
  } else {
    # If there are keys, chunk them then build queries
    # This helps prevent queries so long that they fail
    if (verbose) {
      message("Grouping keys into chunks for queries.")
    }
    # We don't know whether the keys came in as a vector of single keys or if
    # one or more of the character strings contains keys separated by commas
    # so we're going to handle that an get a vector of single-key strings
    keys_vector <- unlist(lapply(X = keys,
                                 FUN = function(X) {
                                   trimws(unlist(stringr::str_split(string = X,
                                                                    pattern = ",")))
                                 }))
    
    # Figure out how many chunks to break these into based on the max number of
    # keys in a chunk
    key_chunk_count <- ceiling(length(keys_vector) / key_chunk_size)
    
    # Make the key chunks
    # For each chunk, figure out the appropriate indices and paste together the
    # relevant key values into strings that we can use to build per-chunk queries
    keys_chunks <- sapply(X = 1:key_chunk_count,
                          keys_vector = keys_vector,
                          key_chunk_size = key_chunk_size,
                          key_count = length(keys_vector),
                          FUN = function(X, keys_vector, key_chunk_size, key_count) {
                            min_index <- max(c(1, (X - 1) * key_chunk_size + 1))
                            max_index <- min(c(key_count, X * key_chunk_size))
                            indices <- min_index:max_index
                            paste(keys_vector[indices],
                                  collapse = ",")
                          })
    
    if (verbose) {
      if (length(keys_chunks == 1)) {
        message("Building query.")
      } else {
        message("Building queries.")
      }
    }
    
    queries <- paste0(base_url,
                      current_table,
                      "?",
                      key_type,
                      "=",
                      keys_chunks)
  }
  
  # Use the queries to snag data
  # This produces a list of results where each index in the list contains the
  # results of one query
  data_list <- lapply(X = queries,
                      timeout = timeout,
                      user_agent = user_agent,
                      FUN = function(X, timeout, user_agent){
                        if (verbose) {
                          message("Attempting to query LDC with:")
                          message(X)
                        }
                        
                        # Full query response
                        response <- httr::GET(X,
                                              config = list(httr::timeout(timeout),
                                                            httr::user_agent(user_agent)))
                        
                        # What if there's an error????
                        if (httr::http_error(response)) {
                          stop(paste0("Query failed with status ",
                                      response$status_code))
                        }
                        
                        # Grab only the data portion
                        response_content <- response[["content"]]
                        # Convert from raw to character
                        content_character <- rawToChar(response_content)
                        # Convert from character to data frame
                        content_df <- jsonlite::fromJSON(content_character)
                        
                        content_df
                      })
  
  # Combine all the results of the queries
  data <- do.call(rbind,
                  data_list)
  
  # If there aren't data, let the user know
  if (length(data) < 1) {
    warning("No data retrieved. Confirm that your keys and key_type are correct.")
    return(NULL)
  } else {
    # If there are data and the user gave keys, find which if any are missing
    if (!is.null(keys)) {
      missing_keys <- keys_vector[!(keys_vector %in% data[[key_type]])]
      if (length(missing_keys) > 0) {
        warning(paste0("The following keys were not associated with data: ",
                       paste(missing_keys,
                             collapse = ",")))
      }
    }
    return(data)
  }
}


#' #' Query the Landscape Data Commons
#' #' @description Fetch data from the Landscape Data commons, querying by ecological site ID, PrimaryKey, or ProjectKey.
#' #' @param keys Character vector. The character strings containing the values to match in the query/queries. May also be a single character string of key values separated by commas, e.g. \code{"R036XB006NM,R042XB012NM"}.
#' #' @param key_type Character string. The type of key values being used. This determines which variable in the Landscape Data Commons to filter using the values \code{keys}. Must be a valid variable name in the queried data table. Common key types include \code{"EcologicalSiteID"}, \code{"PrimaryKey"}, and \code{"ProjectKey"}. For details, the API documentation is available here: https://api.landscapedatacommons.org/api-docs
#' #' @param data_type Character string. Determines which table to download data from. Valid values are \code{"lpi"}, \code{"height"}, \code{"gap"}, \code{"soilstability"}, \code{"species"}, \code{"speciesinventory"}, \code{"indicators"}, \code{"horizontalflux"}, \code{"dustdeposition"}, and \code{"header"}.
#' #' @param verbose Logical. If \code{TRUE} then the function will report with diagnostic messages as it runs. Defaults to \code{FALSE}.
#' #' @export
#' #' 
#' fetch_ldc <- function(keys,
#'                       key_type,
#'                       data_type,
#'                       verbose = FALSE){
#'   # Hardcoding the available variables in each of the queryable tables
#'   # See: https://api.landscapedatacommons.org/api-docs
#'   table_vars <- list("lpi" = c("rid",
#'                                "PrimaryKey",
#'                                "DBKey",
#'                                "ProjectKey",
#'                                "LineKey",
#'                                "RecKey",
#'                                "layer",
#'                                "code",
#'                                "chckbox",
#'                                "ShrubShape",
#'                                "FormType",
#'                                "FormDate",
#'                                "Direction",
#'                                "Measure",
#'                                "LineLengthAmount",
#'                                "SpacingIntervalAmount",
#'                                "SpacingType",
#'                                "ShowCheckbox",
#'                                "CheckboxLabel",
#'                                "PointLoc",
#'                                "PointNbr",
#'                                "source",
#'                                "DateLoadedInDb"),
#'                      "height" = c("rid",
#'                                   "PrimaryKey",
#'                                   "DBKey",
#'                                   "ProjectKey",
#'                                   "PointLoc",
#'                                   "PointNbr",
#'                                   "RecKey",
#'                                   "Height",
#'                                   "Species",
#'                                   "Chkbox",
#'                                   "type",
#'                                   "GrowthHabit_measured",
#'                                   "LineKey",
#'                                   "DateModified",
#'                                   "FormType",
#'                                   "FormDate",
#'                                   "source",
#'                                   "Direction",
#'                                   "Measure",
#'                                   "LineLengthAmount",
#'                                   "SpacingIntervalAmount",
#'                                   "SpacingType",
#'                                   "HeightOption",
#'                                   "HeightUOM",
#'                                   "ShowCheckbox",
#'                                   "CheckboxLabel",
#'                                   "DateLoadedInDb"),
#'                      "gap" = c("rid",
#'                                "PrimaryKey",
#'                                "DBKey",
#'                                "ProjectKey",
#'                                "RecType",
#'                                "SeqNo",
#'                                "GapStart",
#'                                "GapEnd",
#'                                "Gap",
#'                                "LineKey",
#'                                "RecKey",
#'                                "FormDate",
#'                                "DateModified",
#'                                "FormType",
#'                                "Direction",
#'                                "Measure",
#'                                "LineLengthAmount",
#'                                "GapMin",
#'                                "GapData",
#'                                "PerennialsCanopy",
#'                                "AnnualGrassesCanopy",
#'                                "AnnualForbsCanopy",
#'                                "OtherCanopy",
#'                                "Notes",
#'                                "NoCanopyGaps",
#'                                "NoBasalGaps",
#'                                "DateLoadedInDb",
#'                                "PerennialsBasal",
#'                                "AnnualGrassesBasal",
#'                                "AnnualForbsBasal",
#'                                "OtherBasal",
#'                                "source"),
#'                      "soilstability" = c("rid",
#'                                          "PrimaryKey",
#'                                          "DBKey",
#'                                          "ProjectKey",
#'                                          "RecKey",
#'                                          "FormType",
#'                                          "FormDate",
#'                                          "LineKey",
#'                                          "SoilStabSubSurface",
#'                                          "Line",
#'                                          "Position",
#'                                          "Pos",
#'                                          "Veg",
#'                                          "Rating",
#'                                          "Hydro",
#'                                          "Notes",
#'                                          "source",
#'                                          "DateLoadedInDb"),
#'                      "species" = c("rid",
#'                                    "PrimaryKey",
#'                                    "DBKey",
#'                                    "ProjectKey",
#'                                    "Species",
#'                                    "AH_SpeciesCover",
#'                                    "AH_SpeciesCover_n",
#'                                    "Hgt_Species_Avg",
#'                                    "Hgt_Species_Avg_n",
#'                                    "Duration",
#'                                    "GrowthHabit",
#'                                    "GrowthHabitSub",
#'                                    "SpeciesKey",
#'                                    "DateLoadedInDb"),
#'                      "speciesinventory" = c("rid",
#'                                             "PrimaryKey",
#'                                             "DBKey",
#'                                             "ProjectKey",
#'                                             "Species",
#'                                             "DENSITY",
#'                                             "LineKey",
#'                                             "RecKey",
#'                                             "FormType",
#'                                             "FormDate",
#'                                             "SpecRichMethod",
#'                                             "SpecRichMeasure",
#'                                             "SpecRichNbRSubPlots",
#'                                             "SpecRich1Container",
#'                                             "SpecRich1Shape",
#'                                             "SpecRich1Dim1",
#'                                             "SpecRich1Dim2",
#'                                             "SpecRich1Area",
#'                                             "SpecRich2Container",
#'                                             "SpecRich2Shape",
#'                                             "SpecRich2Dim1",
#'                                             "SpecRich2Dim2",
#'                                             "SpecRich2Area",
#'                                             "SpecRich3Container",
#'                                             "SpecRich3Shape",
#'                                             "SpecRich3Dim1",
#'                                             "SpecRich3Dim2",
#'                                             "SpecRich3Area",
#'                                             "SpecRich4Container",
#'                                             "SpecRich4Shape",
#'                                             "SpecRich4Dim1",
#'                                             "SpecRich4Dim2",
#'                                             "SpecRich4Area",
#'                                             "SpecRich5Container",
#'                                             "SpecRich5Shape",
#'                                             "SpecRich5Dim1",
#'                                             "SpecRich5Dim2",
#'                                             "SpecRich5Area",
#'                                             "SpecRich6Container",
#'                                             "SpecRich6Shape",
#'                                             "SpecRich6Dim1",
#'                                             "SpecRich6Dim2",
#'                                             "SpecRich6Area",
#'                                             "Notes",
#'                                             "source",
#'                                             "DateLoadedInDb"),
#'                      "header" = c("rid",
#'                                   "PrimaryKey",
#'                                   "DBKey",
#'                                   "ProjectKey",
#'                                   "DateVisited",
#'                                   "Latitude_NAD83",
#'                                   "Longitude_NAD83",
#'                                   "LocationType",
#'                                   "EcologicalSiteID",
#'                                   "PercentCoveredByEcosite",
#'                                   "SpeciesKey",
#'                                   "PlotID",
#'                                   "DateLoadedInDb",
#'                                   "wkb_geometry",
#'                                   "source"),
#'                      "indicators" = c("rid",
#'                                       "PrimaryKey",
#'                                       "DBKey",
#'                                       "ProjectKey",
#'                                       "DateVisited",
#'                                       "EcologicalSiteId",
#'                                       "PercentCoveredByEcoSite",
#'                                       "Latitude_NAD83",
#'                                       "Longitude_NAD83",
#'                                       "LocationStatus",
#'                                       "LocationType",
#'                                       "Latitude_NAD83_Actual",
#'                                       "Longitude_NAD83_Actual",
#'                                       "BareSoilCover",
#'                                       "TotalFoliarCover",
#'                                       "AH_AnnGrassCover",
#'                                       "AH_ForbCover",
#'                                       "AH_GrassCover",
#'                                       "AH_PerenForbCover",
#'                                       "AH_PerenForbGrassCover",
#'                                       "AH_PerenGrassCover",
#'                                       "AH_ShrubCover",
#'                                       "FH_CyanobacteriaCover",
#'                                       "FH_DepSoilCover",
#'                                       "FH_DuffCover",
#'                                       "FH_EmbLitterCover",
#'                                       "FH_HerbLitterCover",
#'                                       "FH_LichenCover",
#'                                       "FH_MossCover",
#'                                       "FH_RockCover",
#'                                       "FH_TotalLitterCover",
#'                                       "FH_VagrLichenCover",
#'                                       "FH_WaterCover",
#'                                       "FH_WoodyLitterCover",
#'                                       "GapCover_101_200",
#'                                       "GapCover_200_plus",
#'                                       "GapCover_25_50",
#'                                       "GapCover_25_plus",
#'                                       "GapCover_51_100",
#'                                       "Hgt_Forb_Avg",
#'                                       "Hgt_Grass_Avg",
#'                                       "Hgt_Herbaceous_Avg",
#'                                       "Hgt_PerenForb_Avg",
#'                                       "Hgt_PerenForbGrass_Avg",
#'                                       "Hgt_PerenGrass_Avg",
#'                                       "Hgt_Woody_Avg",
#'                                       "RH_AnnualProd",
#'                                       "RH_BareGround",
#'                                       "RH_BioticIntegrity",
#'                                       "RH_CommentsBI",
#'                                       "RH_CommentsHF",
#'                                       "RH_CommentsSS",
#'                                       "RH_Compaction",
#'                                       "RH_DeadDyingPlantParts",
#'                                       "RH_FuncSructGroup",
#'                                       "RH_Gullies",
#'                                       "RH_HydrologicFunction",
#'                                       "RH_InvasivePlants",
#'                                       "RH_LitterAmount",
#'                                       "RH_LitterMovement",
#'                                       "RH_PedestalsTerracettes",
#'                                       "RH_PlantCommunityComp",
#'                                       "RH_ReprodCapabilityPeren",
#'                                       "RH_Rills",
#'                                       "RH_SoilSiteStability",
#'                                       "RH_SoilSurfLossDeg",
#'                                       "RH_SoilSurfResisErosion",
#'                                       "RH_WaterFlowPatterns",
#'                                       "RH_WindScouredAreas",
#'                                       "SoilStability_All",
#'                                       "SoilStability_Protected",
#'                                       "SoilStability_Unprotected",
#'                                       "DateLoadedInDb",
#'                                       "mlra_name",
#'                                       "mlrarsym",
#'                                       "na_l1name",
#'                                       "na_l2name",
#'                                       "us_l3name",
#'                                       "us_l4name",
#'                                       "State",
#'                                       "wkb_geometry",
#'                                       "modis_landcover"),
#'                      "dustdeposition" = c("rid",
#'                                           "StackID",
#'                                           "ProjectKey",
#'                                           "DateEstablished",
#'                                           "Location",
#'                                           "Notes",
#'                                           "ItemType",
#'                                           "trapOpeningArea",
#'                                           "GPSCoordSys",
#'                                           "Datum",
#'                                           "Zone",
#'                                           "Easting",
#'                                           "Northing",
#'                                           "Longitude",
#'                                           "Latitude",
#'                                           "RecKey",
#'                                           "collectDate",
#'                                           "breakerNbr",
#'                                           "emptyWeight",
#'                                           "recordedWeight",
#'                                           "sedimentWeight",
#'                                           "daysExposed",
#'                                           "sedimentGprDay",
#'                                           "sedimentArchived",
#'                                           "sedimentGperDayByInlet",
#'                                           "SeqNo",
#'                                           "SampleCompromised",
#'                                           "PrimaryKey",
#'                                           "DateLoadedInDb",
#'                                           "DBKey"),
#'                      "horizontalflux" = c("rid",
#'                                           "BoxID",
#'                                           "StackID",
#'                                           "Height",
#'                                           "DateEstablished",
#'                                           "Description",
#'                                           "openingSize",
#'                                           "processMethod",
#'                                           "ovenTemp",
#'                                           "BoxType",
#'                                           "azimuth",
#'                                           "SamplerType",
#'                                           "InletArea",
#'                                           "ProjectKey",
#'                                           "Location",
#'                                           "ItemType",
#'                                           "trapOpeningArea",
#'                                           "GPSCoordSys",
#'                                           "Datum",
#'                                           "Zone",
#'                                           "Easting",
#'                                           "Northing",
#'                                           "Longitude",
#'                                           "Latitude",
#'                                           "RecKey",
#'                                           "collectDate",
#'                                           "beakerNbr",
#'                                           "emptyWeight",
#'                                           "recordedWeight",
#'                                           "sedimentWeight",
#'                                           "daysExposed",
#'                                           "sedimentGperDay",
#'                                           "sedimentArchived",
#'                                           "Notes",
#'                                           "sedimentGperDayByInlet",
#'                                           "SeqNo",
#'                                           "SampleCompromised",
#'                                           "PrimaryKey",
#'                                           "DateLoadedInDb",
#'                                           "DBKey"))
#'   
#'   
#'   if (!(data_type %in% names(table_vars))) {
#'     stop(paste0("data_type must be one of: ",
#'                 paste(names(table_vars),
#'                       collapse = ", ")))
#'   }
#'   
#'   current_data_source <- switch(data_type,
#'                                 "lpi" = {"datalpi"},
#'                                 "height" = {"dataheight"},
#'                                 "gap" = {"datagap"},
#'                                 "soilstability" = {"datasoilstability"},
#'                                 "species" = {"geospecies"},
#'                                 "speciesinventory" = {"dataspeciesinventory"},
#'                                 "header" = {"dataheader"},
#'                                 "indicators" = {"geoindicators"},
#'                                 "dustdeposition" = {"datadustdeposition"},
#'                                 "horizontalflux" = {"datahorizontalflux"})
#'   
#'   
#'   available_vars <- table_vars[[data_type]]
#'   
#'   if (!is.null(key_type)) {
#'     if (!(key_type %in% available_vars)) {
#'       stop(paste0("key_type must be one of: ",
#'                   paste(available_vars,
#'                         collapse = ", ")))
#'     }
#'   }
#'   
#'   
#'   if (is.null(keys) & !is.null(key_type)) {
#'     warning("No keys provided. Ignoring key_type.")
#'   } else if (!is.null(keys) & is.null(key_type)) {
#'     warning("key_type is NULL. Ignoring keys.")
#'   }
#'   
#'   if (is.null(keys) | is.null(key_type)) {
#'     queries <- paste0("https://api.landscapedatacommons.org/api/v1/",
#'                       current_data_source)
#'   } else {
#'     queries <- paste0("https://api.landscapedatacommons.org/api/v1/",
#'                       current_data_source, "?",
#'                       key_type, "=",
#'                       keys)
#'   }
#'   
#'   query_results_list <- lapply(X = queries,
#'                                FUN = function(X){
#'                                  # Build the query
#'                                  query <- X
#'                                  
#'                                  # Getting the data via curl
#'                                  # connection <- curl::curl(query)
#'                                  # results_raw <- readLines(connection)
#'                                  # results <- jsonlite::fromJSON(results_raw)
#'                                  if (verbose) {
#'                                    message("Attempting to query EDIT with:")
#'                                    message(query)
#'                                  }
#'                                  
#'                                  # Full query results
#'                                  response <- httr::GET(query,
#'                                                        config = httr::timeout(60))
#'                                  # What if there's an error????
#'                                  if (httr::http_error(response)) {
#'                                    stop(paste0("Query failed with status ",
#'                                                response$status_code))
#'                                  }
#'                                  # Grab only the data portion
#'                                  response_content <- response[["content"]]
#'                                  # Convert from raw to character
#'                                  response_content_character <- rawToChar(response_content)
#'                                  # Convert from character to data frame
#'                                  results <- jsonlite::fromJSON(response_content_character)
#'                                  if (verbose) {
#'                                    message("Results converted from json to character")
#'                                  }
#'                                  
#'                                  results
#'                                })
#'   
#'   results <- do.call(rbind,
#'                      query_results_list)
#'   
#'   # So we can tell the user later which actually got queried
#'   if (is.null(results)) {
#'     warning("No results returned. There are no records in the LDC for the supplied keys.")
#'     return(results)
#'   } else {
#'     if (verbose) {
#'       message("Determining if keys are missing.")
#'     }
#'     if (is.null(keys)) {
#'       missing_keys <- NULL
#'     } else {
#'       # So if they submitted multiple keys as a a single character string with separating commas, handle that
#'       keys <- unlist(sapply(X = keys,
#'                             FUN = function(X){
#'                               if (grepl(x = X, pattern = ",")) {
#'                                 unlist(stringr::str_split(string = X,
#'                                                           pattern = ","))
#'                               } else {
#'                                 X
#'                               }
#'                             }))
#'       
#'       queried_keys <- unique(results[[key_type]])
#'       missing_keys <- keys[!(keys %in% queried_keys)]
#'     }
#'   }
#'   if (verbose) {
#'     message(paste0("length(missing_keys) is: ",
#'                    paste(length(missing_keys),
#'                          collapse = ", ")))
#'   }
#'   
#'   if (length(missing_keys) > 0) {
#'     missing_key_warning <- paste0("The following keys did not return data from the LDC: ",
#'                                   paste(missing_keys,
#'                                         collapse = ", "))
#'     warning(missing_key_warning)
#'   }
#'   
#'   if (verbose) {
#'     message(paste0("length(results) is: ",
#'                    paste(length(results),
#'                          collapse = ", ")))
#'   }
#'   
#'   # Only keep going if there are results!!!!
#'   if (length(results) > 0) {
#'     if (verbose) {
#'       message("Coercing data to numeric.")
#'     }
#'     # Convert from character to numeric variables where possible
#'     data_corrected <- lapply(X = names(results),
#'                              data = results,
#'                              FUN = function(X, data){
#'                                # Get the current variable values as a vector
#'                                vector <- data[[X]]
#'                                # Try to coerce into numeric
#'                                numeric_vector <- suppressWarnings(as.numeric(vector))
#'                                # If that works without introducing NAs, return the numeric vector
#'                                # Otherwise, return the original character vector
#'                                if (all(!is.na(numeric_vector))) {
#'                                  return(numeric_vector)
#'                                } else {
#'                                  return(vector)
#'                                }
#'                              })
#'     
#'     # From some reason co.call(cbind, data_corrected) was returning a list not a data frame
#'     # so I'm resorting to using dplyr
#'     data <- suppressMessages(dplyr::bind_cols(data_corrected))
#'     # Correct the names of the variables
#'     names(data) <- names(results)
#'     
#'     # Put it in the workspace list
#'     if (verbose) {
#'       message("Returning coerced data")
#'     }
#'     return(data)
#'   } else {
#'     if (verbose) {
#'       message("Returning empty results")
#'     }
#'     return(results)
#'   }
#' }