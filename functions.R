#' Query the Landscape Data Commons
#' @description Fetch data from the Landscape Data commons, querying by ecological site ID, PrimaryKey, or ProjectKey.
#' @param keys Character vector. The character strings containing the values to match in the query/queries. May also be a single character string of key values separated by commas, e.g. \code{"R036XB006NM,R042XB012NM"}.
#' @param key_type Character string. The type of key values being used. This determines which variable in the Landscape Data Commons to filter using the values \code{keys}. Must be a valid variable name in the queried data table. Common key types include \code{"EcologicalSiteID"}, \code{"PrimaryKey"}, and \code{"ProjectKey"}. For details, the API documentation is available here: https://api.landscapedatacommons.org/api-docs
#' @param data_type Character string. Determines which table to download data from. Valid values are \code{"lpi"}, \code{"height"}, \code{"gap"}, \code{"soilstability"}, \code{"species"}, \code{"speciesinventory"}, \code{"indicators"}, \code{"horizontalflux"}, \code{"dustdeposition"}, and \code{"header"}.
#' @param verbose Logical. If \code{TRUE} then the function will report with diagnostic messages as it runs. Defaults to \code{FALSE}.
#' @export
fetch_ldc <- function(keys,
                      key_type,
                      data_type,
                      verbose = FALSE){
  # Hardcoding the available variables in each of the queryable tables
  # See: https://api.landscapedatacommons.org/api-docs
  table_vars <- list("lpi" = c("rid",
                               "PrimaryKey",
                               "DBKey",
                               "ProjectKey",
                               "LineKey",
                               "RecKey",
                               "layer",
                               "code",
                               "chckbox",
                               "ShrubShape",
                               "FormType",
                               "FormDate",
                               "Direction",
                               "Measure",
                               "LineLengthAmount",
                               "SpacingIntervalAmount",
                               "SpacingType",
                               "ShowCheckbox",
                               "CheckboxLabel",
                               "PointLoc",
                               "PointNbr",
                               "source",
                               "DateLoadedInDb"),
                     "height" = c("rid",
                                  "PrimaryKey",
                                  "DBKey",
                                  "ProjectKey",
                                  "PointLoc",
                                  "PointNbr",
                                  "RecKey",
                                  "Height",
                                  "Species",
                                  "Chkbox",
                                  "type",
                                  "GrowthHabit_measured",
                                  "LineKey",
                                  "DateModified",
                                  "FormType",
                                  "FormDate",
                                  "source",
                                  "Direction",
                                  "Measure",
                                  "LineLengthAmount",
                                  "SpacingIntervalAmount",
                                  "SpacingType",
                                  "HeightOption",
                                  "HeightUOM",
                                  "ShowCheckbox",
                                  "CheckboxLabel",
                                  "DateLoadedInDb"),
                     "gap" = c("rid",
                               "PrimaryKey",
                               "DBKey",
                               "ProjectKey",
                               "RecType",
                               "SeqNo",
                               "GapStart",
                               "GapEnd",
                               "Gap",
                               "LineKey",
                               "RecKey",
                               "FormDate",
                               "DateModified",
                               "FormType",
                               "Direction",
                               "Measure",
                               "LineLengthAmount",
                               "GapMin",
                               "GapData",
                               "PerennialsCanopy",
                               "AnnualGrassesCanopy",
                               "AnnualForbsCanopy",
                               "OtherCanopy",
                               "Notes",
                               "NoCanopyGaps",
                               "NoBasalGaps",
                               "DateLoadedInDb",
                               "PerennialsBasal",
                               "AnnualGrassesBasal",
                               "AnnualForbsBasal",
                               "OtherBasal",
                               "source"),
                     "soilstability" = c("rid",
                                         "PrimaryKey",
                                         "DBKey",
                                         "ProjectKey",
                                         "RecKey",
                                         "FormType",
                                         "FormDate",
                                         "LineKey",
                                         "SoilStabSubSurface",
                                         "Line",
                                         "Position",
                                         "Pos",
                                         "Veg",
                                         "Rating",
                                         "Hydro",
                                         "Notes",
                                         "source",
                                         "DateLoadedInDb"),
                     "species" = c("rid",
                                   "PrimaryKey",
                                   "DBKey",
                                   "ProjectKey",
                                   "Species",
                                   "AH_SpeciesCover",
                                   "AH_SpeciesCover_n",
                                   "Hgt_Species_Avg",
                                   "Hgt_Species_Avg_n",
                                   "Duration",
                                   "GrowthHabit",
                                   "GrowthHabitSub",
                                   "SpeciesKey",
                                   "DateLoadedInDb"),
                     "speciesinventory" = c("rid",
                                            "PrimaryKey",
                                            "DBKey",
                                            "ProjectKey",
                                            "Species",
                                            "DENSITY",
                                            "LineKey",
                                            "RecKey",
                                            "FormType",
                                            "FormDate",
                                            "SpecRichMethod",
                                            "SpecRichMeasure",
                                            "SpecRichNbRSubPlots",
                                            "SpecRich1Container",
                                            "SpecRich1Shape",
                                            "SpecRich1Dim1",
                                            "SpecRich1Dim2",
                                            "SpecRich1Area",
                                            "SpecRich2Container",
                                            "SpecRich2Shape",
                                            "SpecRich2Dim1",
                                            "SpecRich2Dim2",
                                            "SpecRich2Area",
                                            "SpecRich3Container",
                                            "SpecRich3Shape",
                                            "SpecRich3Dim1",
                                            "SpecRich3Dim2",
                                            "SpecRich3Area",
                                            "SpecRich4Container",
                                            "SpecRich4Shape",
                                            "SpecRich4Dim1",
                                            "SpecRich4Dim2",
                                            "SpecRich4Area",
                                            "SpecRich5Container",
                                            "SpecRich5Shape",
                                            "SpecRich5Dim1",
                                            "SpecRich5Dim2",
                                            "SpecRich5Area",
                                            "SpecRich6Container",
                                            "SpecRich6Shape",
                                            "SpecRich6Dim1",
                                            "SpecRich6Dim2",
                                            "SpecRich6Area",
                                            "Notes",
                                            "source",
                                            "DateLoadedInDb"),
                     "header" = c("rid",
                                  "PrimaryKey",
                                  "DBKey",
                                  "ProjectKey",
                                  "DateVisited",
                                  "Latitude_NAD83",
                                  "Longitude_NAD83",
                                  "LocationType",
                                  "EcologicalSiteID",
                                  "PercentCoveredByEcosite",
                                  "SpeciesKey",
                                  "PlotID",
                                  "DateLoadedInDb",
                                  "wkb_geometry",
                                  "source"),
                     "indicators" = c("rid",
                                      "PrimaryKey",
                                      "DBKey",
                                      "ProjectKey",
                                      "DateVisited",
                                      "EcologicalSiteId",
                                      "PercentCoveredByEcoSite",
                                      "Latitude_NAD83",
                                      "Longitude_NAD83",
                                      "LocationStatus",
                                      "LocationType",
                                      "Latitude_NAD83_Actual",
                                      "Longitude_NAD83_Actual",
                                      "BareSoilCover",
                                      "TotalFoliarCover",
                                      "AH_AnnGrassCover",
                                      "AH_ForbCover",
                                      "AH_GrassCover",
                                      "AH_PerenForbCover",
                                      "AH_PerenForbGrassCover",
                                      "AH_PerenGrassCover",
                                      "AH_ShrubCover",
                                      "FH_CyanobacteriaCover",
                                      "FH_DepSoilCover",
                                      "FH_DuffCover",
                                      "FH_EmbLitterCover",
                                      "FH_HerbLitterCover",
                                      "FH_LichenCover",
                                      "FH_MossCover",
                                      "FH_RockCover",
                                      "FH_TotalLitterCover",
                                      "FH_VagrLichenCover",
                                      "FH_WaterCover",
                                      "FH_WoodyLitterCover",
                                      "GapCover_101_200",
                                      "GapCover_200_plus",
                                      "GapCover_25_50",
                                      "GapCover_25_plus",
                                      "GapCover_51_100",
                                      "Hgt_Forb_Avg",
                                      "Hgt_Grass_Avg",
                                      "Hgt_Herbaceous_Avg",
                                      "Hgt_PerenForb_Avg",
                                      "Hgt_PerenForbGrass_Avg",
                                      "Hgt_PerenGrass_Avg",
                                      "Hgt_Woody_Avg",
                                      "RH_AnnualProd",
                                      "RH_BareGround",
                                      "RH_BioticIntegrity",
                                      "RH_CommentsBI",
                                      "RH_CommentsHF",
                                      "RH_CommentsSS",
                                      "RH_Compaction",
                                      "RH_DeadDyingPlantParts",
                                      "RH_FuncSructGroup",
                                      "RH_Gullies",
                                      "RH_HydrologicFunction",
                                      "RH_InvasivePlants",
                                      "RH_LitterAmount",
                                      "RH_LitterMovement",
                                      "RH_PedestalsTerracettes",
                                      "RH_PlantCommunityComp",
                                      "RH_ReprodCapabilityPeren",
                                      "RH_Rills",
                                      "RH_SoilSiteStability",
                                      "RH_SoilSurfLossDeg",
                                      "RH_SoilSurfResisErosion",
                                      "RH_WaterFlowPatterns",
                                      "RH_WindScouredAreas",
                                      "SoilStability_All",
                                      "SoilStability_Protected",
                                      "SoilStability_Unprotected",
                                      "DateLoadedInDb",
                                      "mlra_name",
                                      "mlrarsym",
                                      "na_l1name",
                                      "na_l2name",
                                      "us_l3name",
                                      "us_l4name",
                                      "State",
                                      "wkb_geometry",
                                      "modis_landcover"),
                     "dustdeposition" = c("rid",
                                          "StackID",
                                          "ProjectKey",
                                          "DateEstablished",
                                          "Location",
                                          "Notes",
                                          "ItemType",
                                          "trapOpeningArea",
                                          "GPSCoordSys",
                                          "Datum",
                                          "Zone",
                                          "Easting",
                                          "Northing",
                                          "Longitude",
                                          "Latitude",
                                          "RecKey",
                                          "collectDate",
                                          "breakerNbr",
                                          "emptyWeight",
                                          "recordedWeight",
                                          "sedimentWeight",
                                          "daysExposed",
                                          "sedimentGprDay",
                                          "sedimentArchived",
                                          "sedimentGperDayByInlet",
                                          "SeqNo",
                                          "SampleCompromised",
                                          "PrimaryKey",
                                          "DateLoadedInDb",
                                          "DBKey"),
                     "horizontalflux" = c("rid",
                                          "BoxID",
                                          "StackID",
                                          "Height",
                                          "DateEstablished",
                                          "Description",
                                          "openingSize",
                                          "processMethod",
                                          "ovenTemp",
                                          "BoxType",
                                          "azimuth",
                                          "SamplerType",
                                          "InletArea",
                                          "ProjectKey",
                                          "Location",
                                          "ItemType",
                                          "trapOpeningArea",
                                          "GPSCoordSys",
                                          "Datum",
                                          "Zone",
                                          "Easting",
                                          "Northing",
                                          "Longitude",
                                          "Latitude",
                                          "RecKey",
                                          "collectDate",
                                          "beakerNbr",
                                          "emptyWeight",
                                          "recordedWeight",
                                          "sedimentWeight",
                                          "daysExposed",
                                          "sedimentGperDay",
                                          "sedimentArchived",
                                          "Notes",
                                          "sedimentGperDayByInlet",
                                          "SeqNo",
                                          "SampleCompromised",
                                          "PrimaryKey",
                                          "DateLoadedInDb",
                                          "DBKey"))
  
  
  if (!(data_type %in% names(table_vars))) {
    stop(paste0("data_type must be one of: ",
                paste(names(table_vars),
                      collapse = ", ")))
  }
  
  current_data_source <- switch(data_type,
                                "lpi" = {"datalpi"},
                                "height" = {"dataheight"},
                                "gap" = {"datagap"},
                                "soilstability" = {"datasoilstability"},
                                "species" = {"geospecies"},
                                "speciesinventory" = {"dataspeciesinventory"},
                                "header" = {"dataheader"},
                                "indicators" = {"geoindicators"},
                                "dustdeposition" = {"datadustdeposition"},
                                "horizontalflux" = {"datahorizontalflux"})
  
  available_vars <- table_vars[[data_type]]
  
  if (!(key_type %in% available_vars)) {
    stop(paste0("key_type must be one of: ",
                paste(available_vars,
                      collapse = ", ")))
  }
  
  query_results_list <- lapply(X = keys,
                               current_data_source = current_data_source,
                               current_key_type = key_type,
                               FUN = function(X, current_data_source, current_key_type){
                                 # Build the query
                                 query <- paste0("https://api.landscapedatacommons.org/api/v1/",
                                                 current_data_source, "?",
                                                 current_key_type, "=",
                                                 X)
                                 
                                 # Getting the data via curl
                                 # connection <- curl::curl(query)
                                 # results_raw <- readLines(connection)
                                 # results <- jsonlite::fromJSON(results_raw)
                                 if (verbose) {
                                   message("Attempting to query EDIT with:")
                                   message(query)
                                 }
                                 
                                 # Full query results
                                 response <- httr::GET(query,
                                                       config = httr::timeout(60))
                                 # What if there's an error????
                                 if (httr::http_error(response)) {
                                   stop(paste0("Query failed with status ",
                                               response$status_code))
                                 }
                                 # Grab only the data portion
                                 response_content <- response[["content"]]
                                 # Convert from raw to character
                                 response_content_character <- rawToChar(response_content)
                                 # Convert from character to data frame
                                 results <- jsonlite::fromJSON(response_content_character)
                                 if (verbose) {
                                   message("Results converted from json to character")
                                 }
                                 
                                 results
                               })
  
  results <- do.call(rbind,
                     query_results_list)
  
  # So we can tell the user later which actually got queried
  if (is.null(results)) {
    warning("No results returned. There are no records in the LDC for the supplied keys.")
    return(results)
  } else {
    if (verbose) {
      message("Determining if keys are missing.")
    }
    
    # So if they submitted multiple keys as a a single character string with separating commas, handle that
    keys <- unlist(sapply(X = keys,
                          FUN = function(X){
                            if (grepl(x = X, pattern = ",")) {
                              unlist(stringr::str_split(string = X,
                                                        pattern = ","))
                            } else {
                              X
                            }
                          }))
    
    queried_keys <- unique(results[[key_type]])
    missing_keys <- keys[!(keys %in% queried_keys)]
  }
  if (verbose) {
    message(paste0("length(missing_keys) is: ",
                   paste(length(missing_keys),
                         collapse = ", ")))
  }
  
  if (length(missing_keys) > 0) {
    missing_key_warning <- paste0("The following keys did not return data from the LDC: ",
                                  paste(missing_keys,
                                        collapse = ", "))
    warning(missing_key_warning)
  }
  
  if (verbose) {
    message(paste0("length(results) is: ",
                   paste(length(results),
                         collapse = ", ")))
  }
  
  # Only keep going if there are results!!!!
  if (length(results) > 0) {
    if (verbose) {
      message("Coercing data to numeric.")
    }
    # Convert from character to numeric variables where possible
    data_corrected <- lapply(X = names(results),
                             data = results,
                             FUN = function(X, data){
                               # Get the current variable values as a vector
                               vector <- data[[X]]
                               # Try to coerce into numeric
                               numeric_vector <- suppressWarnings(as.numeric(vector))
                               # If that works without introducing NAs, return the numeric vector
                               # Otherwise, return the original character vector
                               if (all(!is.na(numeric_vector))) {
                                 return(numeric_vector)
                               } else {
                                 return(vector)
                               }
                             })
    
    # From some reason co.call(cbind, data_corrected) was returning a list not a data frame
    # so I'm resorting to using dplyr
    data <- suppressMessages(dplyr::bind_cols(data_corrected))
    # Correct the names of the variables
    names(data) <- names(results)
    
    # Put it in the workspace list
    if (verbose) {
      message("Returning coerced data")
    }
    return(data)
  } else {
    if (verbose) {
      message("Returning empty results")
    }
    return(results)
  }
}
