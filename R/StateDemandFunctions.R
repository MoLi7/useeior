# Load BEA schema_info based on model BEA
SchemaInfo <- utils::read.table(system.file("extdata", "2012_Summary_Schema_Info.csv", package = "useeior"),
                                sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
# Extract desired columns from SchemaInfo, return vectors with strings of codes
getVectorOfCodes <- function(colName) {
  return(as.vector(stats::na.omit(SchemaInfo[, c("Code", colName)])[, "Code"]))
}
Commodities <- getVectorOfCodes("Commodity")
Industries <- getVectorOfCodes("Industry")
HouseholdDemandCodes <- getVectorOfCodes("HouseholdDemand")
InvestmentDemandCodes <- getVectorOfCodes("InvestmentDemand")
ChangeInventories <- getVectorOfCodes("ChangeInventories")
ImportCodes <- getVectorOfCodes("Import")
ExportCodes <- getVectorOfCodes("Export")
GovernmentDemandCodes <- getVectorOfCodes("GovernmentDemand")

#' Get industry-level Compensation for all states at a specific year.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state Compensation for all states at a specific year.
getStateEmpCompensation <- function(year) {
  # Load pre-saved state Compensation 2007-2017
  StateEmpCompensation <- useeior::State_Compensation_2007_2017
  StateEmpCompensation <- StateEmpCompensation[, c("GeoName", "LineCode", as.character(year))]
  return(StateEmpCompensation)
}

#' Get industry-level Tax for all states at a specific year.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state Tax for all states at a specific year.
getStateTax <- function(year) {
  # Load pre-saved state Tax 2007-2017
  StateTax <- useeior::State_Tax_2007_2017
  StateTax <- StateTax[, c("GeoName", "LineCode", as.character(year))]
  return(StateTax)
}

#' Get industry-level Gross Operating Surplus (GOS) for all states at a specific year.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state GOS for all states at a specific year.
getStateGOS <- function(year) {
  # Load pre-saved state Tax 2007-2017
  StateGOS <- useeior::State_GOS_2007_2017
  StateGOS <- StateGOS[, c("GeoName", "LineCode", as.character(year))]
  return(StateGOS)
}

#' Calculate state-US Commodity Output ratios at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state-US Commodity Output ratios at BEA Summary level.
calculateStateCommodityOutputRatio <- function(year) {
  # Load state Commodity output
  load(paste0("data/State_Summary_CommodityOutput_", year, ".rda"))
  states <- names(State_Summary_CommodityOutput_list)
  # Load US Commodity output
  US_Summary_Make <- get(paste("Summary_Make", year, "BeforeRedef", sep = "_"))*1E6
  US_Summary_MakeTransaction <- US_Summary_Make[-which(rownames(US_Summary_Make)=="Total Commodity Output"),
                                                -which(colnames(US_Summary_Make)=="Total Industry Output")]
  US_Summary_CommodityOutput <- colSums(US_Summary_MakeTransaction)
  # Calculate state Commodity output ratio
  State_CommodityOutputRatio <- data.frame()
  for (state in states) {
    CommodityOutputRatio <- cbind.data.frame(names(US_Summary_CommodityOutput), state,
                                             State_Summary_CommodityOutput_list[[state]]/US_Summary_CommodityOutput)
    colnames(CommodityOutputRatio) <- c("BEA_2012_Summary_Code", "State", "Ratio")
    CommodityOutputRatio[, c("BEA_2012_Summary_Code", "State")] <- sapply(CommodityOutputRatio[, c("BEA_2012_Summary_Code", "State")], as.character)
    rownames(CommodityOutputRatio) <- NULL
    State_CommodityOutputRatio <- rbind(State_CommodityOutputRatio, CommodityOutputRatio)
  }
  return(State_CommodityOutputRatio)
}

#' Assemble Summary-level value added sectors (V001, V002, V003) for all states at a specific year.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains Summary-level value added (V001, V002, V003) for all states at a specific year.
assembleStateValueAdded <- function(year) {
  StateValueAdded <- data.frame()
  for (sector in c("EmpCompensation", "Tax", "GOS")) {
    # Generate Value Added tables by BEA
    df <- allocateStateTabletoBEASummary(sector, year, allocationweightsource = "Employment")
    # Convert table from long to wide
    df <- reshape2::dcast(df, GeoName ~ BEA_2012_Summary_Code, value.var = as.character(year))
    # Assign row names
    rownames(df) <- df$GeoName
    # Drop GeoName column
    df$GeoName <- NULL
    # Replace NA with 0
    df[is.na(df)] <- 0
    # Calculate Value Added for Overseas
    df["Overseas", ] <- df[rownames(df)=="United States *", ] - colSums(df[rownames(df)!="United States *", ])
    # Drop US values
    df <- df[rownames(df)!="United States *", ]
    # Modify row names
    sector_code <- ifelse(sector=="EmpCompensation", "V001",
                          ifelse(sector=="Tax", "V002",
                                 ifelse(sector=="GOS", "V003")))
    rownames(df) <- paste(rownames(df), sector_code, sep = ".")
    StateValueAdded <- rbind(StateValueAdded, df)
  }
  return (StateValueAdded)
}

#' Calculate state-US PCE (personal consumption expenditures) ratios at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains ratios of state/US PCE for all states at a specific year at BEA Summary level.
calculateStateUSPCERatio <- function(year) {
  # Calculate PCE for Overseas
  StatePCE <- get("State_PCE_2007_2018")[, c("GeoName", "Line", as.character(year))]
  # Generate sum of state PCE
  StatePCE_sum <- stats::aggregate(StatePCE[, as.character(year)], by = list(StatePCE$Line), sum)
  colnames(StatePCE_sum) <- c("Line", as.character(year))
  # Load US PCE from state PCE table
  PCEtable <- utils::read.table("inst/extdata/SAEXP/SAEXP1__ALL_AREAS_1997_2018.csv", sep = ",",
                                header = TRUE, stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE)
  USPCE <- PCEtable[PCEtable$GeoName=="United States", c("Line", as.character(year))]
  # Convert values to US $
  USPCE[, as.character(year)] <- USPCE[, as.character(year)]*1E6
  # Merge sum of state PCE with US PCE to get PCE of Overseas region
  OverseasPCE <- merge(StatePCE_sum, USPCE, by = "Line")
  OverseasPCE[, as.character(year)] <- OverseasPCE[, paste0(year, ".y")] - OverseasPCE[, paste0(year, ".x")]
  OverseasPCE$GeoName <- "Overseas"
  # Append Overseas PCE to StatePCE
  StatePCE <- rbind(StatePCE, OverseasPCE[, colnames(StatePCE)])
  # Merge State and US PCE by LineCode
  StateUSPCE <- merge(StatePCE, USPCE, by = "Line")
  # Calculate the state-US PCE ratios by LineCode
  StateUSPCE$Ratio <- StateUSPCE[, paste0(year, ".x")]/StateUSPCE[, paste0(year, ".y")]
  # Map to BEA Summary
  StatePCEtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StatePCEtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                            sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  StateUSPCE <- merge(StatePCEtoBEASummary[!StatePCEtoBEASummary$BEA_2012_Summary_Code=="", ],
                      StateUSPCE, by = "Line")
  # Adjust Ratio based on state PCE
  for (state in unique(StateUSPCE$GeoName)) {
    for (sector in unique(StateUSPCE$BEA_2012_Summary_Code)) {
      adjustedratio <- weighted.mean(StateUSPCE[StateUSPCE$GeoName==state&StateUSPCE$BEA_2012_Summary_Code==sector, "Ratio"],
                                     StateUSPCE[StateUSPCE$GeoName==state&StateUSPCE$BEA_2012_Summary_Code==sector, paste0(year, ".x")])
      StateUSPCE[StateUSPCE$GeoName==state&StateUSPCE$BEA_2012_Summary_Code==sector, "Ratio"] <- adjustedratio
    }
  }
  # Replace NaN with zero
  StateUSPCE[is.na(StateUSPCE$Ratio), "Ratio"] <- 0
  StateUSPCE$State <- StateUSPCE$GeoName
  # Keep wanted columns
  StateUSPCE <- unique(StateUSPCE[order(StateUSPCE$State, StateUSPCE$BEA_2012_Summary_Code),
                                  c("BEA_2012_Summary_Code", "State", "Ratio")])
  return(StateUSPCE)
}

#' Estimate state household demand at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state household demand for all states at a specific year at BEA Summary level.
estimateStateHouseholdDemand <- function(year) {
  US_Summary_Use <- get(paste("Summary_Use", year, "PRO_BeforeRedef", sep = "_"))*1E6
  US_HouseholdDemand <- US_Summary_Use[Commodities, HouseholdDemandCodes, drop = FALSE]
  PCE_ratio <- calculateStateUSPCERatio(year)
  State_HouseholdDemand <- data.frame()
  for (state in unique(PCE_ratio$State)) {
    HouseholdDemand <- US_HouseholdDemand * PCE_ratio[PCE_ratio$State==state, "Ratio"]
    rownames(HouseholdDemand) <- paste(rownames(HouseholdDemand), state, sep = ".")
    State_HouseholdDemand <- rbind.data.frame(State_HouseholdDemand, HouseholdDemand)
  }
  return(State_HouseholdDemand)
}

#' Calculate state-US employee compensation ratios at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state-US employment compensation ratios for all states at a specific year at BEA Summary level.
calculateStateUSEmpCompensationRatio <- function(year) {
  # Generate state Employee Compensation table
  StateEmpCompensation <- allocateStateTabletoBEASummary("EmpCompensation", year, "Employment")
  # Separate into state Employee Compensation
  StateEmpCompensation <- StateEmpCompensation[StateEmpCompensation$GeoName!="United States *", ]
  # Map US Employee Compensation to BEA
  USEmpCompensation <- getStateEmpCompensation(year)
  USEmpCompensation <- USEmpCompensation[USEmpCompensation$GeoName=="United States *", ]
  BEAStateGDPtoBEASummary <- utils::read.table(system.file("extdata", "Crosswalk_StateGDPtoBEASummaryIO2012Schema.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  allocation_sectors <- BEAStateGDPtoBEASummary[duplicated(BEAStateGDPtoBEASummary$LineCode) | duplicated(BEAStateGDPtoBEASummary$LineCode, fromLast = TRUE), ]
  USEmpCompensation <- merge(USEmpCompensation, BEAStateGDPtoBEASummary, by = "LineCode")
  USEmpCompensation <- merge(USEmpCompensation, useeior::Summary_GrossOutput_IO[, as.character(year), drop = FALSE],
                             by.x = "BEA_2012_Summary_Code", by.y = 0)
  USEmpCompensation[, as.character(year)] <- USEmpCompensation[, paste0(year, ".x")]
  for (linecode in unique(allocation_sectors$LineCode)) {
    value_vector <- USEmpCompensation[USEmpCompensation$LineCode==linecode, paste0(year, ".x")]
    weight_vector <- USEmpCompensation[USEmpCompensation$LineCode==linecode, paste0(year, ".y")]
    USEmpCompensation[USEmpCompensation$LineCode==linecode, as.character(year)] <- value_vector*(weight_vector/sum(weight_vector))
  }
  USEmpCompensation <- USEmpCompensation[, c("BEA_2012_Summary_Code", as.character(year))]
  # Generate sum of state Employee Compensation
  StateEmpCompensation_sum <- stats::aggregate(StateEmpCompensation[, as.character(year)],
                                               by = list(StateEmpCompensation$BEA_2012_Summary_Code),
                                               sum, na.rm = TRUE)
  colnames(StateEmpCompensation_sum) <- c("BEA_2012_Summary_Code", "StateEmpCompensation_sum")
  # Merge sum of state Employee Compensation with US employee Compensation to get employee Compensation of Overseas region
  OverseasEmpCompensation <- merge(StateEmpCompensation_sum, USEmpCompensation,
                                   by = "BEA_2012_Summary_Code", all.x = TRUE)
  OverseasEmpCompensation[is.na(OverseasEmpCompensation)] <- 0
  OverseasEmpCompensation[, paste0(year, ".x")] <- OverseasEmpCompensation[, as.character(year)] - OverseasEmpCompensation$StateEmpCompensation_sum
  OverseasEmpCompensation[, paste0(year, ".y")] <- OverseasEmpCompensation[, as.character(year)]
  OverseasEmpCompensation$GeoName <- "Overseas"
  # Merge state GDP and US Employee Compensation tables
  StateUSEmpCompensation <- merge(StateEmpCompensation, USEmpCompensation, by = "BEA_2012_Summary_Code")
  # Append Overseas EmpCompensation to StateUSEmpCompensation
  StateUSEmpCompensation <- rbind(StateUSEmpCompensation, OverseasEmpCompensation[, colnames(StateUSEmpCompensation)])
  # Calculate the state-US Employee Compensation ratios
  StateUSEmpCompensation$Ratio <- StateUSEmpCompensation[, paste0(year, ".x")]/StateUSEmpCompensation[, paste0(year, ".y")]
  StateUSEmpCompensation <- StateUSEmpCompensation[order(StateUSEmpCompensation$GeoName, StateUSEmpCompensation$BEA_2012_Summary_Code),
                                         c("BEA_2012_Summary_Code", "GeoName", "Ratio")]
  rownames(StateUSEmpCompensation) <- NULL
  return(StateUSEmpCompensation)
}

#' Calculate weighting factor of each expenditure component over US total gov expenditure.
#' @param year A numeric value between 2007 and 2019 specifying the year of interest.
#' @param defense A boolean value indicating if the expenditure is spent on defense or not.
#' @return A data frame contains weighting factor of each expenditure component over US total gov expenditure.
calculateUSGovExpenditureWeightFactor <- function(year, defense) {
  # Load data
  GovInvestment <- useeior::GovInvestment_2007_2019
  GovConsumption <- useeior::GovConsumption_2007_2019
  # Keep rows by line code
  if (defense) {
    GovConsumption <- GovConsumption[GovConsumption$Line%in%c(26:28), ]
    GovInvestment <- GovInvestment[GovInvestment$Line%in%c(20:21, 23:24), ]
  } else {
    GovConsumption <- GovConsumption[GovConsumption$Line%in%c(37:39), ]
    GovInvestment <- GovInvestment[GovInvestment$Line%in%c(28:29, 31:32), ]
  }
  # Calculate weight factors and stack dfs together
  WeightFactor <- rbind(cbind(GovConsumption[, c("Line", "Description")],
                              GovConsumption[, as.character(year), drop = FALSE]/colSums(GovConsumption[, as.character(year), drop = FALSE])),
                        cbind(GovInvestment[, c("Line", "Description")],
                              GovInvestment[, as.character(year), drop = FALSE]/colSums(GovInvestment[, as.character(year), drop = FALSE])))
  # Modify Description
  WeightFactor$Description <- gsub("\\\\.*", "", WeightFactor$Description)
  return(WeightFactor)
}

#' Calculate state and local government expenditure ratio at BEA Summary level.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state and local government expenditure ratio for all states at a specific year at BEA Summary level.
calculateStateLocalGovExpenditureRatio <- function(year) {
  # Load state and local government expenditure
  GovExp <- get(paste0("Census_StateLocalGovExpenditure_", year), as.environment("package:useeior"))
  # Map to BEA Summary sectors
  mapping <- utils::read.table(system.file("extdata", "Crosswalk_StateLocalGovExptoBEASummaryIO2012Schema.csv", package = "useeior"),
                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  GovExpBEA <- merge(mapping, GovExp, by = "Description")
  # Aggregate by BEA
  GovExpBEA <- stats::aggregate(GovExpBEA[, c("United States Total", state.name, "District of Columbia")],
                                by = list(GovExpBEA$BEA_2012_Summary_Code), sum)
  colnames(GovExpBEA)[1] <- "BEA_2012_Summary_Code"
  TotalGovExp <- GovExp[GovExp$Description=="Expenditure1", ]
  colnames(TotalGovExp) <- colnames(GovExpBEA)
  TotalGovExp$BEA_2012_Summary_Code <- "TotalExpenditure"
  GovExpBEA <- rbind(GovExpBEA, TotalGovExp)
  # Calculate ratios
  GovExpBEA[, c(state.name, "District of Columbia")] <- GovExpBEA[, c(state.name, "District of Columbia")]/GovExpBEA[, "United States Total"]
  # Convert table from wide to long
  GovExpBEA[, "United States Total"] <- NULL
  GovExpBEA <- reshape2::melt(GovExpBEA, id.vars = "BEA_2012_Summary_Code")
  colnames(GovExpBEA) <- c("BEA_2012_Summary_Code", "State", "Ratio")
  return(GovExpBEA)
}

#' Calculate state private investment at BEA Summary level.
#' Apply state PCE ratio to F02R.
#' Apply state Gross Output ratio to F02S, F02E, F02N, and F030.
#' @param year A numeric value between 2007 and 2017 specifying the year of interest.
#' @return A data frame contains state household demand for all states at a specific year at BEA Summary level.
estimateStatePrivateInvestment <- function(year) {
  US_Summary_Use <- get(paste("Summary_Use", year, "PRO_BeforeRedef", sep = "_"))*1E6
  US_PrivateInvestment <- US_Summary_Use[Commodities, c(InvestmentDemandCodes, ChangeInventories), drop = FALSE]
  # Separate US_PrivateInvestment into Residential (F02R) and NonResidential (F02S, F02E, F02N, and F030)
  US_ResidentialInvestment <- US_PrivateInvestment[Commodities, "F02R", drop = FALSE]
  US_NonResidentialInvestment <- US_PrivateInvestment[Commodities, colnames(US_PrivateInvestment)!="F02R"]
  # Apply state PCE ratio to F02R.
  # Generate state PCE ratio
  PCE_ratio <- calculateStateUSPCERatio(year)
  # Apply state Gross Output ratio to F02S, F02E, F02N, and F030
  # Generate state Commodity Output ratio
  GrossOutput_ratio <- calculateStateCommodityOutputRatio(year)
  # Calculate state Private Investment (Residential and NonResidential)
  State_ResidentialInvestment <- data.frame()
  State_NonResidentialInvestment <- data.frame()
  for (state in unique(PCE_ratio$State)) {
    # Residential
    ResidentialInvestment <- US_ResidentialInvestment * PCE_ratio[PCE_ratio$State==state, "Ratio"]
    rownames(ResidentialInvestment) <- paste(rownames(ResidentialInvestment), state, sep = ".")
    State_ResidentialInvestment <- rbind.data.frame(State_ResidentialInvestment, ResidentialInvestment)
    # NonResidential
    NonResidentialInvestment <- US_NonResidentialInvestment * GrossOutput_ratio[GrossOutput_ratio$State==state, "Ratio"]
    rownames(NonResidentialInvestment) <- paste(rownames(NonResidentialInvestment), state, sep = ".")
    State_NonResidentialInvestment <- rbind.data.frame(State_NonResidentialInvestment, NonResidentialInvestment)
  }
  # Assemble State Residential and NonResidential Private Investment
  State_PrivateInvestment <- cbind(State_ResidentialInvestment, State_NonResidentialInvestment)
  State_PrivateInvestment <- State_PrivateInvestment[, colnames(US_PrivateInvestment)]
  return(State_PrivateInvestment)
}
