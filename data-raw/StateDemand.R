#'Creates Combined Demand Table (in Use Table form) for all 52 states (including DC and Overseas) for a given year
#'Stores table by year .rda files in useeior package

#' 1 - Load state industry output for the given year.
year <- 2012
load(paste0("data/State_Summary_IndustryOutput_", year, ".rda"))
states <- names(State_Summary_IndustryOutput_list)

#' 2 - Load US Summary Make table for given year
#' Generate US Summary Industry Output
US_Summary_Make <- get(paste("Summary_Make", year, "BeforeRedef", sep = "_"))*1E6
US_Summary_MakeTransaction <- US_Summary_Make[-which(rownames(US_Summary_Make)=="Total Commodity Output"),
                                              -which(colnames(US_Summary_Make)=="Total Industry Output")]
US_Summary_IndustryOutput <- rowSums(US_Summary_MakeTransaction)

#' 3 - Load US Summary Use table for given year
#' Generate US Summary Use Transaction
US_Summary_Use <- get(paste("Summary_Use", year, "PRO_BeforeRedef", sep = "_"))*1E6
US_Summary_UseTransaction <- US_Summary_Use[colnames(US_Summary_MakeTransaction),
                                            rownames(US_Summary_MakeTransaction)]

#' 4 - Calculate state_US_IndustryOutput_ratio, for each state and each industry,
#' Divide state IndustryOutput by US IndustryOutput.
State_Summary_UseTransaction_list <- list()
for (state in states) {
  IndustryOutputRatio <- State_Summary_IndustryOutput_list[[state]]/US_Summary_IndustryOutput
  State_Summary_UseTransaction_list[[state]] <- as.matrix(US_Summary_UseTransaction) %*% diag(IndustryOutputRatio)
  colnames(State_Summary_UseTransaction_list[[state]]) <- colnames(US_Summary_UseTransaction)
}

#' 5 - Vertically stack all state Use trascation tables.
State_Summary_UseTransaction <- do.call(rbind, State_Summary_UseTransaction_list)
rownames(State_Summary_UseTransaction) <- paste(rep(names(State_Summary_UseTransaction_list),
                                                    each = nrow(State_Summary_UseTransaction_list[[1]])),
                                                rep(rownames(State_Summary_UseTransaction_list[[1]]),
                                                    time = length(names(State_Summary_UseTransaction_list))),
                                                sep = ".")
colnames(State_Summary_UseTransaction) <- colnames(US_Summary_UseTransaction)

#' 6 - Validate if state totals == national total
# Row sum
rowSums(State_Summary_UseTransaction) - rowSums(US_Summary_UseTransaction)
# Column sum
State_CommInputTotal_list <- list()
for (industry in colnames(US_Summary_UseTransaction)) {
  State_CommInputTotal_list[[industry]] <- sum(State_Summary_UseTransaction[, paste(states, industry, sep = ".")])
}
unlist(State_CommInputTotal_list) - colSums(US_Summary_UseTransaction)

#' 7 - For each state, append detailed Value Added to the end of Use table
State_Value_Added <- assembleStateValueAdded(year)
State_Summary_Use <- rbind(State_Summary_UseTransaction, State_Value_Added)
State_Summary_Use <- State_Summary_Use[order(rownames(State_Summary_Use)), ]

#' 8 - Apply RAS to PCE
State_PCE <- estimateStateHouseholdDemand(year)
State_PCE[, "BEA_2012_Summary_Code"] <- gsub("\\..*", "", rownames(State_PCE))
State_PCE[, "State"] <- gsub(".*\\.", "", rownames(State_PCE))
State_PCE <- reshape2::dcast(State_PCE, BEA_2012_Summary_Code ~ State, value.var = "F010")
m0 <- State_PCE[, -1]
t_r <- US_Summary_Use[, "F010"]
t_c <- as.numeric(colSums(US_Summary_MakeTransaction))
# Adjust t_c/t_r, make sum(t_c)==sum(t_r)
if (sum(t_c) > sum(t_r)) {
  t_r <- (t_r/sum(t_r))*sum(t_c)
} else {
  t_c <- (t_c/sum(t_c))*sum(t_r)
}
t <- ToleranceforRAS(t_r, t_c, NULL, 1E6)
State_Summary_MakeTransaction_balanced <- RAS(m0, t_r, t_c, t, max_itr = 1E6)
colnames(State_Summary_MakeTransaction_balanced) <- colnames(m0)

#' 9 - Apply RAS to state and local gov expenditure
#' 10 - Apply RAS to federal gov expenditure