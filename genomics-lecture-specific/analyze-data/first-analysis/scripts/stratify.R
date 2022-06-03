sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)
source("assets/stratification/utils.R")
source("genomics-lecture-specific/analyze-data/cleanResults.R")

getCompleteResultData <- function() {
  resultData <- getResultData()
  return(withConsent(resultData, consentQuestion = "Q402"))
}
getTumResults <- function() {
  resultData <- getCompleteResultData()
  tumResults <- resultData[resultData$QUESTNNR == "TUM_Retrospective",]
  return(removeTumRetrospectiveFutureAnalysesWithoutPassword(tumResults, passwordQuestionId = "Q408", furtherAnalysesQuestionId = "Q411"))
} 
getHpiResults <- function() {
  resultData <- getCompleteResultData()
  hpiResults <- resultData[resultData$QUESTNNR == "Q4_HPI",]
  return(hpiResults)
}
getCommonQuestions <- function() {
  return(
    c("Q405", "Q407", "Q408", "Q411", "Q412", "Q413", "Q414", "Q417", "Q418", "Q419", "Q420"))
}
getOnlyHpiQuestions <- function() {
  return(c("Q409", "Q415")) # Not included: Q410, Q404
}
getOnlyTumQuestions <- function() {
  return(c("T203", "Q421")) # Not included: T205
}

getComparisonSpecification <- function() {
  # Simple comparison between HPI and TUM
  
  tumResults <- getTumResults()
  hpiResults <- getHpiResults()
  
  tumYearPopulations <- buildPopulations(tumResults, "T201")
  tumAllEducationPopulations <- buildPopulations(tumResults, "T207")
  # 1: Medicine
  # 2: Genetic and Genomic Counseling
  # 3: Other educational background
  tumCondensedEducaltionPopulations <- buildPopulations(tumResults, "T207", groups = list("Medicine" = 1, "Not medicine" = c(2, 3)))
  hpiEducationPopulations <- buildPopulations(hpiResults, "Q403", groups = list("Medicine" = 1, "Not medicine" = 2))
  
  commonQuestions <- getCommonQuestions()
  onlyHPIQuestions <- getOnlyHpiQuestions()
  onlyTUMQuestions <- getOnlyTumQuestions()
  comparisonSpecification <- list(
    "by_uni" = list(
      "populations" = list("HPI" = hpiResults, "TUM" = tumResults),
      "questions" = commonQuestions),
    "tum_by_year" = list(
      "populations" = tumYearPopulations,
      "questions" = c(commonQuestions, onlyTUMQuestions)),
    "tum_by_education" = list(
      "populations" = tumAllEducationPopulations,
      "questions" = c(commonQuestions, onlyTUMQuestions)),
    "tum_medicine_vs_other" = list(
      "populations" = tumCondensedEducaltionPopulations,
      "questions" = c(commonQuestions, onlyTUMQuestions)),
    "hpi_by_medical_background" = list(
      "populations" = hpiEducationPopulations,
      "questions" = c(commonQuestions, onlyHPIQuestions))
  )
  return(comparisonSpecification)
}

computeStratificationResults <- function() {
  comparisonSpecification <- getComparisonSpecification()
  runComparisons(comparisonSpecification)

  # Special cases
  
  joinQuestionIds <- function(ids) {
    return(paste(ids, collapse = "_"))
  }

  ## Analyses: Q410 (HPI) + T203 (TUM)
  source("genomics-lecture-specific/analyze-data/first-analysis/scripts/analyzePastAnalyses.R")
  hpiAnalysesQuestion <- "Q410"
  tumAnalysesQuestion <- "T203"
  jointAnalysesQuestionId <- joinQuestionIds(c(hpiAnalysesQuestion, tumAnalysesQuestion))
  analyzePastAnalyses(getTumResults(), getHpiResults(), getOptionData(), "by_uni", tumAnalysesQuestion, hpiAnalysesQuestion, jointAnalysesQuestionId)
  
  ## Motivations: Q404 (HPI) + T205 (TUM)
  source("genomics-lecture-specific/analyze-data/first-analysis/scripts/analyzeMotivation.R")
  jointMotivationsQuestionId <- joinQuestionIds(c("Q404", "T103", "T205"))
  analyzeMotivation(jointMotivationsQuestionId, "motivation_stratification")

  combinedStatistics <- combineStatistics(
    c(names(comparisonSpecification), "motivation_stratification"),
    c(getCommonQuestions(), getOnlyHpiQuestions(), getOnlyTumQuestions(), jointAnalysesQuestionId, jointMotivationsQuestionId))
  combinedStatistics <- combinedStatistics[order(combinedStatistics$p.value,
                                                 combinedStatistics$nearest.effect.size.interpretation),]
  adjusted.p.over.all.tests <- p.adjust(combinedStatistics$p.value, method = "fdr")
  combinedStatistics$adjusted.p.over.all.tests <- adjusted.p.over.all.tests
  resultFilePath <- file.path(getResultDirectory(), "stratification_stats.csv")
  write.csv(combinedStatistics, resultFilePath, row.names = FALSE)
}
