sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)
source("assets/analyzeMotivation.R")

getMotivationDataSpecification <- function() {
  tumQ1Ids <- c("T103_01", "T103_02", NA, "T103_03", "T103_04", "T103_05",
                "T103_06", "T103_07", "T103_08", "T103_09", "T103_10", "T103_11")
  tumQ1Specification <- list(
    "questionnaire" = "TUM_Q1",
    "question" = "T103",
    "stratification" = NULL,
    "ids" = tumQ1Ids,
    "color" = getColor(6)
  )
  
  tumRetroIds <- c("T205_01", "T205_02", "T205_03", NA, "T205_05", "T205_06",
                   "T205_07", "T205_08", "T205_09", "T205_10", "T205_11", "T205_12")
  tumRetroMedicalSpecification <- list(
    "questionnaire" = "TUM_Retrospective",
    "question" = "T205",
    "stratification" = list("question" = "T207", "answers" = c(1)),
    "ids" = tumRetroIds,
    "color" = getColor(4)
  )
  tumRetroNonMedicalSpecification <- list(
    "questionnaire" = "TUM_Retrospective",
    "question" = "T205",
    "stratification" = list("question" = "T207", "answers" = c(2, 3)),
    "ids" = tumRetroIds,
    "color" = "#2B93CE"
  )
  tumRetroSpecification <- list(
    "questionnaire" = "TUM_Retrospective",
    "question" = "T205",
    "stratification" = NULL,
    "ids" = tumRetroIds,
    "color" = getColor(5)
  )

  hpiQ4Ids <- c("Q404_01", "Q404_02", "Q404_03", "Q404_04", "Q404_05", "Q404_06",
                "Q404_07", "Q404_08", "Q404_09", "Q404_10", "Q404_11", "Q404_12")
  hpiQ4Specification <- list(
    "questionnaire" = "Q4_HPI",
    "question" = "Q404",
    "stratification" = NULL,
    "ids" = hpiQ4Ids,
    "color" = getColor(1)
  )

  dataSpecification <- list(
    #"TUM Q1" = tumQ1Specification,
    #"TUM (medicine students)" = tumRetroMedicalSpecification,
    #"TUM (other students)" = tumRetroNonMedicalSpecification,
    "TUM" = tumRetroSpecification,
    "HPI" = hpiQ4Specification
  )
  return(dataSpecification)
}

analyzeMotivation <- function(outputQuestionId, resultName) {
  computeMotivationResults(getMotivationDataSpecification(), outputQuestionId, resultName)
}
