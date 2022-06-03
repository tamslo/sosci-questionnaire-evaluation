library(rjson)
library(stringr)
#sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
#sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)

source("assets/sankeyPlots/createCombinedSankeyPlots.R", local = TRUE)


getQ1ToQ4SankeyParameters <- function() {
  plot_parameters <- list()
  plot_parameters[["usage_genotyping"]] <- list(
    "title" = "Do you plan to (Q1) or did (Q2, Q3, Q4) you get yourself genotyped?",
    "question.list" =   list(
      "Q1" = "DU02",
      "Q2" = "DU14",
      "Q3" = "DU14",
      "Q4_HPI" = "QX08"
    )
  )
  plot_parameters[["q4_no_counseling"]] = list(
    "title" = "Do you think that personal genotyping should be allowed without genetic counseling?",
    "question.list" = list(
      "Q1" = "RS02_03",
      "Q2" = "RS02_03",
      "Q3" = "RS02_03",
      "Q4_HPI" = "QX07"
    )
  )
  plot_parameters[["q4_analyze_complete_password"]] = list(
    "title" = "Do you want to analyze your complete genotype data (Q1-Q3), did you collect your password (Q4)?",
    "question.list" = list(
      "Q1" = "RS02_05",
      "Q2" = "RS02_05",
      "Q3" = "RS02_05",
      "Q4_HPI" = "Q402"
    )
  )
  plot_parameters[["q4_analyze_complete_password_future"]] = list(
    "title" = "Do you want to analyze your complete genotype data (Q1-Q3), do you plan to collect your password (Q4)?",
    "question.list" = list(
      "Q1" = "RS02_05",
      "Q2" = "RS02_05",
      "Q3" = "RS02_05",
      "Q4_HPI" = "Q403"
    )
  )
  plot_parameters[["q4_analyze_complete_only_receive"]] = list(
    "title" = "Do you want to analyze your complete genotype data (Q1-Q3), do you only want to receive your data (Q4)?",
    "question.list" = list(
      "Q1" = "RS02_05",
      "Q2" = "RS02_05",
      "Q3" = "RS02_05",
      "Q4_HPI" = "Q404_01"
    )
  )
  plot_parameters[["q4_only_receive"]] <- list(
    "title" = "I would only like to receive my complete data, but I do not want to analyze it",
    "question.list" = list(
      "Q1" = "RS02_07",
      "Q2" = "RS02_07",
      "Q3" = "RS02_07",
      "Q4_HPI" = "Q404_01"
    )
  )
  plot_parameters[["q4_usage_beyond_course_password"]] <- list(
    "title" = "Are you planning to receive and analyze your complete data beyond the course (Q1-Q3), did you collect your password (Q4)?",
    "question.list" = list(
      "Q1" = "DU11",
      "Q2" = "DU11",
      "Q3" = "DU11",
      "Q4_HPI" = "Q402"
    )
  )
  plot_parameters[["q4_usage_beyond_course_password_future"]] <- list(
    "title" = "Are you planning to receive and analyze your complete data beyond the course (Q1-Q3), do you plan to collect your password (Q4)?",
    "question.list" = list(
      "Q1" = "DU11",
      "Q2" = "DU11",
      "Q3" = "DU11",
      "Q4_HPI" = "Q403"
    )
  )
  plot_parameters[["q4_opinion_favor_family"]] <- list(
    "title" = "PGT is useful to inform family members about health risks (Q1-Q3), I would recommend PGT to relatives and friends who have not taken the course.",
    "question.list" = list(
      "Q1" = "DU07_02",
      "Q2" = "DU07_02",
      "Q3" = "DU07_02",
      "Q4_HPI" = "QX12"
    )
  )
  plot_parameters[["q4_ancestry"]] = list(
    "title" = "I want to (Q1, Q2) or did (Q3, Q4) calculate my genetic ancestry.",
    "question.list" = list(
      "Q1" = "DU06_03",
      "Q2" = "DU06_03",
      "Q3" = "DU18_06",
      "Q4_HPI" = "Q404_02"
    )
  )
  plot_parameters[["q4_disease_risk_mono"]] = list(
    "title" = "I want to (Q1, Q2) or did (Q3, Q4: monogenic) assess my personal disease risk.",
    "question.list" = list(
      "Q1" = "DU06_02",
      "Q2" = "DU06_02",
      "Q3" = "DU18_05",
      "Q4_HPI" = "Q404_06"
    )
  )
  plot_parameters[["q4_disease_risk_poly"]] = list(
    "title" = "I want to (Q1, Q2) or did (Q3, Q4: polygenic) assess my personal disease risk.",
    "question.list" = list(
      "Q1" = "DU06_02",
      "Q2" = "DU06_02",
      "Q3" = "DU18_05",
      "Q4_HPI" = "Q404_07"
    )
  )
  plot_parameters[["q4_wellness_traits"]] = list(
    "title" = "I want to (Q1, Q2) or did (Q4: monogenic) assess my wellness traits.",
    "question.list" = list(
      "Q1" = "DU06_04",
      "Q2" = "DU06_04",
      "Q4_HPI" = "Q404_04"
    )
  )
  plot_parameters[["q4_pgx"]] = list(
    "title" = "I want to (Q1, Q2) or did (Q3, Q4: monogenic) assess my pharmacogenomic markers.",
    "question.list" = list(
      "Q1" = "DU06_05",
      "Q2" = "DU06_05",
      "Q3" = "DU18_08",
      "Q4_HPI" = "Q404_03"
    )
  )
  plot_parameters[["q4_adequately_trained"]] = list(
    "title" = "I feel adequately trained to analyze and interpret my personal genomic data beyond the course.",
    "question.list" = list(
      "Q1" = "DU13",
      "Q2" = "DU13",
      "Q3" = "DU13",
      "Q4_HPI" = "QX19"
    )
  )
  return(plot_parameters)
}

# file_name <- "q4_sankeys"
# createCombinedSankeyPlots(plot_parameters, file_name)
