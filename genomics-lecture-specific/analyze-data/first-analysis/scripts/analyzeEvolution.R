source("modules/data/load.R", local = TRUE)
source("modules/data/questions.R", local = TRUE)

getEvolutionSpecification <- function() {
  evolutionSpecification <- list()
  evolutionSpecification[["titles"]] <- list()
  evolutionSpecification[["titles"]][["wgs"]] <- "(1) Whole Genome Sequencing (WGS)"
  evolutionSpecification[["titles"]][["understanding"]] <- "(2) Understanding genetic data"
  evolutionSpecification[["titles"]][["gendg"]] <- "(3) Legislation"
  evolutionSpecification[["titles"]][["opinion"]] <- "(4) General opinions on genetic testing"
  evolutionSpecification[["titles"]][["decision"]] <- "(5) Descisions to get tested"
  evolutionSpecification[["titles"]][["useful"]] <- "(6) Usefulness of genetic data"
  evolutionSpecification[["titles"]][["usage"]] <- "(7) Usage of genetic data_TEST"

  evolutionSpecification[["wgs"]] <- list()
  evolutionSpecification[["wgs"]][["wgs_conducted"]] <- list(
    "title" = "Did you get your genome sequenced with WGS outside of this course?",
    "question.list" = list(
      "Q1" = "WG05",
      "Q2" = "WG05",
      "Q3" = "WG05"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_accurate"]] <- list(
    "title" = "I think WGS results are more accurate than results from array data",
    "question.list" = list(
      "Q1" = "WG02_01",
      "Q2" = "WG02_01",
      "Q3" = "WG02_01"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_predictive"]] <- list(
    "title" = "I think WGS results are more predictive than results from array data",
    "question.list" = list(
      "Q1" = "WG02_02",
      "Q2" = "WG02_02",
      "Q3" = "WG02_02"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_prefer"]] <- list(
    "title" = "I would prefer WGS technology over array technology for genotyping my own data",
    "question.list" = list(
      "Q1" = "WG02_03",
      "Q2" = "WG02_03",
      "Q3" = "WG02_03"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_risk"]] <- list(
    "title" = "I think WGS results are connected with more risks than results from array data",
    "question.list" = list(
      "Q1" = "WG02_04",
      "Q2" = "WG02_04",
      "Q3" = "WG02_04"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_privacy"]] <- list(
    "title" = "I am more concerned about privacy issues when using WGS technology, compared to using array technology",
    "question.list" = list(
      "Q1" = "WG02_05",
      "Q2" = "WG02_05",
      "Q3" = "WG02_05"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_physician"]] <- list(
    "title" = "I think the results from WGS are more useful to a physician than results from array data",
    "question.list" = list(
      "Q1" = "WG02_06",
      "Q2" = "WG02_06",
      "Q3" = "WG02_06"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_patients"]] <- list(
    "title" = "I think the results from WGS are more useful to patients themselves than results from array data",
    "question.list" = list(
      "Q1" = "WG02_07",
      "Q2" = "WG02_07",
      "Q3" = "WG02_07"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_patient_behavior"]] <- list(
    "title" = "I think the results from WGS would more likely lead to changes in patients' behavior than results from array data",
    "question.list" = list(
      "Q1" = "WG02_08",
      "Q2" = "WG02_08",
      "Q3" = "WG02_08"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_personal_behavior"]] <- list(
    "title" = "I think the results from WGS would more likely lead to changes in my personal behavior than results from array data",
    "question.list" = list(
      "Q1" = "WG02_09",
      "Q2" = "WG02_09",
      "Q3" = "WG02_09"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_future_decisions"]] <- list(
    "title" = "I think the results from WGS would more likely lead to changes in my future health care decisions than results from array data",
    "question.list" = list(
      "Q1" = "WG02_10",
      "Q2" = "WG02_10",
      "Q3" = "WG02_10"
    )
  )
  
  evolutionSpecification[["wgs"]][["wgs_free"]] <- list(
    "title" = "Would you get yourself genotyped with WGS at no charge?",
    "question.list" = list(
      "Q1" = "WG03",
      "Q2" = "WG03",
      "Q3" = "WG03"
    )
  )
  evolutionSpecification[["wgs"]][["wgs_pay"]] <- list(
    "title" = "Would you get yourself genotyped with WGS if you had to pay for it?",
    "question.list" = list(
      "Q1" = "WG04",
      "Q2" = "WG04",
      "Q3" = "WG04"
    )
  )
  evolutionSpecification[["understanding"]] <- list()
  evolutionSpecification[["understanding"]][["understanding_variant"]] <- list(
    "title" = "Please state whether you understood the concept 'variant'.",
    "question.list" =   list(
      "Q1" = "GC03_01",
      "Q2" = "GC03_01",
      "Q3" = "GC03_01"
    )
  )
  evolutionSpecification[["understanding"]][["understanding_genotyping"]] <- list(
    "title" = "Please state whether you understood the concept 'genotyping'.",
    "question.list" =   list(
      "Q1" = "GC03_02",
      "Q2" = "GC03_02",
      "Q3" = "GC03_02"
    )
  )
  evolutionSpecification[["understanding"]][["understanding_array"]] <- list(
    "title" = "Please state whether you understood the concept 'array technology'.",
    "question.list" =   list(
      "Q1" = "GC03_03",
      "Q2" = "GC03_03",
      "Q3" = "GC03_03"
    )
  )
  evolutionSpecification[["understanding"]][["understanding_wgs"]] <- list(
    "title" = "Please state whether you understood the concept 'whole genome sequencing'.",
    "question.list" =   list(
      "Q1" = "GC03_04",
      "Q2" = "GC03_04",
      "Q3" = "GC03_04"
    )
  )
  evolutionSpecification[["understanding"]][["understanding_difference"]] <- list(
    "title" = "Please state whether you understood the concept 'the difference between array technology and whole genome sequencing'.",
    "question.list" =   list(
      "Q1" = "GC03_05",
      "Q2" = "GC03_05",
      "Q3" = "GC03_05"
    )
  )
  evolutionSpecification[["gendg"]] <- list()
  evolutionSpecification[["gendg"]][["gendg_appropriate"]] <- list(
    "title" = "I think the GenDG is appropriate",
    "question.list" = list(
      "Q1" = "RS02_01",
      "Q2" = "RS02_01",
      "Q3" = "RS02_01"
    )
  )
  evolutionSpecification[["gendg"]][["gendg_always"]] <- list(
    "title" = "I think genetic testing should be allowed independently of disease diagnosis",
    "question.list" = list(
      "Q1" = "RS02_02",
      "Q2" = "RS02_02",
      "Q3" = "RS02_02"
    )
  )
  evolutionSpecification[["gendg"]][["gendg_no_counseling"]] <- list(
    "title" = "I think genetic testing should be allowed without genetic counseling",
    "question.list" = list(
      "Q1" = "RS02_03",
      "Q2" = "RS02_03",
      "Q3" = "RS02_03"
    )
  )
  evolutionSpecification[["gendg"]][["gendg_get_counseling"]] <- list(
    "title" = "In the context of clinical diagnostics, I would make use of genetic counseling",
    "question.list" = list(
      "Q1" = "RS02_04",
      "Q2" = "RS02_04",
      "Q3" = "RS02_04"
    )
  )
  evolutionSpecification[["gendg"]][["gendg_parts"]] <- list(
    "title" = "I would like to analyze only parts of my data",
    "question.list" = list(
      "Q1" = "RS02_06",
      "Q2" = "RS02_06",
      "Q3" = "RS02_06"
    )
  )
  evolutionSpecification[["gendg"]][["gendg_receive"]] <- list(
    "title" = "I would only like to receive my complete data, but I do not want to analyze it",
    "question.list" = list(
      "Q1" = "RS02_07",
      "Q2" = "RS02_07",
      "Q3" = "RS02_07"
    )
  )
  evolutionSpecification[["gendg"]][["gendg_complete"]] <- list(
    "title" = "I would like to analyze my complete data",
    "question.list" = list(
      "Q1" = "RS02_05",
      "Q2" = "RS02_05",
      "Q3" = "RS02_05"
    )
  )
  evolutionSpecification[["opinion"]] <- list()
  evolutionSpecification[["opinion"]][["opinion_favor_curiosity"]] <- list(
    "title" = "PGT is useful to satisfy general curiosity",
    "question.list" = list(
      "Q1" = "DU07_01",
      "Q2" = "DU07_01",
      "Q3" = "DU07_01"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_favor_family"]] <- list(
    "title" = "PGT is useful to inform family members about health risks",
    "question.list" = list(
      "Q1" = "DU07_02",
      "Q2" = "DU07_02",
      "Q3" = "DU07_02"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_favor_patients"]] <- list(
    "title" = "PGT is useful to understand what a patient may learn/experience",
    "question.list" = list(
      "Q1" = "DU07_03",
      "Q2" = "DU07_03",
      "Q3" = "DU07_03"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_favor_learn"]] <- list(
    "title" = "PGT is useful to help understand principles of human genetics",
    "question.list" = list(
      "Q1" = "DU07_04",
      "Q2" = "DU07_04",
      "Q3" = "DU07_04"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_against_accuracy"]] <- list(
    "title" = "PGT results are not accurate (the results are error-prone)",
    "question.list" = list(
      "Q1" = "DU08_01",
      "Q2" = "DU08_01",
      "Q3" = "DU08_01"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_against_predictive"]] <- list(
    "title" = "PGT results are not predictive (I cannot be sure a condition indicated by the results will break out)",
    "question.list" = list(
      "Q1" = "DU08_02",
      "Q2" = "DU08_02",
      "Q3" = "DU08_02"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_against_privacy"]] <- list(
    "title" = "Regarding PGT, I have concerns about privacy",
    "question.list" = list(
      "Q1" = "DU08_03",
      "Q2" = "DU08_03",
      "Q3" = "DU08_03"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_against_useful"]] <- list(
    "title" = "PGT information will not be medically useful and will not change medical decisions",
    "question.list" = list(
      "Q1" = "DU08_04",
      "Q2" = "DU08_04",
      "Q3" = "DU08_04"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_against_unwanted"]] <- list(
    "title" = "I get unwanted information from PGT",
    "question.list" = list(
      "Q1" = "DU08_05",
      "Q2" = "DU08_05",
      "Q3" = "DU08_05"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_course_disadvantage"]] <- list(
    "title" = "I feel that I would be at a disadvantage to my classmates if I did not undergo the testing",
    "question.list" = list(
      "Q1" = "DU09_01",
      "Q2" = "DU09_01",
      "Q3" = "DU09_01"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_course_opportunity"]] <- list(
    "title" = "I see this as an opportunity to get a service that I would not ordinarily get",
    "question.list" = list(
      "Q1" = "DU09_02",
      "Q2" = "DU09_02",
      "Q3" = "DU09_02"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_course_instructors_testing"]] <- list(
    "title" = "I am concerned that my professors or course instructors would know who took up the offer of testing and who did not",
    "question.list" = list(
      "Q1" = "DU09_03",
      "Q2" = "DU09_03",
      "Q3" = "DU09_03"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_course_classmates_testing"]] <- list(
    "title" = "I am concerned that my classmates would know who took up the offer of testing and who did not",
    "question.list" = list(
      "Q1" = "DU09_04",
      "Q2" = "DU09_04",
      "Q3" = "DU09_04"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_course_improve"]] <- list(
    "title" = "I see this as an opportunity to get information that would help me improve my health and wellbeing",
    "question.list" = list(
      "Q1" = "DU09_05",
      "Q2" = "DU09_05",
      "Q3" = "DU09_05"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_course_disturbing"]] <- list(
    "title" = "I am concerned that I might get some results that would be disturbing",
    "question.list" = list(
      "Q1" = "DU09_06",
      "Q2" = "DU09_06",
      "Q3" = "DU09_06"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_course_counseling_results"]] <- list(
    "title" = "I would only take up the offer of testing if I could get genetic counseling after I got my results back",
    "question.list" = list(
      "Q1" = "DU09_07",
      "Q2" = "DU09_07",
      "Q3" = "DU09_07"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_course_counseling_sample"]] <- list(
    "title" = "I would only take up the offer of testing if I could get genetic counseling before my sample is sent in",
    "question.list" = list(
      "Q1" = "DU09_08",
      "Q2" = "DU09_08",
      "Q3" = "DU09_08"
    )
  )
  evolutionSpecification[["opinion"]][["opinion_course_health"]] <- list(
    "title" = "I would be concerned that people would find out genetic or health information about me",
    "question.list" = list(
      "Q1" = "DU09_09",
      "Q2" = "DU09_09",
      "Q3" = "DU09_09"
    )
  )
  evolutionSpecification[["decision"]] <- list()
  # TODO: Also plot all DU05 items for Q1
  evolutionSpecification[["decision"]][["decision_informed"]] <- list(
    "title" = "Regarding the decision whether or not to get genotyped, I feel adequately informed about the issues important to my decision",
    "question.list" = list(
      "Q2" = "DU17_01",
      "Q3" = "DU17_01"
    )
  )
  evolutionSpecification[["decision"]][["decision_best"]] <- list(
    "title" = "Regarding the decision whether or not to get genotyped, the decision I made was the best decision possible for me personally",
    "question.list" = list(
      "Q2" = "DU17_02",
      "Q3" = "DU17_02"
    )
  )
  evolutionSpecification[["decision"]][["decision_own"]] <- list(
    "title" = "Regarding the decision whether or not to get genotyped, I am satisfied that this was my decision to make",
    "question.list" = list(
      "Q2" = "DU17_03",
      "Q3" = "DU17_03"
    )
  )
  evolutionSpecification[["decision"]][["decision_satisfied"]] <- list(
    "title" = "Regarding the decision whether or not to get genotyped, I am satisfied with my decision",
    "question.list" = list(
      "Q2" = "DU17_04",
      "Q3" = "DU17_04"
    )
  )
  evolutionSpecification[["decision"]][["decision_comfortable"]] <- list(
    "title" = "Regarding the decision whether or not to get genotyped, I am comfortable with my decision",
    "question.list" = list(
      "Q2" = "DU17_05",
      "Q3" = "DU17_05"
    )
  )
  evolutionSpecification[["useful"]] <- list()
  evolutionSpecification[["useful"]][["useful_learning"]] <- list(
    "title" = "I think analyzing my own genomic data as part of the AYPG course is useful",
    "question.list" =   list(
      "Q1" = "DF02_01",
      "Q2" = "DF02_01",
      "Q3" = "DF02_01"
    )
  )
  evolutionSpecification[["useful"]][["useful_physicians"]] <- list(
    "title" = "I think the results from array genotyping are useful to physicians",
    "question.list" =   list(
      "Q1" = "DF02_02",
      "Q2" = "DF02_02",
      "Q3" = "DF02_02"
    )
  )
  evolutionSpecification[["useful"]][["useful_patients"]] <- list(
    "title" = "I think the results from array genotyping are useful to patients themselves",
    "question.list" =   list(
      "Q1" = "DF02_03",
      "Q2" = "DF02_03",
      "Q3" = "DF02_03"
    )
  )
  evolutionSpecification[["useful"]][["useful_patients_behavior"]] <- list(
    "title" = "I think the results from array genotyping lead to changes in patients' behavior",
    "question.list" =   list(
      "Q1" = "DF02_04",
      "Q2" = "DF02_04",
      "Q3" = "DF02_04"
    )
  )
  evolutionSpecification[["useful"]][["useful_own_behavior"]] <- list(
    "title" = "I think the results from array genotyping will lead to changes in my personal behavior",
    "question.list" =   list(
      "Q1" = "DF02_05",
      "Q2" = "DF02_05",
      "Q3" = "DF02_05"
    )
  )
  evolutionSpecification[["useful"]][["useful_future_decisions"]] <- list(
    "title" = "I think the results from array genotyping will lead to changes in my future health care decisions",
    "question.list" =   list(
      "Q1" = "DF02_06",
      "Q2" = "DF02_06",
      "Q3" = "DF02_06"
    )
  )
  evolutionSpecification[["usage"]] <- list()
  # Creating Sankey with only one node throws error; use single diagram from www
  # evolutionSpecification[["usage"]][["already_genotyped"]] <- list(
  #   "title" = "Did you get yourself already genotyped outside of this course?",
  #   "question.list" = list(
  #     "Q1" = "GC04"
  #   )
  # )
  evolutionSpecification[["usage"]][["usage_genotyping"]] <- list(
    "title" = "Do you plan to (Q1) or did (Q2, Q3) you get yourself genotyped?",
    "question.list" =   list(
      "Q1" = "DU02",
      "Q2" = "DU14",
      "Q3" = "DU14"
    )
  )
  evolutionSpecification[["usage"]][["usage_decision_strongness"]] <- list(
    "title" = "I expect to stick with my decision to conduct PGT (Q1) or I would go for the same choice again (Q2, Q3).",
    "question.list" =   list(
      "Q1" = "DU05_06",
      "Q2" = "DU16",
      "Q3" = "DU16"
    )
  )
  evolutionSpecification[["usage"]][["usage_payment"]] <- list(
    "title" = "I would conduct PGT if I had to pay for it myself.",
    "question.list" =   list(
      "Q1" = "DU10",
      "Q2" = "DU10",
      "Q3" = "DU10"
    )
  )
  evolutionSpecification[["usage"]][["usage_not_interested"]] <- list(
    "title" = "I am not interested in my own data. I want to learn how to prepare and analyze genetic data for research in general.",
    "question.list" = list(
      "Q1" = "DU06_01",
      "Q2" = "DU06_01"
    )
  )
  evolutionSpecification[["usage"]][["usage_variant_annotation"]] <- list(
    "title" = "I want to (Q1, Q2) or did (Q3) conduct variant annotation with my own data.",
    "question.list" = list(
      "Q1" = "DU06_06",
      "Q2" = "DU06_06",
      "Q3" = "DU18_04"
    )
  )
  evolutionSpecification[["usage"]][["usage_disease_risk"]] <- list(
    "title" = "I want to (Q1, Q2) or did (Q3) assess my personal disease risk.",
    "question.list" = list(
      "Q1" = "DU06_02",
      "Q2" = "DU06_02",
      "Q3" = "DU18_05"
    )
  )
  evolutionSpecification[["usage"]][["usage_ancestry"]] <- list(
    "title" = "I want to (Q1, Q2) or did (Q3) calculate my genetic ancestry.",
    "question.list" = list(
      "Q1" = "DU06_03",
      "Q2" = "DU06_03",
      "Q3" = "DU18_06"
    )
  )
  evolutionSpecification[["usage"]][["usage_wellness_traits"]] <- list(
    "title" = "I want to (Q1, Q2) or did (Q3) calculate genetic scores for wellness traits.",
    "question.list" = list(
      "Q1" = "DU06_04",
      "Q2" = "DU06_04",
      "Q3" = "DU18_07"
    )
  )
  evolutionSpecification[["usage"]][["usage_pgx"]] <- list(
    "title" = "I want to (Q1, Q2) or did (Q3) explore my pharmacogenomic markers.",
    "question.list" = list(
      "Q1" = "DU06_05",
      "Q2" = "DU06_05",
      "Q3" = "DU18_08"
    )
  )
  evolutionSpecification[["usage"]][["usage_beyond_course"]] <- list(
    "title" = "Are you planning to receive and analyze your complete data beyond the course?",
    "question.list" = list(
      "Q1" = "DU11",
      "Q2" = "DU11",
      "Q3" = "DU11"
    )
  )
  evolutionSpecification[["usage"]][["usage_well_trained"]] <- list(
    "title" = "I feel adequately trained to analyze and interpret my personal genomic data beyond the course.",
    "question.list" = list(
      "Q1" = "DU13",
      "Q2" = "DU13",
      "Q3" = "DU13"
    )
  )
  return(evolutionSpecification)
}

analyzeQuestionsNotCovered <- function() {
  evolutionSpecification <- getEvolutionSpecification()
  questionnairesInSpecification <- c()
  questionIdsInSpecification <- c()
  for (topic in names(evolutionSpecification)) {
    if (topic == "titles") {
      next
    }
    for (combination in names(evolutionSpecification[[topic]])) {
      questionList <- evolutionSpecification[[topic]][[combination]][["question.list"]]
      questionnairesInSpecification <- unique(c(questionnairesInSpecification, names(questionList)))
      questionIdsInSpecification <- unique(c(questionIdsInSpecification, unlist(questionList)))
    }
  }

  resultData <- getResultData()
  relevantResultData <- resultData[which(resultData$QUESTNNR %in% questionnairesInSpecification),]
  questionIds <- getQuestionIds(relevantResultData)
  askedQuestionsIds <- c()
  for (questionId in questionIds) {
    if (!(all(is.na(relevantResultData[, questionId])) | all(relevantResultData[, questionId] == ""))) {
      askedQuestionsIds <- c(askedQuestionsIds, questionId)
    }
  }
  
  questions <- getQuestions()
  for (askedQuestionId in askedQuestionsIds) {
    question <- questions[which(questions$id == askedQuestionId),]
    handledElsewhere <- any(startsWith(askedQuestionId, c("IO", "MN", "ES", "GC04")))
    if (!(askedQuestionId %in% questionIdsInSpecification) & !(question$type == "OPEN") & !handledElsewhere) {
      print(paste("#", askedQuestionId, "missing"))
      print(question$text)
      print(question$option)
      print("")
    }
  }
}
