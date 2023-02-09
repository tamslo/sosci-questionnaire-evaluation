library(networkD3)
library(htmlwidgets)
library(htmltools)
library(webshot)
# run webshot::install_phantomjs() before execution
source("modules/data/responses.R", local = TRUE)
source("modules/statistics/utils.R", local = TRUE)

get_file_path <- function(plot_path, file_ending) {
  return(paste0(plot_path, ".", file_ending))
}

get_html_path <- function(plot_path) {
  return(get_file_path(plot_path, "html"))
}

get_png_path <- function(plot_path) {
  return(get_file_path(plot_path, "png"))
}


create_sanky_plot <- function(plot_parameters) {
  canvas_height <- plot_parameters[["canvas_height"]]
  canvas_width <- plot_parameters[["canvas_width"]]
  plot_path <- plot_parameters[["plot_path"]]
  question_list <- plot_parameters[["question.list"]]
  title <- plot_parameters[["title"]]
  binary <- FALSE
  if ("binary" %in% names(plot_parameters)) {
    binary <- plot_parameters[["binary"]]
  }
  uni <- NULL
  if ("uni" %in% names(plot_parameters)) {
    uni <- plot_parameters[["uni"]]
  }
  year <- NULL
  if ("year" %in% names(plot_parameters)) {
    year <- plot_parameters[["year"]]
  }
  
  get_timepoint <- function(questionnaire) {
    # questionnaire <- str_replace_all(questionnaire, "_HPI", "")
    # timepoint <- str_replace_all(questionnaire, "Q", "T")
    # return(timepoint)
    return(questionnaire) # stop returning timepoints
  }
  
  migration_list <- list()
  for (index in 1:(length(question_list) - 1)) {
    source_questionnaire <- names(question_list)[index]
    target_questionnaire <- names(question_list)[index + 1]
    migration_list[[index]] <- c(source_questionnaire, target_questionnaire)
  }

  results <- plot_parameters[["results"]]
  if (is.null(results)) {
    results <- getResultData()
  }

  options <- plot_parameters[["options"]]
  if (is.null(options)) {
    options <- getOptionData()
  }

  questions <- plot_parameters[["questions"]]
  if (is.null(questions)) {
    questions <- getQuestions()
  }

  questionnaires <- names(question_list)
  if (!is.null(uni)) {
    results <- results[which(results$UNI == uni),]
  }
  if (!is.null(year)) {
    results <- results[which(results$COURSE_YEAR == year),]
  }
  
  # Only get complete results
  completeParticipants <- getCompleteParticipants(results, questionnaires)
  results <- results[which(results$SERIAL %in% completeParticipants),]
  
  migration_node_ids <- c()
  migration_questionnaires <- c()
  migration_participants <- c()
  migration_answers <- c()
  QUESTION_NOT_ASKED <- "Question not asked"
  for (questionnaire in questionnaires) {
    question_id <- question_list[[questionnaire]]
    questionnaire_results <- results[which(results$QUESTNNR == questionnaire),]
    migration_questionnaires <- c(migration_questionnaires, rep(questionnaire, nrow(questionnaire_results)))
    questionnaire_participants <- questionnaire_results$SERIAL
    migration_participants <- c(migration_participants, questionnaire_participants)
    question_results <- questionnaire_results[[question_id]]
    if (length(question_results) == 0) {
      print(paste0("[WARNING] Question ", question_id, " in questionnnaire ", questionnaire, "does not have results"))
      next
    }
    for (response_index in 1:length(question_results)) {
      response <- question_results[response_index]
      # WARNING: THIS IS HIGHLY SPECIFIC CODE FOR ANALYZES
      is_analysis_question <- startsWith(question_id, "DU18")
      answer_count_correction <- 0
      if (is_analysis_question) {
        # # WARNING: This is even more specific to summer term Q3; adding "Yes" based on "Other" text answer
        # yes_answer <- "I have not started yet but will perform variant annotation, calculate my genetic ancestry and maybe explore my pharmacogenomic markers"
        # add_yes_questions <- c("DU18_04", "DU18_06", "DU18_08")
        # if (question_id %in% add_yes_questions) {
        #   yes_participant <- questionnaire_results[which(questionnaire_results$DU18_01a == yes_answer),]$SERIAL[1]
        #   yes_participant_index <- which(questionnaire_participants == yes_participant)
        #   if (yes_participant_index == response_index) {
        #     response <- 2
        #   }
        #   # Remove one "No" from answer_count
        #   if (response == 1) {
        #     answer_count_correction <- - 1
        #   }
        #   # Add one "Yes" to answer_count
        #   if (response == 2) {
        #     answer_count_correction <- 1
        #   }
        # }
        # # OTHER HIGHLY SPECIFIC CODE CONTINUES
        question_options <- c("No", "Yes")
        answer <- question_options[response]

      } else {
        # SPECIFIC CODE FOR ANALYSES Q4
        # if (question_id == "Q411" && (is.na(response) || response == 2)) {
        #   response <- 2 # No
        #   answer_count_correction <- 1
        # }
        
        # NORMAL CODE STARTING HERE AGAIN
        question_options <- options[options$VAR == question_id,]
        answer <- question_options[question_options$RESPONSE == response,]$MEANING
        if (nrow(question_options) == 0 | is.na(response)) {
          answer <- QUESTION_NOT_ASKED
          answer_count_correction <- length(which(nrow(question_options) == 0 | is.na(question_results)))
        } else {
          answer <- replaceLabel(answer)
          answer <- str_replace(answer, "\n", " ")
        }
      }
      answer_count <- length(question_results[which(question_results == response)]) + answer_count_correction
      migration_node_ids <- c(migration_node_ids, paste0(answer, " (", get_timepoint(questionnaire), ", N = ", answer_count, ") "))
      migration_answers <- c(migration_answers, answer)
    }
  }
  migration_data <- data.frame(
    migration_node_ids,
    migration_questionnaires,
    migration_participants,
    migration_answers
  )
  names(migration_data) <- c("NodeID", "Questionnaire", "Participant", "Answer")

  # Order by answer value for diagram order
  # See https://stackoverflow.com/questions/52229334/fixing-the-order-of-a-sankey-flow-graph-in-r-networkd3-package
  group_values <- c()
  for (row_index in 1:nrow(migration_data)) {
    row_answer_value <- getValueGroup(migration_data$Answer[row_index])
    group_values <- c(group_values, getGroupValue(row_answer_value))
  }
  migration_data <- migration_data[order(-group_values, migration_data$Answer),]
  
  NodeID <- unique(migration_data$NodeID)
  NodeGroup <- c()
  for (node_id in NodeID) {
    node_group <- getValueGroup(node_id)
    NodeGroup <- c(NodeGroup, node_group)
  }
  
  get_node_index <- function(NodeID, nodeId) {
    r_index <- which(NodeID %in% c(nodeId))
    real_index <- r_index - 1
    return(real_index)
  }
  
  Source <- c()
  Target <- c()
  Value <- c()
  LinkGroup <- c()
  
  for (questionnaires in migration_list) {
    source_questionnaire <- questionnaires[1]
    target_questionnaire <- questionnaires[2]
    source_nodes <- migration_data[which(migration_data$Questionnaire == source_questionnaire),]
    target_nodes <- migration_data[which(migration_data$Questionnaire == target_questionnaire),]
    participants <- source_nodes$Participant
    source_node_ids <- c()
    target_node_ids <- c()
    migration_ids <- c()
    for (participant in participants) {
      source_node_id <- source_nodes[which(source_nodes$Participant == participant), "NodeID"]
      target_node_id <- target_nodes[which(target_nodes$Participant == participant), "NodeID"]
      source_node_ids <- c(source_node_ids, source_node_id)
      target_node_ids <- c(target_node_ids, target_node_id)
      migration_ids <- c(migration_ids, paste(source_node_id, ">", target_node_id))
    }
    migration_table <- data.frame(participants, source_node_ids, target_node_ids, migration_ids)
    names(migration_table) <- c("Participant", "SourceNodeID", "TargetNodeID", "MigrationID")
    for (migration_id in unique(migration_table$MigrationID)) {
      migrations <- migration_table[which(migration_table$MigrationID == migration_id),]
      source_node <- migrations$SourceNodeID[1]
      target_node <- migrations$TargetNodeID[1]
      source_node_index <- get_node_index(NodeID, source_node)
      target_node_index <- get_node_index(NodeID, target_node)
      Source <- c(Source, source_node_index)
      Target <- c(Target, target_node_index)
      Value <- c(Value, nrow(migrations))
      LinkGroup <- c(LinkGroup, getValueGroup(target_node))
    }
  }
  
  Links <- data.frame(Source, Target, Value, LinkGroup)
  Nodes <- data.frame(NodeID, NodeGroup)
  
  borderSize <- 10
  text_style <- paste0("font-family:sans-serif;margin:0;width:", canvas_width, "px;padding-left:", borderSize, "px;")
  title_style <- paste0(text_style, "padding-top:", borderSize)
  title_element <- htmltools::tags$h2(tags$span(title), style = title_style)
  titleHeight <- 25 * ceiling(nchar(title) / 85)

  get_answer_values <- function(migration_data, questionnaire) {
    participants <- unique(migration_data$Participant)
    questionnaire_answers <- migration_data[which(migration_data$Questionnaire == questionnaire),]
    values <- c()
    for (participant in participants) {
      participant_answer <- questionnaire_answers[which(questionnaire_answers$Participant == participant), "Answer"]
      node_group <- getValueGroup(participant_answer)
      value <- getGroupValue(node_group, binary)
      values <- c(values, value)
    }
    return(values)
  }
  
  format_value_information <- function(questionnaire_text, value, digits = 2, comment = "") {
    if (is.nan(value) | is.na(as.numeric(value))) {
      value <- "-"
    } else {
      value <- round(as.numeric(value), digits = digits)
    }
    return(paste0(value, " (", get_timepoint(questionnaire_text), comment, ")"))
  }
  
  mean_values <- c()
  ordered_questionnaires <- unique(migration_data[order(migration_data$Questionnaire),]$Questionnaire)
  for (questionnaire in ordered_questionnaires) {
    values <- get_answer_values(migration_data, questionnaire)
    mean_values <- c(mean_values, format_value_information(questionnaire, mean(values)))
  }
  
  titles <- c()
  comparisons <- c()
  first_question_ids <- c()
  second_question_ids <- c()
  # Run McNemar for binary questions and Wilcoxon Signed Rank for answers with more categories
  p_values <- c()
  MC_NEMAR <- "mc.nemar"
  WILCOXON <- "wilcoxon.signed.rank"
  test_methods <- c()
  effect_values <- c()
  effect_interpretation_values <- c()
  # If binary, also log Wilcoxon values to be able to compare afterwards
  other_binary_p_values <- c()
  other_binary_effect_values <- c()
  other_binary_effect_interpretation_values <- c()
  other_binary_test_methods <- c()
  values <- c()
  average_differences <- c()
  test_warnings <- c()
  other_binary_test_warnings <- c()
  EMPTY_VALUE <- "–"
  EMPTY_TEST_RESULT <- list(
    "pValue" = EMPTY_VALUE,
    "effectSize" = EMPTY_VALUE,
    "effectSizeInterpretation" = EMPTY_VALUE,
    "testWarning" = EMPTY_VALUE)
  questionnaire_comparisons <- getPairwiseComparisons(ordered_questionnaires)
  for (comparison in names(questionnaire_comparisons)) {
    first_questionnaire <- questionnaire_comparisons[[comparison]][1]
    second_questionnaire <- questionnaire_comparisons[[comparison]][2]
    first_question_id <- question_list[[first_questionnaire]]
    second_question_id <- question_list[[second_questionnaire]]
    first_responses <- get_answer_values(migration_data, first_questionnaire)
    second_responses <- get_answer_values(migration_data, second_questionnaire)
    getFormattedAverage <- function(response_values) {
      return(round(mean(response_values[which(!is.na(response_values))]), digits = 2))
    }
    getValueString <- function(questionnaire_name, response_values) {
      return(paste0(questionnaire_name, ": ", paste(response_values, collapse = ", "),
                    paste0(" (avg. ", getFormattedAverage(response_values), ")")))
    }
    values <- c(values, paste0(getValueString(first_questionnaire, first_responses), "; ",
                               getValueString(second_questionnaire, second_responses)))
    getAverageDifferences <- function(first_responses, second_responses) {
      first_average <- getFormattedAverage(first_responses)
      second_average <- getFormattedAverage(second_responses)
      return(max(first_average, second_average) - min(first_average, second_average))
    }
    average_differences <- c(average_differences, getAverageDifferences(first_responses, second_responses))
    areCurrentResponsesPresent <- !all(is.na(first_responses)) & !all(is.na(second_responses))
    if (areCurrentResponsesPresent) {
      if (isComparisonBinary(options, first_question_id, second_question_id)) {
        test_method <- MC_NEMAR
        other_binary_test_method <- WILCOXON
        test_results <- runMcNemarTest(first_responses, second_responses)
        other_binary_test_results <- runPairedWilcoxonTest(first_responses, second_responses)
      } else {
        test_method <- WILCOXON
        other_binary_test_method <- EMPTY_VALUE
        test_results <- runPairedWilcoxonTest(first_responses, second_responses)
        other_binary_test_results <- EMPTY_TEST_RESULT
      }
    } else {
      test_method <- EMPTY_VALUE
      other_binary_test_method <- EMPTY_VALUE
      test_results <- EMPTY_TEST_RESULT
      other_binary_test_results <- EMPTY_TEST_RESULT
    }
    titles <- c(titles, title)
    comparisons <- c(comparisons, comparison)
    first_question_ids <- c(first_question_ids, first_question_id)
    second_question_ids <- c(second_question_ids, second_question_id)
    p_values <- c(p_values, test_results[["pValue"]])
    test_methods <- c(test_methods, test_method)
    effect_values <- c(effect_values, test_results[["effectSize"]])
    effect_interpretation_values <- c(effect_interpretation_values, test_results[["effectSizeInterpretation"]])
    test_warnings <- c(test_warnings, test_results[["testWarning"]])
    other_binary_p_values <- c(other_binary_p_values,  other_binary_test_results[["pValue"]])
    other_binary_test_methods <- c(other_binary_test_methods, other_binary_test_method)
    other_binary_effect_values <- c(other_binary_effect_values,  other_binary_test_results[["effectSize"]])
    other_binary_effect_interpretation_values <- c(other_binary_effect_interpretation_values, other_binary_test_results[["effectSizeInterpretation"]])
    other_binary_test_warnings <- c(other_binary_test_warnings, other_binary_test_results[["testWarning"]])
  }
  q_values <- p.adjust(p_values, method = "fdr")
  formatted_p_values <- c()
  formatted_q_values <- c()
  formatted_effect_values <- c()
  for (index in 1:length(p_values)) {
    comparison <- names(questionnaire_comparisons)[index]
    p_value <- p_values[index]
    q_value <- q_values[index]
    effect_value <- effect_values[index]
    effect_interpretation_value <- effect_interpretation_values[index]
    formatted_p_values <- c(formatted_p_values,  format_value_information(comparison, p_value, digits = 3))
    formatted_q_values <- c(formatted_q_values,  format_value_information(comparison, q_value, digits = 3))
    effect_interpretation_comment <- paste0("; ", effect_interpretation_value)
    formatted_effect_values <- c(formatted_effect_values, format_value_information(comparison, effect_value, digits = 3, comment = effect_interpretation_comment))
  }
  comparison_results <- data.frame(
    titles, comparisons, first_question_ids, second_question_ids, p_values, test_methods, effect_values,
    effect_interpretation_values, values, average_differences, test_warnings, q_values,
    other_binary_p_values, other_binary_test_methods, other_binary_effect_values, other_binary_effect_interpretation_values,
    other_binary_test_warnings)
  write.csv(comparison_results, get_file_path(plot_path, "csv"), row.names = FALSE)

  font_size <- 18
  information_style <- paste0(text_style, "padding-top:", borderSize/2, "px;", "font-size:", font_size, "px;")
  mean_information <- htmltools::tags$p(paste0("Mean values: ", paste(mean_values, collapse = ", ")), style = information_style)
  significance_information <- htmltools::tags$p(paste0("P values: ", paste(formatted_p_values, collapse = ", ")), style = information_style)
  # adjusted_significance_information <- htmltools::tags$p(paste0("Adjusted P values: ", paste(formatted_q_values, collapse = ", ")), style = information_style)
  effect_information <- htmltools::tags$p(paste0("Effects: ", paste(formatted_effect_values, collapse = ", ")), style = information_style)
  information_height <- 3 * 19
  
  diagram_width <- canvas_width - 2 * borderSize
  padding_heigt <- 4 * borderSize
  diagram_height <- canvas_height - titleHeight - information_height - padding_heigt
  
  getGroupColor <- function(node_group, binary) {
    if (node_group != "other") {
      colorValue <- getGroupValue(node_group, binary) + 3
    } else {
      colorValue <- 6
    }
    return(getColor(colorValue, add_alpha = FALSE))
  }
  color_scheme <- c()
  for (node_group in unique(Nodes$NodeGroup)) {
    color_scheme <- c(color_scheme, getGroupColor(node_group, binary))
  }
  color_scheme_string <- paste0("'", paste(color_scheme, collapse = "', '"), "'")
  d3_color_scheme_string <- paste0("d3.scaleOrdinal([", color_scheme_string, "]);")
  sankey <- sankeyNetwork(Links, Nodes, Source = "Source", Target = "Target",
                          Value = "Value", NodeID = "NodeID", NodeGroup = "NodeGroup",
                          units = "student(s)", fontSize = font_size, #LinkGroup = "LinkGroup", 
                          nodeWidth = 30, colourScale = JS(d3_color_scheme_string),
                          fontFamily = "sans-serif", height = diagram_height,
                          width = diagram_width, sinksRight = TRUE, margin = borderSize,
                          iterations = 0)
  sankey <- htmlwidgets::prependContent(sankey, title_element, mean_information, significance_information, effect_information) #, adjusted_significance_information)
  html_path <- get_html_path(plot_path)
  png_path <- get_png_path(plot_path)
  saveWidget(sankey, file = html_path, title = title)
  webshot(html_path, png_path, cliprect = c(0, 0, canvas_width, canvas_height))
  return(sankey)
}
