library(stringr)
sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)

colors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
lwd <- 1.5
legendSpace <- 0.05
legendBorder <- "n"

# # Self-reported Knowledge
# 
# concepts <- list(
#   "Variant" = c(1.8, 2, 2),
#   "Genotyping" = c(1.6, 2, 2),
#   "Array technology" = c(0.6, 0.9, 1.4),
#   "WGS" = c(1.5, 1.5, 1.5),
#   "Difference btw.\narray tech. & WGS" = c(0.6, 0.8, 1.8)
# )
# 
# title <- "Self-reported comprehension of concept (all answers)"
# timepoints <- c("T1", "T2", "T3")
# numericTimepoints <- c(1:length(timepoints))
# 
# png(file="www/analysis/reported_knowledge_means.png", width = 750, height = 500)
# par(oma=c(0, 1, 0, 9.5))
# plot(
#   numericTimepoints, type = "n",
#   ylim = c(0.5, 2), ylab = "Understanding [-2, 2]",
#   xaxt = "n", xlab = ""
# )
# for (index in c(1:length(concepts))) {
#   item <- concepts[[index]]
#   points(numericTimepoints, item, col = colors[index], pch = index)
#   lines(numericTimepoints, item, col = colors[index], lty = index, lwd = lwd)
# }
# axis(1, at = 1:3, labels = timepoints)
# legend(par("usr")[2] + legendSpace, par("usr")[4], xpd = NA, bty = legendBorder, legend = names(concepts), col = colors[1:length(concepts)], lty = c(1:length(concepts)), pch =c(1:length(concepts)), lwd = lwd)
# dev.off()

# Motivation and Expectation

question_list <- list(
  "MN02_01" = "Receiving my own genomic data",
  "MN02_02" = "Learn about genome analysis",
  "MN02_03" = "Analyzing my own genomic data",
  "MN02_04" = "Getting credit points",
  "MN02_05" = "The course sounded interesting",
  "ES02_01" = "Learn about GWAS",
  "ES02_02" = "Learn about ethical implications of genome analysis",
  "ES02_03" = "Learn about legal foundation of genome analysis in Germany",
  "ES02_04" = "Learn about the comparison of legal foundations of genome analysis between countries",
  "ES02_05" = "Learn about genotyping in general",
  "ES02_06" = "Learn about sequencing in general",
  "ES02_07" = "Learn about social implications of genome analysis",
  "ES02_08" = "harmacogenomics",
  "ES02_09" = "Ancestry analysis",
  "ES02_10" = "Hands-on work with genetic data",
  "ES02_11" = "Options for direct to consumer testing",
  "ES02_12" = "Analyzing wellness traits",
  "ES02_13" = "Tools to analyze and interpret (my own) genomic data"
)

questions <- getQuestions()
responses <- list()
labels <- c()
means <- c()
for (question_id in names(question_list)) {
  motivation_question <-  questions[which(questions$id == question_id),]
  question_responses = getResponses(motivation_question, "Q1", FALSE)
  transformed_responses = c()
  for (response_row in 1:nrow(question_responses)) {
    value = question_responses[response_row, "value"]
    count = question_responses[response_row, "count"]
    if (value > 0 && count > 0) {
      value <- normalizeValue(value, question_responses)
      for (index in 1:count) {
        transformed_responses <- c(transformed_responses, value)
      }
    }
  }
  responses[[motivation_question$id]] = transformed_responses
  labels <- c(labels, breakText(question_list[[question_id]], 60))
  means <- c(means, mean(transformed_responses))
}
orderedResponses <- order(means, decreasing = TRUE)
responses <- responses[orderedResponses]
labels <- labels[orderedResponses]

png(file=file.path(getResultDirectory(), "motivation_and_expectation.png"), width = 900, height = 500)
par(oma=c(1,1,1,1))
par(mar=c(2,23.5,1,1.5))
boxplot(responses, names = labels, horizontal = TRUE, las = 2, at=rev(1:length(responses)), xaxt = "n")
grid(nx = 5, ny = length(orderedResponses))
axis(1, labels = FALSE)
text(-2:2, par("usr")[3]- 1, srt = 0,
     labels = c("Definitely no", "Probably no", "Neutral", "Probably yes", "Definitely yes"), xpd = TRUE)
dev.off()

