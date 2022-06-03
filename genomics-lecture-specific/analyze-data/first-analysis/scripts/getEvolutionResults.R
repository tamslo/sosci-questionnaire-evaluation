source("genomics-lecture-specific/analyze-data/first-analysis/scripts/analyzeEvolution.R")
source("assets/sankeyPlots/createCombinedSankeyPlots.R")

evolutionSpecification <- getEvolutionSpecification()
for (topic in names(evolutionSpecification)) {
  if (topic == "titles") {
    next
  }
  topicSpecification <- evolutionSpecification[[topic]]
  topicTitle <- evolutionSpecification[["titles"]][[topic]]
  print(paste("Creating sankey plots for ", topic, "...", sep = ""))
  createCombinedSankeyPlots(topicSpecification, topic, title = topicTitle)
}

# Motivation and Expectation
source("genomics-lecture-specific/analyze-data/first-analysis/scripts/Q1-Q3PlotMotivationAndExpectation.R")

# Get GC04 from www/...

# List what is not covered
analyzeQuestionsNotCovered()
