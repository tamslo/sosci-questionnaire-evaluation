sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("assets/stratification", recursive = TRUE, full.names = TRUE), source)

resultData <- getResultData()
tumResults <- resultData[resultData$QUESTNNR == "TUM_Retrospective",]

# testStratificationByGenotyping("Q412")
# testStratificationByGenotyping("Q413")

#testConfounderForQuestion(tumResults, "Q417", 2, "T201") # No attitude change
#testConfounderForQuestion(tumResults, "Q417", 2, "T207") # No attitude change
#testConfounderForQuestion(tumResults, "Q417", 1, "T207") # Attitude change
#testConfounderForQuestion(tumResults, "Q418", 1, "T207") # Positive attitude change
#testConfounderForQuestion(tumResults, "Q418", 1, "T201") # Positive attitude change
#testConfounderForQuestion(tumResults, "Q418", 2, "T207") # Critical attitude change

testConfounderForQuestion(tumResults, "Q414", 1, "T207") # Genotying
testConfounderForQuestion(tumResults, "Q414", 2, "T207") # Genotying

