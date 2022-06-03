sapply(list.files("modules", recursive = TRUE, full.names = TRUE), source)
sapply(list.files("scripts", recursive = TRUE, full.names = TRUE), source)

results <- getResultData()
options <- getOptionData()


print("One participant did not conduct WGS at T1 but at T2 and did not answer T3.")
print("")

print("What did the student who answered 'I don't know' in DU02 (Q1) answer in DU14 (Q2)?")
participant <- results[which(results$DU02 == 5),"SERIAL"]
participantResults <- results[results$SERIAL == participant,]
participantAnswer <- participantResults[participantResults$QUESTNNR == "Q2",]$DU14
questionOptions <- options[options$VAR == "DU14",]
answerOption <- questionOptions[questionOptions$RESPONSE == participantAnswer,]$MEANING
print(answerOption)
print("")

print("The student who answered 'I don't know' in DU02 (Q1) did not finish Q3.")
print(!("Q3" %in% participantResults$QUESTNNR))
print("")

print("In which questionniaires did the student who answered 'I prefer not to say' in DU14 (Q2) participate?")
participant <- results[which(results$DU14 == 3), "SERIAL"]
participantResults <- results[results$SERIAL == participant,]
print(participantResults$QUESTNNR)
print("")

print("What did the student who answered 'I prefer not to say' in DU14 (Q2) answer in DU02 (Q1)?")
participantAnswer <- participantResults[which(participantResults$QUESTNNR == "Q1"), "DU02"]
questionOptions <- options[options$VAR == "DU02",]
print(questionOptions[questionOptions$RESPONSE == participantAnswer,]$MEANING)
print("")
