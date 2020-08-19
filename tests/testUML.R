files.r = list.files("R", pattern="*\\.R$", full.names=TRUE, ignore.case=F)
sapply(files.r, source)

testData="@startuml
                    [*]-->STATE1
                 @enduml"

browser()
uml = UML$new(plantuml="C:\\SDK\\plantuml\\plantuml.jar")
uml$plot(testData)
