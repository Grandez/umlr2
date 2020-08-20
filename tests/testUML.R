files.r = list.files("R", pattern="*\\.R$", full.names=TRUE, ignore.case=F)
sapply(files.r, source)

testData="@startuml
                    [*]-->STATE1
                 @enduml"

uml = UML$new(plantuml="C:\\SDK\\plantuml\\plantuml.jar")
#rc = uml$checkInstallation()


uml$plot(testData)

browser()
uml = UML$new(plantuml="C:\\SDK\\plantuml\\plantuml.jar", inputDir="uml", outputDir="img")
uml$link(testData, "example", "prueba")
