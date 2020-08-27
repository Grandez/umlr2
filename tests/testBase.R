source("testClasses.R")
testData="@startuml\n[*]-->STATE1\n@enduml\n"

source("../R/R6Messages.R")
source("../R/R6Config.R")
source("../R/R6UMLR2Base.R")

sapply(list.files("../R", "\\.R$", full.names = TRUE), source)
config = CONFIG$new(plantuml   = "c:/SDK/plantuml/plantuml.jar"
                    ,inputDir  = "uml"
                    ,outputDir = "img"
                    )
