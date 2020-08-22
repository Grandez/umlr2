files.r = list.files("R", pattern="*\\.R$", full.names=TRUE, ignore.case=F)
invisible(sapply(files.r, source))

source("vignettes/sampleClasses.R")

testData="@startuml\n[*]-->STATE1\n@enduml\n"
print("Inicio")

umlr = UMLR$new(plantuml="C:\\SDK\\plantuml\\plantuml.jar")

base = UMLShow$complete + UMLShow$subclasses + UMLShow$parents + UMLShow$classComplete
# umlr$plotClass(33, base)


#rc = uml$checkInstallation()


## monochrome
print("Antes")

# umlr$addStyle("monochrome", TRUE)
umlr$addStyle("monochrome", TRUE)
umlr$plotClass(c10, base)
