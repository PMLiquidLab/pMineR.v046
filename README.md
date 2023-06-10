# pMiner.v046

## To install the package: 
  
```
library("remotes")
remotes::install_github("PMLiquidLab/pMineR.v046@main") 
```

If any errors about package dependencies appear, it is necessary to install the packages manually (e.g., DiagrammeR, progress, ...) and try again.

## How to use it

Cause pMineR is an ecosystem of different tools, if you are new to it, we suggest to start reading the documentation in order, starting with the class *dataLoader()*. For a general overview, here there is a list of the most important classes:

* Event Log Loading/Inspection/Manipulation : **dataLoader()**
* Event Log Inspection/Querying : **QOD()**
* Process Discovery : **firstOrderMarkovModel()**, **careFlowMiner()**
* Conformance Checking/Computer Interpretable Clinical Guidelines : **confCheck_easy()**


### 01 - dataLoader : Loading and Inspecting the Event Log

*DataLoader()* is perhaps the most popular class in pMineR and one of the most useful. It is the interface class for Event Logs and is used to feed all the other classes. This is a *good starting point* !

Here you can find information about

* how to load an Event Log and explore the content
* how to *filter* the Event Log or *remap* the eventusing dictionaries, to manipulate the data


http://www.pminer.info/progetti/pMineRTutorialWebsite/01.dataLoader.html


### 02 - QOD : Checking/Querying the Data for Quality of Data Assessment

The class *QOD()* is another useful class. It was born to support Quality of Data Analysis but it is also helppfull for Data inspection and data querying/transformation.

Here you can find an overview about the most significant features. The example exploits a dataset provided in the following link:

dataset description : http://www.pminer.info/progetti/pMineRTutorialWebsite/QOD/QOD.testing.Dataset.Description.pdf

dataset : http://www.pminer.info/progetti/pMineRTutorialWebsite/QOD/EL_CFM_Demo.csv

*QOD()* overview : http://www.pminer.info/progetti/pMineRTutorialWebsite/QOD/QOD_tutorial.html

### 03 - FOMM : Process Discovery via First Order Markov Model 

The class *firstOrderMarkovModel()* is class for Process Discovery (probably the simplest).

Here is an R Markdown that explains how to take the first steps

http://www.pminer.info/progetti/pMineRTutorialWebsite/FOMM/FOMM.html

### 05 - PWL: Conformance Checking and Computer Interpretable Guidelines – part 1

A *Hello World* with PWL, a formalism born for representing simple Clinical Guidelines and make Conformance Checking.

csv for test : http://www.pminer.info/progetti/pMineRTutorialWebsite/PWL.1/csv/one.overview.csv
XML with the process : http://www.pminer.info/progetti/pMineRTutorialWebsite/PWL.1/XML-PWL/one.overview.xml
Overview : http://www.pminer.info/progetti/pMineRTutorialWebsite/PWL.1/PWL.1.html

### 06 - PWL: Conformance Checking and Computer Interpretable Guidelines – part 1 

A more complex Example with PWL :

csv for test : http://www.pminer.info/progetti/pMineRTutorialWebsite/PWL.2/csv/four.generated.csv
XML with the process : http://www.pminer.info/progetti/pMineRTutorialWebsite/PWL.2/XML-PWL/four.overview.xml
Overview : http://www.pminer.info/progetti/pMineRTutorialWebsite/PWL.2/PWL.2.html
