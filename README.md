# pMiner.v046

## To install the package: 
  
```
library("remotes")
remotes::install_github("PMLiquidLab/pMineR.v046@main") 
```

If any errors about package dependencies appear, it is necessary to install the packages manually (e.g., DiagrammeR, progress, ...) and try again.

## How to use it

Cause pMineR is an ecosystem of different tools, if you are new to it, we suggest to start with use cases to get an overview and then consulting the specific documentation for each module (if necessary).


### Use cases


#### Loading/Inspecting the Event Log: the class *dataLoader*

DataLoader is perhaps the most popular class in pMineR and one of the most useful. It is the interface class for Event Logs and is used to feed all the other classes. This is a *good starting point* !

Here you can find information about

* how to load an Event Log and explore the content
* how to *filter* the Event Log or *remap* the eventusing dictionaries, to manipulate the data


http://www.pminer.info/progetti/pMineRTutorialWebsite/01.dataLoader.html

