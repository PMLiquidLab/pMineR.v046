#' CFM: load a dataset
#'
#' @description  Load a dataLoader$getData() structure
#' 
#' @param objCFM the \code{careFlowMiner} obj.
#' @param inputData the data to load formattes as a dataLoader$getData() structure
#' @param dateToColumnName the name of the column containing the optional \code{dateTo} of the clinical event
#' @param dateToFormat the format of the optional \code{dateTo} ( this because \code{dateFrom} and \code{dateTo} can have different formats)
#' @export
#' @examples \dontrun{
#' 
#' # instantiate a dataLoader object
#' library(pMineR)
#' 
#' tmp.objDL <- dataLoader()
#' invisible(tmp.objDL$load.data.frame( mydata = data.frame(myInputCSVData) , IDName = "PatientID",EVENTName = "clinicalEvent",
#'  dateColumnName = "STRT_DT", format.column.date = "%Y-%m-%d %H:%M:%S" ))
#'  DLS <- tmp.objDL$getData()
#'
#' a <- careFlowMiner()
#' a$loadDataset(DLS = DLS , dateToColumnName = "END_DT",dateToFormat="%Y-%m-%d %H:%M:%S") 
#' 
#' }
CFM.loadDataset <- function( objCFM, inputData , dateToColumnName=NA , dateToFormat = "" ) { 
  objCFM$loadDataset( inputData = inputData , dateToColumnName = dateToColumnName , dateToFormat = dateToFormat)
}

#' CFM: plot the graph with the stories
#'
#' @description  Plot the graph containing the stories
#' 
#' @param objCFM the \code{careFlowMiner} obj.
#' @param depth the depht of the final graph, with the respect of the initial node
#' @param starting.ID the starting node (the default is the \code{root} node )
#' @param kindOfGraph the shape of the graph. Can be \code{twopi} (by default), \code{neato} or \code{circo}
#' @param GraphFontsize set to 9 by default
#' @param withPercentages default = \code{TRUE}: do you want percentages, on the arcs?
#' @param relative.percentages default = \code{FALSE}. If the parameter \code{withPercentages} is set to TRUE this define the kind of percentages the graph will present on the edges
#' @param proportionalPenwidth default = \code{TRUE}. Do the bolder attribute has to be proportional with the percentage?
#' @param default.arcColor default = \code{black},  define the default color for the arcs
#' @param arr.States.color an array by which you can specify for some nodes (key) the associated color(value). E.g. c("Home"="Black","Hospital"="Red")
#' @param predictive.model default = \code{FALSE}. If set to \code{TRUE} the entire graph will work as a predictive mode, with the probability on each node to reach one or mode final states (indicated in \code{predictive.model.outcome})
#' @param predictive.model.outcome the array of nodes which represent the goals in predicting the evolution. This makes sense if \code{predictive.model} is set to \code{TRUE}
#' @param predictive.model.skipNodeLabel Shit, I forgot the meaning of this parameter
#' @param preserve.topology default = \code{FALSE}. If set to YES it will preserve the  topology of the graph even if you change some parameters, by plotting all the nodes, in any case (however, the not useful ones will painted in a softgrey color)
#' @param set.to.gray default = \code{FALSE} I don't remember
#' @param set.to.gray.color default = \code{WhiteSmoke} the default color for the nodes not used in coputation but only aimed at preserving the topology
#' 
#' @export
#' @examples \dontrun{
#' 
#' # instantiate a dataLoader object
#' library(pMineR)
#' 
#'  tmp.objDL <- dataLoader()
#'  invisible(tmp.objDL$load.data.frame( mydata = data.frame(myInputCSVData) , IDName = "PatientID",EVENTName = "clinicalEvent",
#'   dateColumnName = "STRT_DT", format.column.date = "%Y-%m-%d %H:%M:%S" ))
#'  DLS <- tmp.objDL$getData()
#'
#' a <- careFlowMiner()
#' a$loadDataset(DLS = DLS , dateToColumnName = "END_DT",dateToFormat="%Y-%m-%d %H:%M:%S") 
#'
#' b <- CFM.plotCFGraph(objCFM = a, depth = 4,kindOfGraph = "neato",default.arcColor = "Red", 
#'               arr.States.color=c("Deces"="Red","intensive care"="Orange","Recovered"="YellowGreen"),
#'               predictive.model = TRUE, predictive.model.outcome = "Deces", 
#'               predictive.model.skipNodeLabel = c("Deces","Recovered"), preserve.topology = TRUE )
#' grViz(b$script)
#' }
CFM.plotCFGraph <- function(  objCFM , depth= 2 , starting.ID = "root",
                          kindOfGraph = "twopi", GraphFontsize = "9" ,
                          withPercentages = TRUE, relative.percentages = FALSE, 
                          proportionalPenwidth=TRUE , default.arcColor = "Black",
                          arr.States.color=c(),
                          predictive.model = FALSE, predictive.model.outcome = "", predictive.model.skipNodeLabel = c(),
                          preserve.topology = FALSE, set.to.gray = FALSE, set.to.gray.color= "WhiteSmoke") {
  
  b <- objCFM$plotCFGraph(  depth = depth , starting.ID = starting.ID,
                            kindOfGraph = kindOfGraph, GraphFontsize = GraphFontsize ,
                            withPercentages = withPercentages, relative.percentages = relative.percentages, 
                            proportionalPenwidth=proportionalPenwidth , default.arcColor = default.arcColor,
                            arr.States.color=arr.States.color,
                            predictive.model = predictive.model, predictive.model.outcome = predictive.model.outcome, 
                            predictive.model.skipNodeLabel = predictive.model.skipNodeLabel,
                            preserve.topology = preserve.topology, set.to.gray = set.to.gray, 
                            set.to.gray.color= set.to.gray.color)
  return(b$script)
  
}


#' CFM: Compare two Graphs
#'
#' @description  Compare two graphs, stratyfing for a given covariate
#' 
#' @param objCFM the \code{careFlowMiner} obj.
#' @param stratifyFor the dicotomic attribute you want to use to stratify. It should be the same for all the record of a specific patients (it is a baseline attribute).
#' @param depth max depth level (default = 4)
#' @param fisher.threshold bla bla bla
#' @param arr.States.color an array contaning the association between nodes and color
#' @export
CFM.plotCFGraphComparison <- function( objCFM, stratifyFor , stratificationValues, depth = 4, fisher.threshold = 0.1,
                                       arr.States.color=c() ) { 
  b <- objCFM$plotCFGraphComparison( stratifyFor = stratifyFor, stratificationValues = stratificationValues, depth = depth,
                                     fisher.threshold = fisher.threshold, arr.States.color = arr.States.color)
  return( b$script )
}

#' CFM: pack start and end nodes and plot the paths
#'
#' @description  stack two given nodes, respectively start and end, and show all the paths between them. It also allow to stratify for a given covariate (optional)
#' 
#' @param objCFM the \code{careFlowMiner} obj.
#' @export
CFM.pathBeetweenStackedNodes <- function( objCFM ,fromState , toState, stratifyFor = "" , minPath = FALSE, stratificationValues=c() , fisher.threshold = 0.1,
                                          kindOfGraph = "dot", arcColor = "black", arc.fontsize = 10, arc.fontcolor = "red",
                                          arr.States.color=c(), set.to.gray.color= "WhiteSmoke", p.value.threshold = 0.05 ) { 
  
  b <- objCFM$pathBeetweenStackedNodes( fromState = fromState , toState = toState, stratifyFor = stratifyFor ,
                                        minPath = minPath, stratificationValues = stratificationValues, 
                                        fisher.threshold = fisher.threshold, kindOfGraph= kindOfGraph,
                                        arcColor = arcColor, arc.fontsize = arc.fontsize, arc.fontcolor = arc.fontcolor,
                                        arr.States.color=arr.States.color, set.to.gray.color= set.to.gray.color, 
                                        p.value.threshold = p.value.threshold )
  return( b )
}

