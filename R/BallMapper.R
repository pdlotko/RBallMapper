#https://kbroman.org/pkg_primer/pages/cran.html
#Basic submission instructions: http://web.mit.edu/insong/www/pdf/rpackage_instructions.pd
#Submission webpage https://cran.r-project.org/
#CRAN policies are here https://cran.r-project.org/


library(igraph)
library(scales)
library(networkD3)
library(fields)


#'Create vertices and edges (with additional properties) of a Ball Mapper graph of the input data.
#'
#'@param points, a collection of input points in a form of a data frame. Those are typically points in Euclidean space.
#'@param values, a collection of function values on points. This variable will be used to color the Ball Mapper graph. If it is not available, please set it up to a constant array.
#'@param epsilon, the value of radius of balls used in the Ball Mapper construction.
#'@return The function return a long list of parameters that are explained below:\
#'vertices, which consist of two binded list: first one contains an increasing sequence of numbers starting from 1 to the number of vertices. Each of them correspond to a landmark point. The second one contains the number of points colvered by a ball of radius epsilon centered by following landmark points.
#'edges, a collection of not directed edges composed of the first and the second vertex. Ordering of vertices do not have meaning.
#'edges_strength, For every edge [a,b] we define its strength as the nuber of points that are both covered by landmarks a and b. This array contains the strengt of every edge in the Ball Mapper graph.
#'points_covered_by_landmarks, is a list of vectors. I-th vector contains the positions of points covered by i-th landmark.
#'landmarks, contains a list of positions of landmark points
#'coloring, is a vector having as many positions as number of lanrmarks. It contains the averaged values of points covered by each landmark.
#' @examples
#' var <- seq(from=0,to=6.3,by=0.1)
#' points <- as.data.frame( cbind( sin(var),cos(var) ) )
#' values <- as.data.frame( sin(var) )
#' epsilon <- 0.25
#' l <- BallMapper(points,values,epsilon)
BallMapper <- function( points , values , epsilon )
{
  #First we create an array of the same length as the collection of points. We will store here the numbers of landmarks that cover every given point.
  coverage <-  list()
  for ( i in 1:length(points[,1]))
  {
    coverage[[i]] <- vector()
  }
  #In this vector we will be storing the ids of landmarks:
  landmarks <- vector()
  first_uncovered <- 1
  number_of_landmark <- 1

  #Now we will be adding landmark by landmark as long as all the points are not covered.
  while ( first_uncovered <=  length(points[,1]) )
  {
    landmarks <- c(landmarks, first_uncovered)
    #Now we will check which points are covered by the first_uncovered.
    for ( j in 1:length(points[,1]) )
    {
      distance <- dist(rbind( points[j,] , points[first_uncovered,]) )
      if ( distance <= epsilon )
      {
        coverage[[j]] <- c( coverage[[j]] , number_of_landmark )
      }
    }
    while ( TRUE )
    {
      if (first_uncovered >  length(points[,1])) break
      if (length(coverage[[first_uncovered]])==0)break
      first_uncovered = first_uncovered+1
    }
    #print(paste0("first_uncovered: ", first_uncovered))
    number_of_landmark <- number_of_landmark+1
  }
  #To ballance the last additional increment.
  number_of_landmark <- number_of_landmark-1

  #Over here we compute the list of elements which are covered by the following landmarks:
  points_covered_by_landmarks <-  list()
  for ( i in 1:length(landmarks))
  {
    points_covered_by_landmarks[[i]] = vector()
  }
  for ( i in 1:length(coverage) )
  {
    for ( j in 1:length(coverage[[i]]) )
    {
      points_covered_by_landmarks[[ coverage[[i]][j] ]] <- c(points_covered_by_landmarks[[ coverage[[i]][j] ]],i)
    }
  }

  #now we create a graph. Number of vertices is the same as number_of_landmark.
  #We will create a list storing the number of points covered by each landmark.
  numer_of_covered_points = vector( length=number_of_landmark )
  for ( i in 1:length(points_covered_by_landmarks) )
  {
    numer_of_covered_points[ i ] <- 2+length(points_covered_by_landmarks[[i]])
  }

  #And for every landmark, we will consider all the points covered by it, and compute the average value of function therein.
  #It will be stored in the variable named coloring.
  coloring = vector( length=number_of_landmark )
  for ( i in 1:length(points_covered_by_landmarks) )
  {
    average_function_value <- 0
    for ( j in 1:length(points_covered_by_landmarks[[i]]) )
    {
      average_function_value <- average_function_value+values[ points_covered_by_landmarks[[i]][j], ]
    }
    average_function_value <- average_function_value/length(points_covered_by_landmarks[[i]])
    coloring[i] <- average_function_value
  }
  #I am not happy about the coloring function, as it requires some silly rescaling:
  #library(scales)
  #coloring <- rescale(coloring, to = c(0, 10000))

  #Here we create the edges with weights:
  from = vector()
  to = vector()
  for ( i in 1:length(coverage) )
  {
    for ( j in 1:length(coverage[[i]]) )
    {
      for ( k in j:length(coverage[[i]]) )
      {
        if ( j != k )
        {
          from <- c( from,coverage[[i]][j] )
          to <- c(to,coverage[[i]][k])
        }
      }
    }
  }


  #and here we create the network. Nodes are weighted by the number of points covered by them
  nodes=cbind('id'=1:number_of_landmark,size=numer_of_covered_points)
  links = cbind(from,to)

  #We may want to remove repetitions from links:
  #links <- unique(links)
  #or to use the number of repetitions as a measure of a strength of an edge ToDo
  #this part of code compute number of repetitions of edges. This number can be used
  #as the edge's weight and utylized during the visualization.
  #NOTE THAT THIS IS QUADRATIC PROCEDURE THAT SHOULD BE OPTYMIZED!!
  unique_from = vector()
  unique_to = vector()
  strength_of_edges = vector()
  was_edge_counted <- vector(  length=length(links[,1])  )
  first_not_counted_edge = 1;
  while ( first_not_counted_edge <= length(links[,1]) )
  {
    #print(paste0("Edge to consider: ", links[first_not_counted_edge,1],  " " , links[first_not_counted_edge,2]))
    number_of_repetitions_of_this_edge <- 0
    for ( i in first_not_counted_edge:length(links[,1]) )
    {
      if ( (links[i,1] == links[first_not_counted_edge,1])&(links[i,2] == links[first_not_counted_edge,2]) )
      {
        number_of_repetitions_of_this_edge <- number_of_repetitions_of_this_edge+1
        was_edge_counted[ i ] = TRUE;
      }
    }
    unique_from = c( unique_from , links[first_not_counted_edge,1]  )
    unique_to = c( unique_to  , links[first_not_counted_edge,2] )
    strength_of_edges = c( strength_of_edges , number_of_repetitions_of_this_edge )
    while ( first_not_counted_edge <= length(links[,1]) )
    {
      if ( was_edge_counted[ first_not_counted_edge ] == TRUE )
      {
        #print(paste0("first_not_counted_edge: ", first_not_counted_edge))
        first_not_counted_edge <- first_not_counted_edge+1;
      }
      else
      {
        break
      }
    }
    #print(paste0("first_not_counted_edge: ", first_not_counted_edge))
  }
  links = cbind(unique_from,unique_to)
  return_list <- list( "vertices" = nodes , "edges" = links ,
                       "edges_strength" = strength_of_edges ,
                       "points_covered_by_landmarks" = points_covered_by_landmarks,
                       "landmarks" = landmarks , "coloring" = coloring ,
                       "coverage" = coverage )

  return(return_list)
}#BallMapper


#'Produce a static color visualization of the Ball Mapper graph. It is based on the output from BallMapper function.
#'
#'@param outputFromBallMapper, an output from BallMapper function
#'@param showVertexLabels, a boolean value determining if vertex labels are to be shown (TRUE by default).
#'@param showLegend, a boolean value determining if the legend is to be shown (FALSE by default).
#'@param maximal_ball_scale, provide a maximal value of a radius of a ball used in visualization (20 by default).
#'@param minimal_ball_scale, provide a minimal value of a radius of a ball used in visualization (5 by default).
#'@param maximal_color_scale, Provide a maximal value (starting from 0) of a color of a ball (10 by default).
#'@examples Given l, the output from the BallMapper, please use
#'ColorIgraphPlot(l)
ColorIgraphPlot <- function( outputFromBallMapper, showVertexLabels = TRUE , showLegend = FALSE , minimal_ball_radius = 7 , maximal_ball_scale=20, maximal_color_scale=10 )
{
  #this command sents up a fancy background, remove if not needed.
  #par(bg="grey32", mar=c(0,0,0,0))
  vertices = l$vertices
  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])+minimal_ball_radius
  net = graph_from_data_frame(l$edges,vertices = vertices,directed = F)
#OLD
  #coloring <- l$coloring
  #coloring <- rescale(coloring, to = c(0,maximal_color_scale ))
  #V(net)$color = coloring
#NEW
  len <- length( unique(l$coloring) )
  #possible palets  rainbow( len ),
  #jet.colors <- colorRampPalette( c("red","orange","blue") )
  #jet.colors <- colorRampPalette( c("red","green") )
  jet.colors <- colorRampPalette(c("red","orange","yellow","green","cyan","blue","violet"))
  color_spectrum <- jet.colors( len )
  #and over here we map the pallete to the order of values on vertices
  ordered <- order(l$coloring)
  color <- vector(length = length(ordered),mode="double")
  for ( i in 1:length(ordered) )
  {
    color[ ordered[i] ] <- color_spectrum [ i ]
  }
  V(net)$color = color
  if ( showVertexLabels == FALSE  )V(net)$label = NA
  plot(net)
  image.plot(legend.only=T, zlim=range(l$coloring), col=color_spectrum )
}#ColorIgraphPlot


#'Produce a static grayscale visualization of the Ball Mapper graph. It is based on the output from BallMapper function.
#'
#'@param outputFromBallMapper, an output from BallMapper function
#'@param showVertexLabels, a boolean value determining if vertex labels are to be shown (TRUE by default).
#'@param maximal_ball_scale, provide a maximal value of a radius of a ball used in visualization (20 by default).
#'@examples  Given l, the output from the BallMapper, please use
#'GrayscaleIgraphPlot(l)
GrayscaleIgraphPlot <- function( outputFromBallMapper , showVertexLabels = TRUE , maximal_ball_scale=20 )
{
  vertices = l$vertices
  coloring = l$coloring

  vertices[,2] <- maximal_ball_scale*vertices[,2]/max(vertices[,2])
  coloring <- gray(rescale(l$coloring, c(0, 1)))

  net = graph_from_data_frame(l$edges,vertices = vertices,directed = F)
  V(net)$color = coloring
  if ( showVertexLabels == FALSE  )V(net)$label = NA
  #this command sents up a fancy background, remove if not needed.
  #par(bg="grey32", mar=c(0,0,0,0))
  plot(net)
}#GrayscaleIgraphPlot

#'Produce a two column list. The first column contain the number of point (possibly with repetitions), the second one contains the number of landmark points that cover it.
#'For example, let us assume that point 1 is covered by landmark 1 and 2, and point 2 is covered by the landmark 2. In this case the obtained list is of a form:
#'1 1
#'1 2
#'2 2
#'
#'@param outputFromBallMapper, an output from BallMapper function
#'@return List of landmarks covering each point, as described above.
#'@examples  Given l, the output from the BallMapper, please use
#'list <- pointToBallList(l$coverage)
pointToBallList <- function( coverageFromBallMapper )
{
  vertices <- vector()
  coveringBalls <- vector()
  for ( i in 1:length(coverageFromBallMapper) )
  {
      v <- unlist(coverageFromBallMapper[i])
      for ( j in 1:length(v) )
      {
        vertices <- c( vertices , i )
        coveringBalls <- c( coveringBalls , v[j] )
      }
  }
  return(cbind( vertices , coveringBalls ))
}

#'This is a simple example of dynamic visualization using networkD3 library.
#'This version do not implement coloring of vertices, just give a general overview of the edges.
#'@param outputFromBallMapper, an output from BallMapper function.
#' @examples Given l, the output from the BallMapper, please use
#' simpleDynamicNetwork(l)
simpleDynamicNetwork <- function(
  l #, storeAsHtml = FALSE
  )
{

  networkData <- data.frame(l$edges-1)
  sn <- simpleNetwork(networkData,zoom=T)
  show(sn)
  #if ( storeAsHtml == TRUE )saveNetwork(file = 'Net1.html')
}#simpleDynamicNetwork

#'This procedure produces a dynamic graph with colors. It allows zoom-in operation and display information on vertices when clicked on them.
#'
#'@param outputFromBallMapper, an output from BallMapper function
#'@param showLegend, if set to TRUE will display the legend indicating the coloring of the values of vertices.
#'@examples Given l, the output from the BallMapper, please use
#'coloredDynamicNetwork(l)
coloredDynamicNetwork <- function( outputOfBallMapper , showLegend = FALSE )
{
  #preparation of links
  source <- l$edges[,1]-1
  target <- l$edges[,2]-1
  value <- l$edges_strength
  links = cbind(source,target,value)
  links <- as.data.frame(links)


  #preparation of nodes
  vert <- paste('id:',as.character(l$vertices[,1]),',val:',as.character(l$coloring))
  color <- l$coloring
  nodeSize <- l$vertices[,2]

  vertices <- cbind(vert,color,nodeSize)
  vertices<- as.data.frame(vertices)

  fn <- forceNetwork(
           Links = links, Nodes = vertices,
           Source = "source", Target = "target",
           NodeID = "vert",
           Value = "value", Group = "color" ,
           opacity = 2,
           colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),#I am not sure if this change anything.
           opacityNoHover = 0,
           #clickAction = script, I do not know how to make this click thing working.
           zoom = T,
           Nodesize = "nodeSize",
           legend = showLegend
           )
  show(fn)


  #this unfortunatelly do not conrrespond to any color scheme...
  #ColorBar(sort(l$coloring),horizontal=TRUE,scale=1:length(l$coloring))
}#coloredDynamicNetwork
