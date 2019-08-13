
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("Variables.R")
source("Plot.R")
library(shiny)

server <- function(input, output){
  
  output$network <-  renderPlot({
    
    ######################
    #gene input
    selectedgene <- input$genename

    edgeGene = edge[edge$From == selectedgene | edge$To == selectedgene,]
    
    
    
    min <- input$num
    
    # slider input
    for(i in 1:nrow(nodecount))
    {
      if(nodecount[i, 2] < min)
       {
         thisgene <- toString(nodecount[i,1])
     
         #edge <- edge[!(edge$From==thisgene) & (edge$To==thisgene),]
     
         #subset(theedges, From==thisgene & To==thisgene)
     
          for(k in 1:nrow(edge))
          {
            if(toString(edge[k, 1]) == thisgene || toString(edge[k, 2]) == thisgene)
            {
              edge[[k,1]] <- NA
              edge[[k,2]] <- NA
              edge[[k,3]] <- NA
              edge[[k,4]] <- NA
            }
          }
         node[i, 1] <- NA
       }
     }
     node <- na.omit(node)
     edge <- na.omit(edge)

    

    ####################

    # tickbox input
    if(input$ER == T)
    {
      edgeER = edge[edge$Edge.Color == "green" | edge$Edge.Color == "yellow" & (edge$From == selectedgene | edge$To == selectedgene),]  
    }
    else if(input$ER == F & input$p53 == F & input$Grade == F)
    {
      edgeER = edge[edge$Edge.Color == "green" | edge$Edge.Color == "yellow" & (edge$From == selectedgene | edge$To == selectedgene),]
      edgep53 = edge[edge$Edge.Color == "blue" | edge$Edge.Color == "purple" & (edge$From == selectedgene | edge$To == selectedgene),]
      edgeGrade = edge[edge$Edge.Color == "red" | edge$Edge.Color == "black" & (edge$From == selectedgene | edge$To == selectedgene),]
      
    }
    else
    {
      edgeER = data.frame()
    }
    if(input$p53 == T)
    {
      edgep53 = edge[(edge$Edge.Color == "blue" | edge$Edge.Color == "purple") & (edge$From == selectedgene | edge$To == selectedgene),]
    }
    else if(input$ER == F & input$p53 == F & input$Grade == F)
    {
      edgeER = edge[(edge$Edge.Color == "green" | edge$Edge.Color == "yellow") & (edge$From == selectedgene | edge$To == selectedgene),]
      edgep53 = edge[(edge$Edge.Color == "blue" | edge$Edge.Color == "purple") & (edge$From == selectedgene | edge$To == selectedgene),]
      edgeGrade = edge[(edge$Edge.Color == "red" | edge$Edge.Color == "black") & (edge$From == selectedgene | edge$To == selectedgene),]
    }
    else
    {
      edgep53 = data.frame()
    }
    if(input$Grade == T)
    {
      edgeGrade = edge[(edge$Edge.Color == "red" | edge$Edge.Color == "black") & (edge$From == selectedgene | edge$To == selectedgene),]
    }
    else if(input$ER == F & input$p53 == F & input$Grade == F)
    {
      edgeER = edge[(edge$Edge.Color == "green" | edge$Edge.Color == "yellow") & (edge$From == selectedgene | edge$To == selectedgene),]
      edgep53 = edge[(edge$Edge.Color == "blue" | edge$Edge.Color == "purple") & (edge$From == selectedgene | edge$To == selectedgene),]
      edgeGrade = edge[(edge$Edge.Color == "red" | edge$Edge.Color == "black") & (edge$From == selectedgene | edge$To == selectedgene),]
    }
    else
    {
      edgeGrade = data.frame()
    }

    combo_plot = rbind(edgeER,edgep53,edgeGrade)
    
    
    net <- graph_from_data_frame(d = combo_plot, directed = FALSE, vertices = node)
    
    
    
    ######################
    
    lay <- layout_with_kk(net)
    plot(net, vertex.label.cex=.7, vertex.size=10, layout = lay, edge.color=E(net)$Edge.Color)
    
  })
}




#VList <- c(neighbors(net, input$num))

#condensedNet <- induced_subgraph(net, VList)
#newnet <- graph_from_data_frame(d = newedge, directed = FALSE, vertices = node)

#plot(net, vertex.label.cex=.5, vertex.size=6, edge.color=E(condensedNet)$Edge.Color)
