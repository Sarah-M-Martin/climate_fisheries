
library(fmsb)

# example plot

radarchart( nut_by_habitat [c(1:2,6),] , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) ,  # line colour
            pfcol=rgb(0.2,0.5,0.5,0.5) , # fill colour
            plwd=4 ,                    # line width
            
            #custom the grid
            cglcol="grey",     # net colour
            cglty=1,           # net line type
            axislabcol="grey", # colour of axis labels
            caxislabels=seq(0,20,5), # vector of axis labels
            cglwd=0.8,              # net width 
            
            #custom labels
            vlcex=0.8,   # group labels size
            title =  "Habitat 4"
)

####################################################################################################
# set up plotting function 
####################################################################################################

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 1,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}
