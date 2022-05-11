#function that creates the theme of Ádám József Kovács

theme_adam <- function( base_size = 12, base_family = "", base_line_size=base_size/24,
                        base_rect_size=base_size/24,
                        border=FALSE) {
  # Inherit the basic properties of theme_bw
  theme_bw() %+replace% 
    # Replace the following items:
    theme(
      #change text elements
      plot.title = element_text(
        colour = "black",         #set color to black
        family = "Times",            #set font family to Times
        size = 15,                #set font size to 15
        face = 'bold',            #set typeface to bold
        hjust = 0,                #set title alignment to left
        vjust = 1.5),               #set vertical alignment -- little above 
      
      plot.subtitle = element_text(          #subtitle
        colour = "black",         #set color to black
        family = "Times",            #set font family to Times
        size = 12,                   #set font size to 12
        hjust = 0),                  #set subtitle alignment to left
      
      plot.caption = element_text(           #caption
        colour = "black",         #set color to black
        family = "Times",            #set font family to Times
        size = 9,                 #set font size to 9
        hjust = 1,               #set caption alignment to right
        face = 'italic'),        #set typeface to italic
      
      axis.title = element_text(             #axis titles
        colour = "black",         #set color to black
        family = "Times",            #set font family to Times
        size = 12),               #set font size to 12
      
      axis.text = element_text(              #axis text
        colour = "darkblue",         #set color to darkblue
        family = "Times",            #set font family to Times
        size = 10),                #set font size to 10
      
      #change panel elements
      panel.grid.major = element_line(color = "gray"),    #color major gridlines gray
      panel.grid.minor = element_blank(),    #strip minor gridlines
      panel.background = element_rect(fill = "#f5f5f5"), #set background color to cultured
      panel.border = element_rect(linetype = "dashed", fill = NA), 
      
      #change axis elements
      axis.ticks.x = element_line(color = "navyblue", size = 2),          #color axis ticks steelblue, set size to 2
      axis.ticks.y = element_line(color = "navyblue", size = 2),          #color axis ticks lightblue, set size to 2
      axis.ticks.length.y = unit(.1, "cm"),             #set length of ticks to 0.2
      axis.ticks.length.x = unit(.1, "cm"),
      axis.line = element_line(color = "navyblue")         #color axis lines navyblue
    )
}



