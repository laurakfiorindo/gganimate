library(gapminder)
library(shiny)
library(gganimate)

ui <- basicPage(
  column(
    width = 5,
    selectInput("var1","Variável eixo x:",choices=c("gdpPercap","pop","lifeExp")),
    selectInput("var2","Variável eixo y:",choices=c("gdpPercap","pop","lifeExp"))
    ),
  column(
    width=7,
    imageOutput("plot1"))
)

server <- function(input, output) {
    
        
    output$plot1 <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext='.gif')
        
        # now make the animation
        p = ggplot(gapminder, aes(x=get(input$var1),y=get(input$var2),color = continent)) +
          geom_point() + 
          scale_x_log10() +
          transition_time(year)+
          labs(title = "Ano: {frame_time}", x = input$var1,y = input$var2)  # New
        
        anim_save("outfile.gif", animate(p)) # New
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = TRUE)
    
    }

shinyApp(ui, server)
