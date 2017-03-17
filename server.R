
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
overview <- read_excel("malevsfemale.xlsx")
male <- read_excel("men.xlsx")
female <- read_excel("women.xlsx")
black <- read_excel("Black.xlsx")

white <- read_excel("White.xlsx")
latino <- read_excel("HispanicandLatino.xlsx")

shinyServer(function(input, output) {
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 12,
    color = "black"
  )
  
   a <- list(
    title = "",
    showticklabels = TRUE,
    tickangle = 10,
    tickfont = f2,
    size =1
  )
  
  b<- list(
    title = "Count in thousands",
    showticklabels = TRUE,
    tickangle = 45,
    dtick = 250000
   
  )
  bgraph<- list(
    title = "Proportion out of 1",
    showticklabels = TRUE,
    tickangle = 45,
    dtick = 1
  )
  
  output$copyright <- renderText({
  
   return( p("Data from"), p("NSF Women, Minorities, and Persons with Disabilities in Science and Engineering Report" ,style="font-style: italic"))
    })
  

 
  

  observe(
    {
      createDataFrameBarGender <- function()
      {
        labels <- c('Job','Count')
        job <- male$job
        cols <- paste("y", input$WhenGender, sep="")
        countMale <- male[cols]
        countFemale <- female[cols]
        
        total <- countMale + countFemale
        
        countMale <- countMale / total
        countFemale <- countFemale / total

        frame2 <- data.frame(job, countMale,countFemale)
        
        
        names(frame2)[2] <- "maleCount"
        names(frame2)[3] <- "femaleCount"
        return(frame2)
      }
      
      
      
      createDataFrame <- function()
      {
        labels <- c('Job','Count')
     job <- white$Job
         cols <- paste("y", input$When, sep="")
         
        countWhite <- white[cols] 
        countBlack <- black[cols]
        countLatino <- latino[cols]
        
        total <-  countWhite + countBlack + countLatino
        
        countWhite <- countWhite/total
        countBlack <- countBlack/total
        countLatino <- countLatino/total
        frame1 <- data.frame(job, countWhite,countBlack,countLatino)
        names(frame1)[2] <- "whiteCount"
        names(frame1)[3] <- "blackCount"
        
        names(frame1)[4] <- "latinoCount"
        
        return(frame1)
      }
      
      createSummaryDF <- function()
      {
        cols <- paste("y", input$When, sep="")
        
        labels <- c('White','Black', "Hispanic")
        whiteTotal <- colSums(white[cols])
        blackTotal <-colSums(black[cols])
        hispanicTotal <-colSums(latino[cols])
        count <- c(whiteTotal,blackTotal,hispanicTotal)
        frame <- data.frame("races" = labels, "count" = count)

        return(frame)
        
      }
      
      
      createSummaryGenderDF <- function()
      {
        cols <- paste("y", input$WhenGender, sep="")
        
        labels <- c('Male','Female')
        
        
        femaleTotal <-colSums(female[cols])
        maleTotal <- colSums(male[cols])
     
        count <- c(maleTotal, femaleTotal)
        frame <- data.frame( labels,  count)
        return(frame)
        
      }
      
      ax <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      
      cols <- paste("y", input$When, sep="")

      newFrame <- createDataFrame()
      
      
      output$pie <- renderPlotly({
        plot_ly(newFrame[newFrame$whiteCount != 0,],type='bar', x=~job, y = ~whiteCount, name="White Representation in STEM"

                ) %>%
            add_trace(y=~blackCount, name="Black Representation in STEM") %>%
          add_trace(y=~latinoCount, name="Hispanic Representation in STEM") %>% 
          layout( margin = list(b=50,r=90),xaxis = a, yaxis = bgraph, showlegend = FALSE,barmode = 'stack')
      
      })    

      
      
      
      output$pieSum <- renderPlotly({
        plot_ly(createSummaryDF(),type='pie', labels=~races, values = ~count,

                 textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste( count, 'Thousand'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = T) %>%
          layout(margin = list(l = 200,r=200,a=200),xaxis = ax, yaxis = ax)
        
        
      })
      
      
      
      output$gender <- renderPlotly({
        plot_ly(createSummaryGenderDF(),type='pie', labels=~labels, values = ~count,
                
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste( count, 'Thousand'),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = T) %>%
          layout(xaxis = ax, yaxis = ax)
        
        
      })
      
      
      
      
      output$plot <- renderPlotly({
        plot_ly(createDataFrameBarGender(), x=~job, y = ~femaleCount, type='bar', name='Female')%>%
          add_trace(y = ~maleCount, name = 'Male') %>% 
          
          layout(margin = list(b=50,r=90),xaxis = a, yaxis = bgraph, showlegend = FALSE, barmode = 'stack')
        
      })
      
      createOverviewPie <- function(){

        labels <- c('White','Black', "Hispanic", "Asian", "More than one Race","American Indian or Alaska Native" ,"Native Hawaiian or Other Pacific Islander")
         whiteTotal <- colSums(overview["White"])
        blackTotal <-colSums(overview["Black or African American"])
        hispanicTotal <-colSums(overview["Hispanic or Latino"])
        asianTotal <- colSums(overview["Asian"])
        moreTotal <- colSums(overview["More than one race"])
        indianTotal <- colSums(overview["American Indian or Alaska Native"])
        nativeTotal <- colSums(overview["Native Hawaiian or Other Pacific Islander"])
        count <- c(whiteTotal,blackTotal,hispanicTotal,asianTotal,moreTotal,indianTotal,nativeTotal)
        
        frame <- data.frame("races" = labels, "count" = count)
        return(frame)
        
        }
      createOverviewData <- function(){
        
        labels <- c('Job','Count')
        job <- overview$`job`
        overview$White <- overview$White / overview$Total
        overview$`Hispanic or Latino` <- overview$`Hispanic or Latino` / overview$Total
        overview$`Asian` <- overview$`Asian` / overview$Total
        overview$`Black or African American` <- overview$`Black or African American` / overview$Total
        overview$`More than one race` <- overview$`More than one race` / overview$Total
        
        
        overview$`American Indian or Alaska Native` <- overview$`American Indian or Alaska Native` / overview$Total
        overview$`Native Hawaiian or Other Pacific Islander` <- overview$`Native Hawaiian or Other Pacific Islander` / overview$Total
        

        frame1 <- data.frame(job, overview$White,overview$`Hispanic or Latino`, overview$`Black or African American`,overview$`Asian`,overview$`American Indian or Alaska Native`,overview$`More than one race`,   overview$`Native Hawaiian or Other Pacific Islander`)
         names(frame1)[2] <- "whiteCount"
        names(frame1)[3] <- "latinoCount"
        
        names(frame1)[4] <- "blackCount"
        names(frame1)[5] <- "asianCount"
        names(frame1)[6] <- "nativeCount"
        names(frame1)[7] <- "moreCount"
        names(frame1)[8] <- "islandCount"

        return(frame1)
      }
      
      output$overviewBar <- renderPlotly({
        plot_ly(createOverviewData(),type='bar', x=~job, y = ~whiteCount, name="White Representation in STEM"
                
        ) %>%
          add_trace(y=~asianCount, name="Asian Representation in STEM") %>% 
          add_trace(y=~blackCount, name="Black Representation in STEM") %>%
          add_trace(y=~latinoCount, name="Hispanic Representation in STEM") %>% 
          add_trace(y=~moreCount, name="More than one Race Representation in STEM") %>% 
          add_trace(y=~nativeCount, name="American Indian or Alaska Native Representation in STEM") %>% 
          add_trace(y=~islandCount, name="Native Hawaiian or Other Pacific Islander Representation in STEM") %>%
          layout(margin = list(b=50,r=50),xaxis = a, yaxis = bgraph, showlegend = FALSE,barmode = 'stack')
        
      })
      
      output$overviewSum<- renderPlotly({
        a<- plot_ly(createOverviewPie(),type='pie', labels=~races, values = ~count,
                
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = T) %>%
          layout(margin = list(l = 200,r=200,a=200), legend = list(x = 0.1, y = 4), xaxis = ax, yaxis = ax)
        
      
        
      })

    })

})
