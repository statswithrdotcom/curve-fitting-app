
library(shiny)
library(shinythemes)

Plot_Fun <- function(N, Degree, Intercept, First_Degree, Second_Degree, Third_Degree, Fourth_Degree, Fifth_Degree){
  
  X = seq(-5,5,length.out=N)
  
  Y = Fifth_Degree*X^5 + Fourth_Degree*X^4 + Third_Degree*X^3 + Second_Degree*X^2 + First_Degree*X + Intercept
  
  Fit_Data <- matrix(0,ncol=Degree,nrow=N)
  for(j in 1:N){
    for (i in 1:Degree) {
      Fit_Data[j,i] = X[j]^i
    }
  }
 suppressWarnings({ Fit <- summary(lm(Y ~ Fit_Data)) })
  Pred <- Y - Fit$residuals
  
  return({
    plot(X,Y, main = "Curve Fitting Plot", xlab = "X variable", ylab = "Y variable", xaxt = "n")
    axis(1, at = seq(-5,5,by=1))
    lines(X,Pred, lwd = 2, col = "blue")
  })
  
}

ui <- fluidPage(theme = shinytheme("cosmo"),
                navbarPage(
                'Curve Creating and Fitting Demo',
                
                sidebarPanel(
                  sliderInput("SampleN","Input Sample Size", min = 2, max = 100, value = 50),
                  sliderInput("Degrees","Input the Degrees of Fit", min = 1, max = 5, value = 1),
                  sliderInput("Int", "Input the line intercept", min = -5 , max = 5, value = 0, step = 0.1),
                  sliderInput("First", "Input the coefficient C1", min = -5, max = 5, value = 0, step = 0.1),
                  sliderInput("Second", "Input the coefficent C2", min = -5, max = 5, value = 0, step = 0.1),
                  sliderInput("Third", "Input the coefficent C3", min = -5, max = 5, value = 0, step = 0.1),
                  sliderInput("Fourth", "Input the coefficent C4", min = -5, max = 5, value = 0, step = 0.1),
                  sliderInput("Fifth", "Input the coefficent C5", min = -5, max = 5, value = 0, step = 0.1)
                  
                ),
                
                
                mainPanel(
                    h1("Resulting Plot"),
                    plotOutput("plot"),
                    h4("Y = Intercept + C1*X + C2*X^2 + C3*X^3 + C4*X^4 + C5*X^5"),
                    h4("Made by statswithr.com")
                )
                )
)
                

server <- function(input, output) {
                  
                output$plot <- renderPlot({Plot_Fun(N = input$SampleN, Degree = input$Degrees, 
                                 Intercept = input$Int, First_Degree = input$First, Second_Degree = input$Second,
                                 Third_Degree = input$Third, Fourth_Degree = input$Fourth, Fifth_Degree = input$Fifth)})
                  
}
                

shinyApp(ui = ui, server = server)
                
                
