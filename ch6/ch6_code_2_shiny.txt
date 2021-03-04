# install.packages("shiny")
library(shiny)
runExample("01_hello")

# faithful(R 내장데이터)
# faithful데이터 waiting 변수를 활용해 히스토 그램을 작성
head(faithful)


# 먼저 화면에 나타나는 부분을 정의!
ui <- fluidPage( #페이지를 정의할것이다!
  
  # 앱 Title 이름 작성하기!
  titlePanel("Hello Shiny!"),
  
  # 입력과 출력 정의가 있는 공간
  sidebarLayout(
    
    # sidebarPanel(왼쪽공간)을 어떻게 넣을지 (왼쪽위에 변경할 입력 상자)
    sidebarPanel(
      
      # 입력: 어떤 내용을 넣을지를 넣는다! 
      sliderInput(inputId = "bins", #구분하는 이름값! (밑에랑 통일)
                  label = "Number of bins:",  #이름
                  min = 1, #최소값
                  max = 50, #최대값
                  value = 30) #기본으로 나탄는 값
      
    ),
    
    # 오른쪽 main 공간에 어떻게 넣을지 (오른쪽에 출력할 상장) ----
    mainPanel(
      
      # Output: 결과값을 넣는다
      plotOutput(outputId = "distPlot") #distPlot는 밑에서 정의할 시각화!
      # 히스토그램을 밑에서 만든 이름을 넣는부분!
      
    )
  )
)


server <- function(input, output) {
  
  # 히스토그램에 bin(구분 갯수)에 따라서 그림을 그려본다
  # renderPlot : 
  # 1. 반응형 이므로 입력(bin)이 변경되면 자동으로 반영
  # 2. 시각화(plot)로 출력

  output$distPlot <- renderPlot({ 
    #output내부 distPlot라는 공간에 모든 경우의 수시각화를 넣는다 
    
    x    <- faithful$waiting #x에 waiting값을 넣는다
    bins <- seq(min(x), max(x), length.out = input$bins+1) 
    #seq를 통해 모든 경우의 수를 만든다
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
}

shinyApp(ui, server)



##############################################
##############################################


# 먼저 화면에 나타나는 부분을 정의!
ui <- fluidPage( #페이지를 정의할것이다!

  # 앱 Title 이름 작성하기!
  titlePanel("Hello LG!"),

  # 입력과 출력 정의가 있는 공간
  sidebarLayout(

    # sidebarPanel(왼쪽공간)을 어떻게 넣을지 (왼쪽위에 변경할 입력 상자)
    sidebarPanel(

      # 입력: 어떤 내용을 넣을지를 넣는다!
      sliderInput(inputId = "bins", #구분하는 이름값! (밑에랑 통일)
                  label = "Number:",  #이름 (수정)
                  min = 1, #최소값 (수정)
                  max = 30, #최대값 (수정)
                  value = 20) #기본으로 나탄는 값

    ),

    # 오른쪽 main 공간에 어떻게 넣을지 (오른쪽에 출력할 상장) ----
    mainPanel(

      # Output: 결과값을 넣는다
      plotOutput(outputId = "distPlot") #distPlot는 밑에서 정의할 시각화!
      # 히스토그램을 밑에서 만든 이름을 넣는부분!

    )
  )
)


server <- function(input, output) {

  # 히스토그램에 bin(구분 갯수)에 따라서 그림을 그려본다
  # renderPlot :
  # 1. 반응형 이므로 입력(bin)이 변경되면 자동으로 반영
  # 2. 시각화(plot)로 출력

  output$distPlot <- renderPlot({
    #output내부 distPlot라는 공간에 모든 경우의 수시각화를 넣는다

    x    <- faithful$waiting #x에 waiting값을 넣는다
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #seq를 통해 모든 경우의 수를 만든다

    hist(x, breaks = bins, col = "red", border = "white",
         xlab = "rshiny exalmple test ---",
         main = "히스토그램 입니다")
  })
}

shinyApp(ui, server)

