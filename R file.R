#installing the libraries we need
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("writexl")
install.packages("ggplot2")
install.packages("arules")
install.packages("openxlsx")
install.packages("shiny")
library(dplyr)
library(shiny)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(arules)
library(readxl)
data<-read.csv("C:/Users/Victus/AppData/Roaming/Microsoft/Windows/Network Shortcuts/csv.csv")
head(data)
str(data)
summary(data)

#check if there are a missing values
colSums(is.na(data))
which(is.na(data))

#there are no missing values
numeric_data<-data[,sapply(data,is.numeric)]
boxplot(numeric_data,main="boxblot for outliers")

#deleting outliers
outlier<-boxplot(data$count)$out
data[which(data$count%in%outlier),]
cleaned_data<-data[-which(data$count%in%outlier),]
print(cleaned_data)
outlier<-boxplot(cleaned_data$count)$out

#deleting duplicated values
duplicated(cleaned_data)
sum(duplicated(cleaned_data))
library("dplyr")
cleaned_data=distinct(cleaned_data)
sum(duplicated(cleaned_data))

#check data structure
is.character(cleaned_data$items)
is.integer(cleaned_data$count)
is.integer(cleaned_data$total)
is.integer(cleaned_data$rnd)
is.character(cleaned_data$customer)
is.integer(cleaned_data$age)
is.character(cleaned_data$city)
is.character(cleaned_data$paymentType)


par(mfrow = c(2, 2))

# Compare cash and credit totals
paymentType_summary <- aggregate(total ~ paymentType, data = cleaned_data, FUN = sum)
x <- paymentType_summary$total
# show percentage
percentage <- paste0(round(100 * x / sum(x), 2), "%")
pie(x,labels = percentage,      
    main = "Compare paymentType by count", col=c("grey", "maroon4"))
# add more information by using legend
legend("bottomright", legend = c("cash", "credit"), fill = c("grey","maroon4"))


# Total spending of each city
age_summary <- aggregate(total ~ age , data = cleaned_data, FUN = sum)
# Total spending of each city
plot(x = age_summary$age,
     y = age_summary$total, 
     main = "total spending vs age", 
     xlab = "age", 
     ylab = "total spending", 
     col="red" ,
)


# Total spending of each city 
city_summary <- aggregate(total ~ city , data = cleaned_data, FUN = sum)
#arrange each city total spending by total descending
city_summary<-arrange(city_summary,desc(total))

barplot(
  height = city_summary$total,
  names = city_summary$city,
  xlab = "city",
  ylab = "total",
  main = "Total spending of each city",
  col = "pink",
)   

#the distribution of total spending by using a histogram 
boxplot(cleaned_data$total,main="Distribution of total spending",xlab="total" , col="maroon")

# take data from user and read the dataset file
Dataset_Path <- readline(prompt = "Enter the full path of the dataset file: ")
customer_data <- read_excel(Dataset_Path)
# choose columns that i will work with first to do group by()
MYDATA <- data[, c("customer", "age", "total")]
# collect columns customer,age and calculate the total spending
Grouping <- MYDATA %>%
  group_by(customer, age) %>%
  summarise(TotalSpending = sum(total, na.rm = TRUE))  # ignore missing values
input_k <- FALSE
while (input_k==FALSE) {
  #ask user to enter k
  Number_Of_Clusters <- as.integer(readline(prompt = "Enter the number of clusters (between 2 and 4): "))
  if (Number_Of_Clusters >= 2 && Number_Of_Clusters <= 4) {
    input_k<- TRUE
    break  # Input is valid, exit the loop
    cat("You entered a valid number of clusters:", k, "\n")
  } else {
    cat("Invalid input. Please enter a number between 2 and 4.\n")
  }
}

# select data that we do kmeans algorithm with it(Age و TotalSpending)
group_ta <- Grouping[, c("age", "TotalSpending")]
# apply kmeans algorithm
kmeans_result <- kmeans(group_ta, centers = Number_Of_Clusters, nstart = 25)
# add kmeans result to data
Grouping$kmeans_result <- kmeans_result$cluster
#print table of customer , age , totalspending and kmeans_result
cat("\nCustomer clustering results:\n")
print(Grouping)
# save data to new excel file
output_path <- readline(prompt = "Enter the full path to save the new Excel file: ")
write_xlsx(Grouping, path = output_path)
print("The data with K-means results has been saved successfully.")


apriori <-  cleaned_data$items
Apriori=strsplit(apriori, ",")
transactions <- as(Apriori, "transactions")
#calculate support and confidence 
Min_Support<-as.numeric(readline("input support(0.001:1):"))  
Min_Confidence<-as.numeric(readline("input coinfidence(0.001:1):"))

if((Min_Support >= 0.001 && Min_Support <= 1) && 
   (Min_Confidence >= 0.001 && Min_Confidence <= 1)
   && !is.na(Min_Support) && !is.na(Min_Confidence)) {
  inspect(transactions)
  apriori_rules <- apriori(transactions, parameter = list(supp = Min_Support, conf =  Min_Confidence, minlen=2))
  apr=inspect(apriori_rules)
}else{
  print("There are an error")
}
# User Interface (UI)
ui <- fluidPage(
  
  # Application Title
  titlePanel("Data K-Means , Apriori and City Analysis GUI"),
  
  # Sidebar layout with input controls and main content area
  sidebarLayout(
    
    # Sidebar Panel
    sidebarPanel(
      # File input for uploading dataset
      fileInput("file", "Upload Dataset (CSV)", accept = ".csv"),
      
      # Button to load data
      actionButton("load_data", "Load Data"),
      
      # Input for K-Means clustering
      numericInput("clusters", "Number of Clusters (2-4)", value = 3, min = 2, max = 4),
      actionButton("run_kmeans", "Run K-Means"),
      
      # Inputs for Apriori algorithm parameters
      numericInput("support", "Min Support (0.001-1)", value = 0.01, min = 0.001, max = 1),
      numericInput("confidence", "Min Confidence (0.001-1)", value = 0.01, min = 0.001, max = 1),
      actionButton("run_apriori", "Run Apriori"),
      
      # Dynamic dropdown for city selection
      uiOutput("city_selector") 
    ),
    
    # Main Panel
    mainPanel(
      # Tabbed layout for displaying outputs
      tabsetPanel(
        tabPanel("Data Overview", tableOutput("data_table")),  # Data overview table
        tabPanel("K-Means Clustering", plotOutput("kmeans_plot")),  # K-Means clustering plot
        tabPanel("Apriori Rules", tableOutput("apriori_table")),  # Apriori rules table
        tabPanel("City Analysis", plotOutput("city_plot"))  # City analysis plot
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive variable to store the uploaded dataset
  dataset <- reactiveVal()
  
  # Event to load and display the uploaded data
  observeEvent(input$load_data, {
    req(input$file)  # Ensure a file is selected
    data <- tryCatch({
      read.csv(input$file$datapath)  # Load the CSV file
    }, error = function(e) {
      showNotification("Error loading file: Please ensure it's a valid CSV file.", type = "error")
      return(NULL)
    })
    
    req(data)  # Ensure data is successfully loaded
    dataset(data)  # Store the data in the reactive variable
    output$data_table <- renderTable(head(data))  # Display the first few rows of the dataset
    
    # Dynamically update the city selection dropdown
    output$city_selector <- renderUI({
      req(dataset())  # Ensure the dataset is loaded
      if (!"city" %in% colnames(dataset())) {  # Check if the 'city' column exists
        showNotification("The dataset must contain a 'city' column.", type = "error")
        return(NULL)
      }
      selectInput("city", "Select City", choices = unique(dataset()$city))  # Populate dropdown with unique cities
    })
  })
  
  # Event to run K-Means clustering
  observeEvent(input$run_kmeans, {
    req(dataset())  # Ensure the dataset is loaded
    data <- dataset()
    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]  # Select only numeric columns
    
    # Ensure there are at least two numeric columns
    if (ncol(numeric_data) < 2) {
      showNotification("At least two numeric columns are required for K-Means.", type = "error")
      return()
    }
    
    # Perform K-Means clustering
    kmeans_result <- kmeans(numeric_data, centers = input$clusters)
    output$kmeans_plot <- renderPlot({
      plot(numeric_data[, 1:2], col = kmeans_result$cluster, main = "K-Means Clustering")
    })
  })
  
  # Event to run Apriori algorithm
  observeEvent(input$run_apriori, {
    req(dataset())  # Ensure the dataset is loaded
    data <- dataset()
    
    # Ensure the dataset contains an 'items' column
    if (!"items" %in% colnames(data)) {
      showNotification("Dataset must contain an 'items' column with comma-separated values.", type = "error")
      return()
    }
    
    # Prepare data for Apriori analysis
    items <- strsplit(as.character(data$items), ",")  # Split items by comma
    transactions <- as(items, "transactions")  # Convert to transactions
    rules <- apriori(transactions, parameter = list(supp = input$support, conf = input$confidence))  # Run Apriori
    
    # Display the Apriori rules in a table
    output$apriori_table <- renderTable({
      as(rules, "data.frame")
    })
  })
  
  # Event to analyze and plot data by selected city
  observeEvent(input$city, {
    req(dataset())  # Ensure the dataset is loaded
    req(input$city)  # Ensure a city is selected
    
    # Filter data by the selected city and group by payment type
    city_data <- dataset() %>%
      filter(city == input$city) %>%  # Filter rows for the selected city
      group_by(paymentType) %>%  # Group by payment type
      summarise(total = sum(total, na.rm = TRUE))  # Calculate total for each payment type
    
    # Plot the distribution of payment types for the selected city
    output$city_plot <- renderPlot({
      ggplot(city_data, aes(x = paymentType, y = total, fill = paymentType)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Payment Type Distribution in", input$city),
             x = "Payment Type", y = "Total Amount") +
        theme_minimal()
    })
  })
}
# run shiny app
shinyApp(ui = ui, server = server)