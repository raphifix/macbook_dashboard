library(dplyr)
library(DT)
library(readr)
library(rvest)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "2015 Macbook Checker", titleWidth = '300px'),
  dashboardSidebar(disable = T),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width = 12, status = 'primary', DT::dataTableOutput('refurb_dt'))
    )
  )
)

server <- function(input, output) {
  refurb_df <- reactive({
    url <- 'https://www.apple.com/shop/browse/home/specialdeals/mac'
    page <- read_html(url)
    
    tables <- 
      page %>% 
      html_nodes(xpath = '//*[@class="product"]')
    num_products <- length(tables)
    
    page %>% html_nodes(xpath = '//*[@id="primary"]/div[2]/div[2]/table[24]/tbody/tr/td[3]/p/span/span/span') %>% html_text()  %>% parse_number()
    product_list <- list()
    for(i in 1:num_products){
      product <- 
        page %>% 
        html_nodes(xpath = paste0('//*[@id="primary"]/div[2]/div[2]/table[', i, ']/tbody/tr/td[2]/h3/a')) %>% 
        html_text()
      
      year <- 
        page %>% 
        html_nodes(xpath = paste0('//*[@id="primary"]/div[2]/div[2]/table[', i, ']/tbody/tr/td[2]')) %>% 
        html_text()
      year <- gsub(".*Originally released *(.*?) *[\r|\n].*", "\\1", year)
      
      price <- 
        page %>% 
        html_nodes(xpath = paste0('//*[@id="primary"]/div[2]/div[2]/table[', i, ']/tbody/tr/td[3]/p/span/span/span')) %>% 
        html_text() %>% 
        parse_number()
      
      link <- 
        page %>% 
        html_nodes(xpath = paste0('//*[@id="primary"]/div[2]/div[2]/table[', i, ']/tbody/tr/td[2]/h3/a')) %>% 
        html_attr('href') %>% 
        paste0('https://www.apple.com', .)
      
      product_list[[i]] <- c(product, year, price, link)
    }
    product_df <- as.data.frame(do.call('rbind', product_list))
    names(product_df) <- c('full_model', 'mon_year', 'price', 'link')
    product_df$mon_year <- parse_number(product_df$mon_year)
    product_df$thirteen_inch <- grepl('13\\.3', product_df$full_model)
    product_df$full_model <- trimws(product_df$full_model)
    product_df$what_youre_looking_for <- ifelse(product_df$mon_year == 2015 & product_df$thirteen_inch, 1, 0)
    
    product_df %>% 
      arrange(desc(what_youre_looking_for), price) %>% 
      mutate(full_model = paste0("<a href='", link, "'>", full_model, "</a>")) %>% 
      select(-link)
  })
  
  output$refurb_dt <- DT::renderDataTable({
    datatable(refurb_df(),
              selection = 'none',
              escape = FALSE,
              colnames = c('Model', 'Year', 'Price', '13-inch', 'Product Match'),
              options = list(dom = 't',
                             paging = FALSE,
                             ordering = FALSE,
                             columnDefs = list(list(visible=FALSE, targets=c(4,5))))) %>% 
      formatStyle(
        'what_youre_looking_for',
        target = 'row',
        backgroundColor = styleEqual(c(0, 1), c('white', 'lightgreen'))
      ) %>% 
      formatCurrency('price', digits = 0)
  })
}

shinyApp(ui, server)