library(tidyverse)
library(readxl)
library(here)
library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)
library(plotly)
library(webshot)
library(skimr) # install.packages('skimr')
library(kableExtra) # install.packages('kableExtra')


setwd('~/Documents/DATA332/Projects/Patient Data')

billing <- read_excel('Billing.xlsx', .name_repair = 'universal')
patient <- read_excel('Patient.xlsx', .name_repair = 'universal')
visit <- read_excel('Visit.xlsx', .name_repair = 'universal')

df <- left_join(visit, billing, by = c('VisitID'))
df <- left_join(df, patient, by = c('PatientID'))

df$Reason <- gsub("Laceration of (left hand|right foot|right calf)", "Laceration", df$Reason)

##1
df[c('Year.Visit', 'Month.Visit', 'Date.Visit')] <- str_split_fixed(df$VisitDate, '-',3)

reason_for_visit_by_month <- df %>%
  group_by(Reason, Month.Visit) %>%
  summarize(Count = n()) %>%
  mutate(Month.Visit = month.abb[as.numeric(Month.Visit)])

reason_by_month <- ggplot(reason_for_visit_by_month, aes(x = Reason, y = Count, fill= Month.Visit)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text = element_text(angle = 0, vjust =.5, hjust =1)) +
  coord_flip() +
  labs(fill = "Month")

ggsave("~/Documents/DATA332/Projects/Patient Data/Images/Reason for visit by Month.png", reason_by_month, width = 10, height = 6, dpi = 300, bg = "white")

##2
reason_for_visit_walkin <- df %>% 
  group_by(Reason, WalkIn) %>% 
  summarize(Count = n())

reason_walkin <- ggplot(reason_for_visit_walkin, aes(x = Reason, y = Count, fill = WalkIn)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text = element_text(angle = 0, vjust =.5, hjust =1)) +
  coord_flip() +
  labs(fill = "Walk in")

ggsave("~/Documents/DATA332/Projects/Patient Data/Images/Reason for visit by WalkIn.png", reason_walkin, width = 10, height = 6, dpi = 300, bg = "white")

##3
reason_for_visit_city <- df %>% 
  group_by(Reason, City) %>% 
  summarize(Count = n())

reason_city <- ggplot(reason_for_visit_city, aes(x = Reason, y = Count, fill = City)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text = element_text(angle = 0, vjust =.5, hjust =1))+
  coord_flip()

ggsave("~/Documents/DATA332/Projects/Patient Data/Images/Reason for visit by City.png", reason_city, width = 10, height = 6, dpi = 300, bg = "white")

##4
total_invoive <- df %>% 
  group_by(Reason, InvoicePaid) %>% 
  summarize(TotalInvoice = sum(InvoiceAmt))

total_invoice_plot <- ggplot(total_invoive, aes(x = Reason, y = TotalInvoice, fill = InvoicePaid)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text = element_text(angle = 0, vjust =.5, hjust =1)) +
  coord_flip() + 
  labs(fill = "Invoice Paid", y = "Total Invoice")

ggsave("~/Documents/DATA332/Projects/Patient Data/Images/Total Invoice.png", total_invoice_plot, width = 10, height = 6, dpi = 300, bg = "white")

##5
busiest_month <- df %>%
  group_by(Month.Visit) %>% 
  summarize(Count = n ()) %>%
  mutate(Month.Visit = month.abb[as.numeric(Month.Visit)])

busiest_month_plot <- plot_ly(
  labels = busiest_month$Month.Visit,
  parents = "",
  values = busiest_month$Count,
  type = "treemap",
  text = paste("Patients:", busiest_month$Count)
) %>% layout(
  annotations = list(
    text = "Busiest Month",
    x = 0.5,
    y = 1.05,
    xref = "paper",
    yref = "paper",
    showarrow = FALSE
  )
)

# Convert plotly plot to HTML
html_file <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(busiest_month_plot, html_file)

# Save the HTML file as PNG using webshot
webshot(html_file, file = "~/Documents/DATA332/Projects/Patient Data/Images/Busiest_Month.png")










