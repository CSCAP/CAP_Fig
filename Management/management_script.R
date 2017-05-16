library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
library(pdftools)
library(ggplot2)
library(scales)
library(stringr)
library(tidyverse)
library(grid)
library(gridExtra)


dir_plot <- "~/GitHub/CSCAP/CAP_Fig/Management"
setwd(dir_plot)

# Corn CAP Project colours 
# cols <- c("#499314", "#ACCD42", "#FFD8A8", "#FB9147", "#4D4325", "#9FCAE9", "#E4CE77", "#FFD19D")


# Meetings =======================================================

Meeting <- read_excel("~/GitHub/CSCAP/CAP_Data/Meeting.xlsx", 
                      col_types = c("blank", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "numeric"))

Meeting %>%
  select(-c(`Duration (hours:minutes:seconds)`, `In Adobe Recording Master List`, 
            `Recording Saved to P drive`, `Recording Available`, Transcribed, Notes)) %>%
  separate(`Meeting Name`, into = c("date", "group", "additional"), sep = "_|-") %>%
  mutate(date = ymd(date)) %>%
  mutate(additional = ifelse(str_detect(group, "obj"), str_replace(group, "obj", ""), additional)) %>%
  mutate(group = ifelse(str_detect(group, "obj"), "obj", group)) %>%
  filter(str_detect(`Meeting Type`, "Virtual")) %>%
  mutate(byear = ifelse(date < "2012-03-01", "Y1", 
                        ifelse(date < "2013-03-01", "Y2", 
                               ifelse(date < "2014-03-01", "Y3",
                                      ifelse(date < "2015-03-01", "Y4",
                                             ifelse(date < "2016-03-01", "Y5", "Y6")))))) %>%
  mutate(byear = ifelse(byear == "Y6", "Y5", byear)) %>%
  group_by(byear, group) %>%
  summarise(n = n()) %>%
  filter(group %in% c("whole", "leadership", "obj")) %>%
  ggplot(aes(x = byear, y = n, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Funding Years",
       y = "Virtual Meetings") +
  scale_fill_manual(labels = c("Leadership Team", "Objective-Specific", "Whole Team"), 
                    values = c("#9FCAE9", "#ACCD42", "#FB9147", "#FFD19D", "#4D4325")) +
  theme_light() +
  theme(#aspect.ratio = 1, 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = "FIG1a.png", width = 8, 
       height = 6, dpi = 300)




# Students =======================================================

STEM <- read_excel("~/GitHub/CSCAP/CAP_Data/STEM.xlsx")

STEM %>%
  select(`Position Title`, `Employment Start Date`, `Expected Ending Date of Employment`, `Tenure (months)`) %>%
  rename(position = `Position Title`, 
         started = `Employment Start Date`, 
         ended = `Expected Ending Date of Employment`, 
         tenure = `Tenure (months)`) %>% 
  mutate(position = ifelse(grepl("Undergraduate", position), "Undergraduate Student", position)) %>%
  group_by(position) %>%
  summarise(tenure = sum(tenure)) %>%
  filter(tenure > 100) %>%
  mutate(position = factor(position, 
                           levels = c("Undergraduate Student",
                                      "MS Graduate Student",
                                      "PhD Graduate Student",
                                      "Post Doctoral Researcher"),
                           labels = c("Undergraduate Student",
                                      "MS Graduate Student",
                                      "PhD Graduate Student",
                                      "Postdoctoral Associate"))) %>%
  ggplot(aes(x=position, y=tenure)) +
  geom_bar(stat = "identity", position = "stack", fill = "#9FCAE9") +
  labs(x = "",
       y = "Months of Training") +
  theme_light() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  coord_flip() +
  theme(#aspect.ratio = 1, 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())
ggsave(filename = "FIG2b.png", width = 8, 
       height = 6, dpi = 300)





STEM %>%
  select(`Position Title`, `Employment Start Date`, `Expected Ending Date of Employment`, `Tenure (months)`) %>%
  rename(position = `Position Title`, 
         started = `Employment Start Date`, 
         ended = `Expected Ending Date of Employment`, 
         tenure = `Tenure (months)`) %>% 
  mutate(position = ifelse(grepl("Undergraduate", position), "Undergraduate Student", position)) %>%
  filter(position %in% c("Undergraduate Student",
                         "MS Graduate Student",
                         "PhD Graduate Student",
                         "Post Doctoral Researcher")) %>%
  mutate(d1 = ifelse(str_length(started)==7, paste0(started, ".01"), started),
         d2 = ifelse(str_length(ended)==7, paste0(ended, ".15"), ended)) %>%
  mutate(started = ymd(d1), 
         ended = ymd(d2)) %>%
  mutate(started = ifelse(is.na(started) & !is.na(ended), (ended - months(tenure)), started)) %>%
  mutate(started = as.Date(started, origin = origin)) %>%
  mutate(ended = ifelse(is.na(ended) & !is.na(started), (started + months(tenure)), ended)) %>%
  mutate(ended = as.Date(ended, origin = origin)) %>%
  mutate(d1.1 = ifelse(is.na(started), word(d1, 1), NA),
         d2.1 = ifelse(is.na(ended), word(d2, 1), NA)) %>%
  mutate(d1.1 = ifelse(str_length(d1.1)==7, paste0(d1.1, ".01"), d1.1),
         d2.1 = ifelse(str_length(d2.1)==7, paste0(d2.1, ".15"), d2.1)) %>%
  mutate(started = ifelse(is.na(started), ymd(d1.1), started),
         ended = ifelse(is.na(ended), ymd(d2.1), ended)) %>%
  mutate(started = as.Date(started, origin = origin),
         ended = as.Date(ended, origin = origin)) -> Student

Student %>% 
  filter(!is.na(d1.1)) %>%
  mutate(started = NA, ended = NA) %>%
  bind_rows(Student[1:6]) %>%
  mutate(d1.1 = ifelse(is.na(started), word(d1, 3), NA),
         d2.1 = ifelse(is.na(ended), word(d2, 3), NA)) %>%
  mutate(d1.1 = ifelse(str_length(d1.1)==7, paste0(d1.1, ".01"), d1.1),
         d2.1 = ifelse(str_length(d2.1)==7, paste0(d2.1, ".15"), d2.1)) %>%
  mutate(started = ifelse(is.na(started), ymd(d1.1), started),
         ended = ifelse(is.na(ended), ymd(d2.1), ended)) %>%
  mutate(started = as.Date(started, origin = origin),
         ended = as.Date(ended, origin = origin)) %>%
  select(position, started, ended) %>%
  mutate(Syear = ifelse(started < "2012-03-01", 1, 
                        ifelse(started < "2013-03-01", 2, 
                               ifelse(started < "2014-03-01", 3,
                                      ifelse(started < "2015-03-01", 4,
                                             ifelse(started < "2016-03-01", 5, 6)))))) %>%
  
  mutate(Eyear = ifelse(ended < "2012-03-01", 1, 
                        ifelse(ended < "2013-03-01", 2, 
                               ifelse(ended < "2014-03-01", 3,
                                      ifelse(ended < "2015-03-01", 4,
                                             ifelse(ended < "2016-03-01", 5, 6)))))) %>%
  mutate(years = Eyear - Syear) %>%
  mutate(Y1 = ifelse(Syear == 1, 1, NA),
         Y2 = ifelse(Syear == 2, 2, NA),
         Y3 = ifelse(Syear == 3, 3, NA),
         Y4 = ifelse(Syear == 4, 4, NA),
         Y5 = ifelse(Syear == 5, 5, NA),
         Y6 = ifelse(Syear == 6, 6, NA)) %>%
  mutate(Y1 = ifelse(Eyear == 1, 1, Y1),
         Y2 = ifelse(Eyear == 2, 2, Y2),
         Y3 = ifelse(Eyear == 3, 3, Y3),
         Y4 = ifelse(Eyear == 4, 4, Y4),
         Y5 = ifelse(Eyear == 5, 5, Y5),
         Y6 = ifelse(Eyear == 6, 6, Y6)) %>%
  rownames_to_column() %>%
  gather(key = Year, value = value, na.rm = FALSE, Y1:Y6) %>%
  arrange(rowname, Year) %>%
  group_by(rowname) %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>% 
  mutate(Year = ifelse(Year == "Y6", "Y5", Year)) %>%
  group_by(position, Year) %>%
  summarise(count = sum(!is.na(value))) %>%
  ungroup() %>%
  mutate(position = factor(position, 
                           levels = c("Undergraduate Student",
                                      "MS Graduate Student",
                                      "PhD Graduate Student",
                                      "Post Doctoral Researcher"),
                           labels = c("Undergraduate Student",
                                      "MS Graduate Student",
                                      "PhD Graduate Student",
                                      "Postdoctoral Associate"))) %>% 
  ggplot(aes(x=Year, y=count, fill = position)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Funding Years",
       y = "Next Generation Scientists") +
  scale_fill_manual(values = c("#499314", "#ACCD42", "#FFD8A8", "#FB9147", "#4D4325", "#9FCAE9")) +
  theme_light() +
  theme(#aspect.ratio = 1, 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = "FIG2a.png", width = 8, 
       height = 6, dpi = 300)




# Outputs =============================
 
Output <- read_excel("~/GitHub/CSCAP/CAP_Data/Team_Output.xlsx")

# # Figure 3a
# Output %>%
#   filter(`USDA Acknowledgment` == "Yes") %>%
#   filter(Status %in% c("Complete", "Submitted for Review", "Accepted/In Press")) %>%
#   mutate(Status = factor(Status, levels = c("Submitted for Review", "Accepted/In Press", "Complete"))) %>%
#   mutate(`Funding Year` = ifelse(`Funding Year` == "Y6", "Y5", `Funding Year`)) %>%
#   group_by(Status, `Funding Year`) %>%
#   summarise(count = n()) %>%
#   ggplot(aes(x=`Funding Year`, y=count, fill = Status)) +
#   geom_bar(stat = "identity", position = "stack") +
#   labs(x = "Funding Years",
#        y = "Number of Outputs") +
#   scale_y_continuous(limits = c(0, 600)) +
#   scale_fill_brewer(#labels = c("Leadership Team", "Objective-Specific", "Whole Team"), 
#     palette = "Accent", direction = -1) +
#   theme_light() +
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         legend.text = element_text(size = 16),
#         legend.title = element_blank())
# ggsave(filename = "FIG3a.png", width = 8, height = 6, dpi = 300)



# Figure 3aa
Output %>%
  filter(`USDA Acknowledgment` == "Yes") %>%
  filter(Status %in% c("Complete", "Submitted for Review", "Accepted/In Press")) %>%
  mutate(Status = factor(Status, 
                         levels = c("Submitted for Review", "Accepted/In Press", "Complete"),
                         labels = c("Submitted for Review", "Accepted and In Press", "Published"))) %>%
  mutate(`Funding Year` = ifelse(`Funding Year` == "Y6", "Y5", `Funding Year`)) %>%
  filter(`Type of Output/ Product` == "Refereed Journal") %>%
  group_by(Status, `Funding Year`) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=`Funding Year`, y=count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Funding Years",
       y = "Refereed Journals") +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_manual(values = c("#9FCAE9", "#FFD8A8", "#ACCD42", "#499314", "#FFD19D","#4D4325")) +
  theme_light() +
  theme(#aspect.ratio = 1, 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = "FIG3aa.png", width = 8, 
       height = 6, dpi = 300)


# # Figure 3.b
# Output %>%
#   filter(`USDA Acknowledgment` == "Yes") %>%
#   filter(Status %in% c("Complete", "Submitted for Review", "Accepted/In Press")) %>%
#   mutate(Status = factor(Status, levels = c("Submitted for Review", "Accepted/In Press", "Complete"))) %>%
#   mutate(type = `Type of Output/ Product`) %>%
#   mutate(category = ifelse(type %in% c("Presentation (Extension/ Outreach)", "Presentation (Conference)", 
#                                        "Education Webinar", "Teaching (Formal Education)", "Education Camp/ Workshop"), "Presentations",
#                            ifelse(type %in% c("Extension Publication", "White Paper/ Fact Sheet", "Promotional/ Project Report", 
#                                               "Book", "Book Chapter", "Survey"), "Other Publications",
#                                   ifelse(type %in% c("PhD Dissertation", "MS Thesis"), "Thesis/Dissertation",
#                                          ifelse(type %in% c("Refereed Journal"), "Refereed Journal", "Media"))))) %>%
# # LIST OF MEDIA: "Media_ Univ Press", "Media_ News Release", "Media_ Popular Press", "Media_ Blog", "Media_ Radio/TV", "Media_ Video", "Website", 
#   group_by(category) %>%
#   summarise(count = n()) %>%
#   ggplot(aes(x = reorder(category, count), y = count)) +
#   geom_bar(fill = "skyblue", stat = "identity") +
#   labs(#title = "Type of Completed Outputs", 
#        x = "",
#        y = "Number of Outputs") +
#   theme_light() +
#   scale_y_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 400)) +
#   coord_flip() +
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16))
# ggsave(filename = "FIG3b.png", width = 8, height = 6, dpi = 300)
  
  
  
#  OPTION BB
Output %>%
  filter(`USDA Acknowledgment` == "Yes") %>%
  filter(Status %in% c("Complete", "Submitted for Review", "Accepted/In Press")) %>%
  mutate(Status = factor(Status, levels = c("Submitted for Review", "Accepted/In Press", "Complete"))) %>%
  mutate(type = `Type of Output/ Product`) %>%
  mutate(category = ifelse(type %in% c("Presentation (Extension/ Outreach)", "Presentation (Conference)", 
                                       "Education Webinar", "Teaching (Formal Education)", "Education Camp/ Workshop"), "Presentation",
                           ifelse(type %in% c("Extension Publication", "White Paper/ Fact Sheet", "Promotional/ Project Report", 
                                              "Book", "Book Chapter", "Survey"), "Other Publication",
                                  ifelse(type %in% c("PhD Dissertation", "MS Thesis"), "Thesis or Dissertation",
                                         ifelse(type %in% c("Refereed Journal"), "Refereed Journal", "Media"))))) %>%
  mutate(`Funding Year` = ifelse(`Funding Year` == "Y6", "Y5", `Funding Year`)) %>%
  group_by(category, `Funding Year`) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=`Funding Year`, y=count, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = "Funding Years", y = "Outputs") +
  theme_light() +
  scale_y_continuous(limits = c(0, 600)) +
  scale_fill_manual(values = c("#FB9147", "#FFD8A8", "#ACCD42", "#499314", "#4D4325", "#9FCAE9")) +
  theme(#aspect.ratio = 1, 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = "FIG3bb.png", width = 8, 
       height = 6, dpi = 300) 



#  Figure 4
Output %>%
  filter(`USDA Acknowledgment` == "Yes") %>%
  filter(Status %in% c("Complete", "Submitted for Review", "Accepted/In Press")) %>%
  mutate(Status = factor(Status, levels = c("Submitted for Review", "Accepted/In Press", "Complete"))) %>%
  mutate(type = `Type of Output/ Product`) %>%
  mutate(category = ifelse(type %in% c("Presentation (Extension/ Outreach)", "Presentation (Conference)", 
                                       "Education Webinar", "Teaching (Formal Education)", "Education Camp/ Workshop"), "Presentation",
                           ifelse(type %in% c("Extension Publication", "White Paper/ Fact Sheet", "Promotional/ Project Report", 
                                              "Book", "Book Chapter", "Survey"), "Other Publication",
                                  ifelse(type %in% c("PhD Dissertation", "MS Thesis"), "Thesis or Dissertation",
                                         ifelse(type %in% c("Refereed Journal"), "Refereed Journal", "Media"))))) %>%
  group_by(category) %>%
  summarise(`PIs` = sum(as.numeric(`Other_# CSCAP PI's Authors`), na.rm = TRUE),
            `Graduate Students` = sum(as.numeric(`Other_# CSCAP Grad Authors`), na.rm = TRUE),
            `Postdoc` = sum(as.numeric(`Other_# CSCAP Postdocs Authors`), na.rm = TRUE),
            `Staff` = sum(as.numeric(`Other_# CSCAP Staff Authors`), na.rm = TRUE)) %>%
  gather(key = Author, value = count, na.rm = FALSE, PIs:Staff) %>%
  mutate(category = factor(category, levels = c("Media", "Other Publication", "Presentation", 
                                                "Refereed Journal", "Thesis or Dissertation"))) %>%
  filter(category != "Media") %>% 
  mutate(Author = ifelse(Author == "Postdoc", "Postdoctoral Associate", 
                         ifelse(Author == "Graduate Students", "Graduate Student",
                                ifelse(Author == "PIs", "PI", Author)))) %>%
  ggplot(aes(x=reorder(Author, count), y=count, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Number of Outputs") +
  theme_light() +
  scale_fill_manual(values = c("#FFD8A8", "#ACCD42", "#499314", "#4D4325", "#9FCAE9")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  theme(#aspect.ratio = 1, 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = "FIG4.png", width = 8, 
       height = 6, dpi = 300)  


################################################################################



# Leverage Funding =============================

Leverage <- read_excel("~/GitHub/CSCAP/CAP_Data/Leverage_Dollars.xlsx", 
                       sheet = "Sheet1")

Leverage %>%
  gather(key = funds, value = amount, 2:4, na.rm = FALSE) %>%
  mutate(year = factor(year, levels = c("Y1", "Y2", "Y3", "Y4", "Y5", "Post Project", "Total"))) %>%
  mutate(funds = factor(funds, levels = c("USDA-NIFA Grant", "Leveraged Funds ", "Total Funds "))) %>%
  ggplot(aes(x = year, y = amount/10^6, fill = funds)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_light() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous("Received Funding (millions of USD)", labels = scales::dollar) +
  scale_fill_manual(values = c("#499314", "#ACCD42",  "#4D4325", "#FFD8A8", "#FB9147", "#9FCAE9", "#E4CE77")) +
  theme(#aspect.ratio = 1/2,
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
ggsave(filename = "FIG5.png", width = 12, 
       height = 6, dpi = 300)  
