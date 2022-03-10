library(shiny)
library(plotly)
library(readxl)
library(DT)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(haven)
library(glue)


shinyServer(function(input, output) {
        
        convert.size <-
                function(x) {
                        return(factor(x,
                                      levels = c("0", "1", "2", "3", "4"),
                                      labels = c("All", "6-19 RME", "20-49 RME", "50-99 RME",
                                                 "100+ RME")
                                      )
                               )
                        }

        convert.industry <- 
                function(x) {
                        return(factor(x, 
                                      levels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                                               "10", "11", "12", "13", "14", "15", "16", "17", "18"),
                                      labels = c("All", "Agriculture, forestry and fishing", "Mining",
                                                 "Manufacturing", 
                                                 "Electricity, gas, water and waste services",
                                                 "Construction", "Wholesale trade", "Retail trade",
                                                 "Accomodation", "Transport, postal and warehousing",
                                                 "Informationn, media and telecommunications", 
                                                 "Financial and information services",
                                                 "Rental, hiring and real estate services",
                                                 "Professional, scientific and technical services",
                                                 "Administration and support services", 
                                                 "Education and training", "Health care and social assistance",
                                                 "Arts and recreation services", "Other services")
                                      )
                               )
                }
        
        convert.cluster <- 
                function(x) {
                        return(factor(x, 
                                      levels = c("strat_what", "strat_how", "ss_chains",
                                                 "info_assess", "info_scan", "employees",
                                                 "quality", "n/a"),
                                      labels = c("Strategy - focus", "Strategy - practices", "Supply chain linkages",
                                                 "Information - Assessment", "Information - scanning", 
                                                 "Employee practices", "Quality and process", "Other")
                                      
                                      )
                               )
                        }
        
        convert.weight <- 
                function(x) {
                        return(factor(x, 
                                      levels=c("pweight", "eweight"),
                                      labels = c("Firm", "Employment")
                                      )
                               )
                        }
        
        convert.decomp <- 
                function(x) {
                        return(factor(x, 
                                      levels=c("Continuers - within", "Continuers - across", 
                                               "Continuers - enter", "Continuers - exit", 
                                               "Entrants", "Exiters", "Joiners", "Leavers"),
                                      labels = c("Continuers - within", "Continuers - across", 
                                                 "Continuers - enter", "Continuers - exit", 
                                                 "Entrants", "Exiters", "Joiners", "Leavers")
                                      )
                               )
                        }
        
        
        dat <- read_stata("Data/T1_overall_wide.dta") %>%
                rename(`2005` = rr_2005, 
                       `2009` = rr_2009,
                       `2013` = rr_2013,
                       `2017` = rr_2017,
                       variable_code = variable, 
                       practice_name = long_name, 
                       practice_shortname = short_name,  
                       aggregate_change = change05_17, 
                       pct_change = pct_change05_17,
                       p_value = prtest05_17) %>%
                filter(variable_code != "smp_index") %>% 
                mutate(size = convert.size(size),
                       industry = convert.industry(industry),
                       cluster = convert.cluster(cluster),
                       weight = convert.weight(weight),
                       `2005` = ifelse(`2005`>1, 1, `2005`),
                       `2009` = ifelse(`2009`>1, 1, `2009`),
                       `2013` = ifelse(`2013`>1, 1, `2013`),
                       `2017` = ifelse(`2017`>1, 1, `2017`)) %>% 
                mutate_at(vars(1:6), as.factor) %>% 
                mutate_at(vars(7:22), as.numeric) %>%
                mutate_at(vars(23:26), as.factor)  
        
        
        dat_plot <- dat %>% 
                        dplyr::select(practice_name, practice_shortname, 
                                      cluster:weight, 
                                      `2005`:aggregate_change, 
                                      p_value)
        
        dat_plot <- merge(
                        dat_plot %>% 
                        dplyr::select(-starts_with("se")) %>% 
                        pivot_longer(c(`2005`, `2009`, `2013`, `2017`), 
                                     names_to = "year",
                                     values_to = "value"
                                     ) %>% 
                        mutate(year = as.numeric(year))
                        ,
                        dat_plot %>% 
                        select(-`2005`, -`2009`, -`2013`, -`2017`) %>%
                        pivot_longer(c(starts_with("se")), 
                                     names_to = "year", 
                                     values_to = "se"
                                     ) %>% 
                        mutate(year = as.numeric(gsub("se_", "", year)))
                        ) %>% 
                dplyr::arrange(practice_name, industry, size, weight, year) %>% 
                as_tibble()
        
        
        dat2 <- read_excel("Data/2c_decomp_CONF.xlsx", 
                           range = cell_cols("D:J"), 
                           sheet =  "pw_anz06_transpose"
                           ) %>% 
                rbind(read_excel("Data/2c_decomp_CONF.xlsx", 
                                 range = cell_cols("D:J"), 
                                 sheet = "ew_anz06_transpose"
                                 )
                      ) %>% 
                dplyr::select(variable, cluster, weight, 
                              rr_decomp_within, rr_decomp_across, rr_D_total
                              ) %>% 
                dplyr::filter(cluster != "n/a") %>% 
                mutate(cluster = convert.cluster(cluster),
                       weight = convert.weight(weight)
                       ) %>% 
                rename(variable_code = variable,
                       decomp_within = rr_decomp_within, 
                       decomp_across = rr_decomp_across,
                       decomp_overall = rr_D_total
                       ) %>%
                mutate_at(vars(1:3), as.factor) %>%
                mutate_at(vars(4:6), as.numeric) %>% 
                left_join(
                          subset(dat, dat$size== "All" & dat$industry == "All", 
                                 select = c("variable_code", "practice_name", 
                                            "weight", "size", "industry", 
                                            "p_value", "practice_shortname")
                                 ), 
                          by = c("variable_code" = "variable_code", 
                                 "weight" = "weight")
                          ) 
        
        
        dat2_plot <- dat2 %>% 
                        dplyr::select(practice_name, cluster, weight, 
                                      decomp_within, decomp_across, p_value
                                      ) %>% 
                        pivot_longer(c("decomp_within", "decomp_across"),
                                     names_to = "Type", 
                                     values_to = "Decomposition"
                                     ) %>% 
                mutate(significance = ifelse(p_value < 0.1, TRUE, 
                                             FALSE))
        
        
        dat3 <- read_excel("Data/2a_decomp_CONF.xlsx", 
                           range = cell_cols("D:N"),
                           sheet = "Transpose") %>% 
                dplyr::select(-`...8`) %>% 
                dplyr::filter(cluster != "n/a") %>% 
                `colnames<-` (c("Continuers - within", "Continuers - across", "Entrants",
                                "Exiters", "Joiners", "Leavers", "Total", 
                                "variable_code", "cluster", "weight")
                              ) %>% 
                mutate(cluster = convert.cluster(cluster),
                       weight = convert.weight(weight)
                       ) %>%
                mutate_at(vars(1:7), as.numeric) %>%
                mutate_at(vars(8:9), as.factor) %>% 
                left_join(
                          subset(dat, dat$size== "All" & dat$industry == "All", 
                                 select = c("variable_code", "practice_name", 
                                            "weight", "size", "industry", 
                                            "p_value", "practice_shortname")
                                 ), 
                          by = c("variable_code" = "variable_code", 
                                 "weight" = "weight")
                          ) 
        
        
        dat3_plot <- dat3 %>% 
                        dplyr::select(practice_name, weight, 
                                      `Continuers - within`, `Continuers - across`, 
                                      Entrants, Exiters, Joiners, Leavers, 
                                      p_value, practice_shortname) %>% 
                        pivot_longer(c("Continuers - within", 
                                       "Continuers - across", "Entrants",
                                       "Exiters", "Joiners", "Leavers"),
                                     names_to = "Group",
                                     values_to = "Decomposition"
                                     )  %>% 
                        mutate(Group = convert.decomp(Group),
                               significance = ifelse(p_value < 0.1, TRUE,
                                                     FALSE),
                               limit_min = ifelse(practice_name == "Identify risks or opportunities from skill availability", -0.07,
                                                  -0.045),
                               limit_max = ifelse(practice_name == "Measures to reduce environmental impact", 0.05,
                                                  0.045),
                               Group = fct_rev(Group))
        
        
        dat4 <- read_excel("Data/2b_within_decomp_transpose.xlsx") %>% 
                `colnames<-` (c("Continuers - within", "Continuers - across", "Continuers - enter",
                                "Continuers - exit", "Entrants", "Exiters", "Joiners", 
                                "Leavers", "Total", "variable_code", "cluster", "weight",
                                "by_var", "by_group")) %>% 
                dplyr::filter(cluster != "n/a") %>% 
                mutate(across(.cols = c("Continuers - within", "Continuers - across", "Continuers - enter",
                                        "Continuers - exit", "Entrants", "Exiters", "Joiners", 
                                        "Leavers", "Total"),
                              .fns = ~str_replace(., "c", "suppressed"))
                       ) %>% 
                pivot_wider(names_from = "by_var", 
                            values_from = "by_group"
                            ) %>% 
                mutate(size = ifelse(is.na(size), 0, size),
                       industry = ifelse(is.na(industry), 0, industry)
                       ) %>% 
                dplyr::filter(!(industry %in% c(2,4,18))) %>% 
                mutate(size = convert.size(size),
                       industry = convert.industry(industry),
                       cluster = convert.cluster(cluster),
                       weight = convert.weight(weight)
                       ) %>% 
                left_join(
                          subset(dat, 
                                 select = c("variable_code", "practice_name", 
                                            "weight", "size", "industry", 
                                            "p_value", "practice_shortname")
                                 ),
                          by = c("variable_code" = "variable_code", 
                                 "weight" = "weight",
                                 "size" = "size",
                                 "industry" = "industry")
                          ) 
        
        
        dat4_plot <- dat4 %>% 
                        dplyr::select(practice_name, cluster, size, 
                                      industry, weight, `Continuers - within`, 
                                      `Continuers - across`, `Continuers - enter`, 
                                      `Continuers - exit`, Entrants, Exiters, 
                                      Joiners, Leavers, p_value, practice_shortname
                                      ) %>% 
                        pivot_longer(c("Continuers - within", "Continuers - across", 
                                       "Continuers - enter", "Continuers - exit", 
                                       "Entrants", "Exiters", "Joiners", "Leavers"),
                                     names_to = "Group",
                                     values_to = "Decomposition"
                                     )  %>% 
                        mutate(Group = convert.decomp(Group),
                               significance2 = ifelse(p_value < 0.1, TRUE, FALSE),
                               significance = ifelse(p_value < 0.1, "Sig", "Not-sig"),
                               Decomposition = as.numeric(ifelse(Decomposition == "suppressed", NA, Decomposition)),
                               limit_max = ifelse(Decomposition >= 0.025 & Decomposition <= 0.05, 0.055, 
                                                  ifelse(Decomposition >= 0.05 & Decomposition <= 0.075, 0.0755, 
                                                         ifelse(Decomposition >= 0.075 & Decomposition <= 0.1, 0.105, 
                                                                ifelse(Decomposition >= 0.1 & Decomposition <= 0.15, 0.155,
                                                                       ifelse(Decomposition >= 0.15 & Decomposition <= 0.2, 0.205, 
                                                                              ifelse(Decomposition >= 0.2 & Decomposition <= 0.3, 0.305,
                                                                                     ifelse(Decomposition >= 0.3 & Decomposition <= 0.4, 0.405,
                                                                                            ifelse(Decomposition >= 0.4, 0.45, 
                                                                                                   0.0275)))))))),
                               limit_min = ifelse(Decomposition <= -0.025 & Decomposition >= -0.05, -0.055,
                                                  ifelse(Decomposition <= -0.0525 & Decomposition >= -0.075, -0.0755,
                                                         ifelse(Decomposition <= -0.0725 & Decomposition >= -0.1, -0.105, 
                                                                ifelse(Decomposition <= -0.1 & Decomposition >= -0.15, -0.155,
                                                                       ifelse(Decomposition <= -0.15 , -0.185, 
                                                                              -0.0275)))))
                               ) %>% 
                        mutate(Group = fct_rev(Group),
                               limit_max = ifelse(is.na(limit_max), 0.0275, limit_max),
                               limit_min = ifelse(is.na(limit_min), -0.0275, limit_min))
        
        ### Tab 1; Prevalence of practices ###
        tab1_reac1 <- 
                reactive({
                        dat_final <- dat_plot %>% 
                                        dplyr::filter(practice_name %in% input$tab1_input,
                                                      size == "All",
                                                      industry == "All"
                                                      ) 
                
                        return(dat_final)
                })
        
        tab1_reac2 <- 
                reactive({
                        dat_final <- dat %>% 
                                        dplyr::select(practice_name, cluster:weight,
                                                      `2005`:se_2017, practice_shortname
                                                      ) %>% 
                                        dplyr::filter(practice_name %in% input$tab1_input,
                                                      size == "All",
                                                      industry == "All"
                                                      ) %>% 
                                        relocate(practice_shortname, .after = last_col()) 
                        
                        return(dat_final)
                })
        
        
        output$tab1_plot <- renderPlotly({
                shiny::validate(
                                need(input$tab1_input, "Input a value!")
                                )
                
                g <-     
                        tab1_reac1() %>% 
                        ggplot(aes(x = year,
                                   y = value,
                                   col = practice_shortname)
                               ) +
                        geom_path()  + 
                        geom_point()  +
                        geom_errorbar(aes(ymin = value - se,
                                          ymax = value + se),
                                      width = 0.25
                                      ) +
                        facet_wrap(~weight) + 
                        scale_y_continuous(breaks = seq(0, 1, 0.1),
                                           limits = c(0, 1.0),
                                           expand = expansion(mult = c(0, 0))
                                           ) +
                        scale_x_continuous(breaks = c(2005, 2009, 2013, 2017),
                                           labels = c("2005", "2009","2013", "2017")
                                           ) +
                        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                       '#753BBD', '#DF1995', '#FF6900', 
                                                       '#FBE122', '#000000',
                                                       '#91bbc2', '#caeb7e', '#81dbf1',
                                                       '#bfa5e1', '#f091cc', '#ffb581',
                                                       '#fdf29c', '#252443', '#a8a7b4',
                                                       '#c6dce0', '#e9f7ca', '#bababa')
                                            ) +
                        labs(y = "", x = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#dff7ff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing = unit(.25, units = "cm"),
                              plot.title = element_text(size=14, hjust = 5, vjust = -5, face="bold"),
                              legend.title = element_blank(),
                              axis.text.x=element_text(size=10),
                              axis.text.y=element_text(size=10),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.5,0.25,0.25,0.25), "cm")
                              )
                
                ggplotly(g) %>% 
                        layout(legend = list(title=list(text='<b> Practice: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h')
                               ) %>%  #yaxis = list(autorange = TRUE)
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian') #'resetScale2d' 'autoScale2d'
                        )
                
                
                
        })
        
        output$tab1_table <- 
                renderDT({
                        tab1_reac2() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                 list(extend = 'copy',
                                                                      buttons = c('copy'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'csv',
                                                                      buttons = c('csv'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'print',
                                                                      buttons = c('print'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show All',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show Less',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                 )
                                                         )
                                          )
                                )
                }) 
        
        
        ### Tab 2; Individual Practices ###
        tab2_reac1 <- 
                reactive({
                        
                        dat_s <- dat_plot %>% 
                                        dplyr::filter(practice_name %in% input$tab2_input,
                                                      size %in% input$tab2_size,
                                                      industry == "All"
                                                      ) 
                        
                        dat_i <- dat_plot %>% 
                                        dplyr::filter(practice_name %in% input$tab2_input,
                                                      size == "All",
                                                      industry %in% input$tab2_industry
                                                      ) 
                        
                        dat_final <- rbind(dat_s, dat_i) %>% 
                                        distinct() 
                        
                        return(dat_final)
                        
                })
        
        
        tab2_reac2<- 
                reactive({
                        
                        dat_s <- dat %>% 
                                        dplyr::select(practice_name, cluster:weight,
                                                      `2005`:se_2017, practice_shortname
                                                      ) %>% 
                                        dplyr::filter(practice_name %in% input$tab2_input,
                                                      size %in% input$tab2_size,
                                                      industry == "All"
                                                      ) 
                        
                        dat_i <- dat %>% 
                                        dplyr::select(practice_name, cluster:weight,
                                                      `2005`:se_2017, practice_shortname
                                                      ) %>% 
                                        dplyr::filter(practice_name %in% input$tab2_input,
                                                      size == "All",
                                                      industry %in% input$tab2_industry
                                                      ) 
                        
                        dat_final <- rbind(dat_s, dat_i) %>% 
                                        distinct() %>% 
                                        relocate(practice_shortname, .after = last_col())
                        
                        return(dat_final)
                        
                })
        
        output$tab2_plot_size <- renderPlotly({
                shiny::validate(
                        need(input$tab2_size, "Input a value!")
                )
                
                g <-     
                        tab2_reac1() %>% 
                        filter(industry == "All") %>% 
                        ggplot(aes(x = year,
                                   y = value,
                                   col = size)
                               ) +
                        geom_path()  + 
                        geom_point()  +
                        geom_errorbar(aes(ymin = value - se,
                                          ymax = value + se),
                                      width = 0.25
                        ) +
                        facet_wrap(~weight) + 
                        scale_y_continuous(breaks = seq(0, 1, 0.1),
                                           limits = c(0, 1.0),
                                           expand = expansion(mult = c(0, 0))
                                           ) +
                        scale_x_continuous(breaks = c(2005, 2009, 2013, 2017),
                                           labels = c("2005", "2009","2013", "2017")
                                           ) +
                        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                       '#753BBD', '#DF1995')
                                            ) +
                        labs(y = "", x = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#dff7ff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing = unit(.5, units = "cm"),
                              plot.title = element_text(size=14, hjust = 5, vjust = -5, face="bold"),
                              legend.title = element_blank(),
                              axis.text.x=element_text(size=10),
                              axis.text.y=element_text(size=10),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.25,0.25,0.5,0.25), "cm")
                              )
                
                ggplotly(g) %>% 
                        layout(legend = list(title=list(text='<b> Size: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h')
                               ) %>%  #yaxis = list(autorange = TRUE)
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian') #'resetScale2d' 'autoScale2d'
                        )
                
                
                
        })
        
        output$tab2_plot_ind <- renderPlotly({
                shiny::validate(
                        need(input$tab2_industry, "Input a value!")
                )
                
                g <-     
                        tab2_reac1() %>% 
                        filter(size == "All") %>% 
                        ggplot(aes(x = year,
                                   y = value,
                                   col = industry)
                               ) +
                        geom_path()  + 
                        geom_point()  +
                        geom_errorbar(aes(ymin = value - se,
                                          ymax = value + se),
                                      width = 0.25
                                      ) +
                        facet_wrap(~weight) + 
                        scale_y_continuous(breaks = seq(0, 1, 0.1),
                                           limits = c(0, 1.0),
                                           expand = expansion(mult = c(0, 0))
                                           ) +
                        scale_x_continuous(breaks = c(2005, 2009, 2013, 2017),
                                           labels = c("2005", "2009","2013", "2017")
                                           ) +
                        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                       '#753BBD', '#DF1995', '#FF6900', 
                                                       '#FBE122', '#000000',
                                                       '#91bbc2', '#caeb7e', '#81dbf1',
                                                       '#bfa5e1', '#f091cc', '#ffb581',
                                                       '#fdf29c', '#252443', '#a8a7b4',
                                                       '#c6dce0', '#e9f7ca')
                                            ) +
                        labs(y = "", x = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#dff7ff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing = unit(.5, units = "cm"),
                              plot.title = element_text(size=14, hjust = 5, vjust = -5, face="bold"),
                              legend.title = element_blank(),
                              axis.text.x=element_text(size=10),
                              axis.text.y=element_text(size=10),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.25,0.25,0.5,0.25), "cm")
                        )
                
                ggplotly(g) %>% 
                        layout(legend = list(title=list(text='<b> Industry: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h', yaxis = list(autorange = TRUE)) 
                        ) %>%  
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian') #'resetScale2d' 'autoScale2d'
                        )
                
                
        })
        
        
        output$tab2_table <- 
                renderDT({
                        tab2_reac2() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                        list(extend = 'copy',
                                                                             buttons = c('copy'),
                                                                             exportOptions = list(modifiers = list(page = "current"))
                                                                             ),
                                                                        list(extend = 'csv',
                                                                             buttons = c('csv'),
                                                                             exportOptions = list(modifiers = list(page = "current"))
                                                                             ),
                                                                        list(extend = 'print',
                                                                             buttons = c('print'),
                                                                             exportOptions = list(modifiers = list(page = "current"))
                                                                             ),
                                                                        list(extend = "collection",
                                                                             text = 'Show All',
                                                                             action = DT::JS(
                                                                                        "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                             ),
                                                                        list(extend = "collection",
                                                                             text = 'Show Less',
                                                                             action = DT::JS(
                                                                                        "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                             )
                                                                        )
                                                         )
                                          )
        }) 
        


        ### Tab 3; Aggregate change/decomposition ###
        tab3_reac1 <- 
                reactive({
                        dat_final <- dat_plot %>%
                                        dplyr::filter(cluster %in% input$tab3_input,
                                                      size == "All",
                                                      industry == "All"
                                                      ) %>% 
                                        mutate(practice_name = fct_rev(factor(practice_name)))
                        
                        return(dat_final)
                })
        
        tab3_reac2 <- 
                reactive({
                        dat_final <- dat %>% 
                                        dplyr::filter(cluster %in% input$tab3_input,
                                                      size == "All",
                                                      industry == "All"
                                                      ) %>% 
                                        dplyr::select(practice_name, cluster:weight,
                                                      aggregate_change, p_value, practice_shortname
                                                      ) %>% 
                                        dplyr::arrange(cluster ,practice_name, industry, size, weight)
                        
                        return(dat_final)
                })
        
        
        output$tab3_ui_plot <- renderUI({
                
                n_facets <- length(input$tab3_input) * 400
                plotlyOutput("tab3_plot", height = glue("{n_facets}px"))
                
        })
        
         output$tab3_plot <- renderPlotly({
                 shiny::validate(
                         need(input$tab3_input, "Input a value!")
                         )

                 g <-
                   tab3_reac1() %>% 
                         mutate(significance = ifelse(p_value < 0.1, TRUE,
                                                      FALSE)) %>% 
                         ggplot(aes(x = practice_name,
                                    y = aggregate_change,
                                    alpha = significance)
                                ) +
                         geom_bar(stat = "identity",
                                  position = "identity",
                                  colour = "#006272",
                                  fill = "#006272"
                                  ) +
                         geom_hline(yintercept = 0, size = 0.45) +
                         scale_y_continuous(
                                 breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15),
                                 limits = c(-0.13, 0.13),
                                 expand = expansion(mult = c(0, 0.035))
                                 ) +
                         scale_alpha_manual(values = c(0,1)) +
                         facet_grid(cluster ~ weight, scales = "free_y", space = "free") +
                         coord_flip() +
                         labs(title = "", x = "", y = "") +
                         theme_bw() +
                         theme(panel.grid.major.y = element_line(colour = "#e5faff"),
                               panel.grid.minor.y = element_blank(),
                               panel.grid.major.x = element_blank(),
                               panel.grid.minor.x = element_blank(),
                               panel.background = element_blank(),
                               panel.spacing.x = unit(.15, units = "cm"),
                               panel.spacing.y = unit(.15, units = "cm"),
                               plot.title = element_text(size=14, face="bold"),
                               legend.title = element_blank(),
                               legend.position = "none",
                               axis.text.x=element_text(size=11, hjust = -0.5),
                               axis.text.y=element_text(size=9.5),
                               axis.title = element_text(size=12,face="bold"),
                               strip.background = element_rect(fill = "#dfe3e6"),
                               strip.text = element_text(size = 14, face="bold"),
                               plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                               )               
                 
                 ggplotly(g, tooltip = c("aggregate_change", "significance")) %>%
                         # layout(uniformtext=list(minsize=8, mode='show'),
                         #        yaxis = list(ticklen = 20, tickcolor = "transparent")
                         #        ) %>%  
                         config(displaylogo = FALSE,
                                displayModeBar = TRUE,
                                modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                           'lasso2d','zoomIn2d','zoomOut2d',
                                                           'toggleSpikelines', 'hoverClosestCartesian', 
                                                           'hoverCompareCartesian')
                                )

         })

         output$tab3_table <- 
                 renderDT({
                         tab3_reac2() %>% 
                                 datatable(extensions = 'Buttons', 
                                           rownames = FALSE,
                                           options = list(dom = 'Brtip',
                                                          lengthMenu = list(c(5, 15, -1), 
                                                                            c('5', '15', 'All')),
                                                          pageLength = 10,
                                                          buttons = list(
                                                                  list(extend = 'copy',
                                                                       buttons = c('copy'),
                                                                       exportOptions = list(modifiers = list(page = "current"))
                                                                       ),
                                                                  list(extend = 'csv',
                                                                       buttons = c('csv'),
                                                                       exportOptions = list(modifiers = list(page = "current"))
                                                                       ),
                                                                  list(extend = 'print',
                                                                       buttons = c('print'),
                                                                       exportOptions = list(modifiers = list(page = "current"))
                                                                       ),
                                                                  list(extend = "collection",
                                                                       text = 'Show All',
                                                                       action = DT::JS(
                                                                               "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                       ),
                                                                  list(extend = "collection",
                                                                       text = 'Show Less',
                                                                       action = DT::JS(
                                                                               "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                       )
                                                                  )
                                                          )
                                           )
        }) 
         

        ### Tab 4; industry decomposition ###
         tab4_reac1 <- 
                 reactive({
                         dat_final <- dat2_plot %>% 
                                        dplyr::filter(cluster %in% input$tab4_input) %>% 
                                        mutate(practice_name = fct_rev(factor(practice_name)))
                         
                         return(dat_final)
                 })
         
         tab4_reac2 <- 
                 reactive({
                         dat_final <- dat2 %>% 
                                        dplyr::select(
                                                practice_name, cluster, size, 
                                                industry, weight, decomp_within, 
                                                decomp_across, decomp_overall, 
                                                p_value, practice_shortname 
                                                ) %>% 
                                        dplyr::filter(cluster %in% input$tab4_input) %>% 
                                        dplyr::arrange(cluster ,practice_name, industry, size, weight)
                         
                         return(dat_final)
                 })
        
        output$tab4_ui_plot <- renderUI({
                
                n_facets <- length(input$tab4_input) * 400
                plotlyOutput("tab4_plot", height = glue("{n_facets}px"))
                
        })
        
        output$tab4_plot <- renderPlotly({
                shiny::validate(
                        need(input$tab4_input, "Input a value!")
                        )
                
                g <- 
                        tab4_reac1() %>% 
                        ggplot(aes(
                                x = practice_name,
                                y = Decomposition,
                                fill = Type,
                                colour = Type,
                                alpha = significance)
                               ) +
                        geom_bar(stat = "identity") +
                        geom_hline(yintercept = 0, size = 0.45) +
                        scale_y_continuous(
                                breaks = c(-0.15,-0.1,-0.05, 0, 0.05, 0.1, 0.15),
                                limits = c(-0.1375, 0.1375),
                                expand = expansion(mult = c(0, 0.035))
                                ) +
                        scale_fill_manual(values = c('#97D700', '#006272'), guide = "none") +
                        scale_colour_manual(values = c('#97D700', '#006272'), guide = "none") +
                        scale_alpha_manual(values = c(0, 1), guide = "none") +
                        facet_grid(cluster ~ weight, scales = "free_y", space = "free") +
                        coord_flip() +
                        labs(title = "",x = "", y = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#e5faff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing.x = unit(.15, units = "cm"),
                              panel.spacing.y = unit(.15, units = "cm"),
                              plot.title = element_text(size=14, face="bold"),
                              legend.title = element_blank(),
                              legend.position = "none",
                              axis.text.x=element_text(size=11, hjust = -0.5),
                              axis.text.y=element_text(size=9.5),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                              )


                ggplotly(g, tooltip = c("Decomposition", "fill", "significance")) %>%
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian')
                        )

        })

        output$tab4_table <-
                renderDT({
                        tab4_reac2() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                 list(extend = 'copy',
                                                                      buttons = c('copy'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'csv',
                                                                      buttons = c('csv'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'print',
                                                                      buttons = c('print'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show All',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show Less',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                 )
                                                         )
                                          )
                                )
                })

        ### Tab 5; Decomp by dynamics ###
        tab5_reac1 <- 
                reactive({
                        dat_final <- dat3_plot %>% 
                                        dplyr::filter(practice_name %in% input$tab5_input) 
                        
                        return(dat_final)
                })
        
        tab5_reac2 <- 
                reactive({
                        dat_final <- dat3 %>% 
                                        dplyr::select(
                                                practice_name, cluster, size, 
                                                industry, weight, `Continuers - within`, 
                                                `Continuers - across`, Entrants,
                                                Exiters, Joiners, Leavers, Total,
                                                p_value, practice_shortname) %>%
                                        dplyr::filter(practice_name %in% input$tab5_input)
                        
                        return(dat_final)
                })
        
        output$tab5_ui_plot <- renderUI({
                
                n_facets <- length(input$tab5_input) * 400
                plotlyOutput("tab5_plot", height = glue("{n_facets}px"))
                
        })

        output$tab5_plot <- renderPlotly({
                shiny::validate(
                        need(input$tab5_input, "Input a value!")
                )
                
                alpha_scale <- 
                        if(length(unique(tab5_reac1()$significance)) == 1 & 
                           sum(tab5_reac1()$significance) > 0) {
                                c(1,0) 
                                } else{ 
                                        c(0,1)
                                        }
                
                g <- 
                        tab5_reac1() %>% 
                        ggplot(aes(
                                x = Group,
                                y = Decomposition,
                                alpha = significance)
                        ) +
                        geom_bar(stat = "identity",
                                 position = "identity",
                                 colour = '#006272',
                                 fill = '#006272') +
                        geom_hline(yintercept = 0, size = 0.45) +
                        scale_y_continuous(
                                breaks = c(-0.08, -0.06, -0.04, -0.02, 0, 0.02,
                                           0.04, 0.06, 0.08),
                                limits =  c(min(tab5_reac1()$limit_min),
                                            max(tab5_reac1()$limit_max)),
                                expand = expansion(mult = c(0, 0.035))
                        ) +
                        scale_alpha_manual(values = alpha_scale, guide = "none") +
                        facet_grid(practice_shortname ~ weight, scales = "free_y", space = "free") +
                        coord_flip() +
                        labs(title = "",x = "", y = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#e5faff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing.x = unit(.25, units = "cm"),
                              panel.spacing.y = unit(.15, units = "cm"),
                              plot.title = element_blank(),
                              legend.title = element_blank(),
                              legend.position = "none",
                              axis.text.x = element_text(size=11, hjust = -0.5),
                              axis.text.y = element_text(size=11),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text.x = element_text(size = 14, face="bold"),
                              strip.text.y = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                        )
                
                
                ggplotly(g, tooltip = c("Decomposition", "significance")) %>%
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian')
                        )
                
        })

        output$tab5_table <-
                renderDT({
                        tab5_reac2() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                 list(extend = 'copy',
                                                                      buttons = c('copy'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'csv',
                                                                      buttons = c('csv'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'print',
                                                                      buttons = c('print'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show All',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show Less',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                 )
                                                         )
                                          )
                                )
                })
        
        ### Tab 6; Decomp by dynamics; size and industry ###
        tab6_reac1 <- 
                reactive({
                        
                        dat_s <- dat4_plot %>% 
                                        dplyr::filter(practice_name %in% input$tab6_input,
                                                      size %in% input$tab6_size,
                                                      industry == "All"
                                                      ) 
                        
                        dat_i <- dat4_plot %>% 
                                        dplyr::filter(practice_name %in% input$tab6_input,
                                                      size == "All",
                                                      industry %in% input$tab6_industry
                                                      ) 
                        
                        dat_final <- rbind(dat_s, dat_i) %>% 
                                        distinct() 
                        
                        return(dat_final)
                })
        
        
        tab6_reac2<- 
                reactive({
                        
                        dat_s <- dat4 %>% 
                                        dplyr::select(practice_name, cluster, size, 
                                                      industry, weight, `Continuers - within`, 
                                                      `Continuers - across`, `Continuers - enter`, 
                                                      `Continuers - exit`, Entrants, Exiters, 
                                                      Joiners, Leavers, Total, p_value, practice_shortname
                                                      ) %>% 
                                        dplyr::filter(practice_name %in% input$tab6_input,
                                                      size %in% input$tab6_size,
                                                      industry == "All"
                                                      ) 
                        
                        dat_i <- dat4 %>% 
                                        dplyr::select(practice_name, cluster, size, 
                                                      industry, weight, `Continuers - within`, 
                                                      `Continuers - across`, `Continuers - enter`, 
                                                      `Continuers - exit`, Entrants, Exiters, 
                                                      Joiners, Leavers, Total, p_value, practice_shortname
                                                      ) %>% 
                                        dplyr::filter(practice_name %in% input$tab6_input,
                                                      size == "All",
                                                      industry %in% input$tab6_industry
                                                      ) 
                        
                        dat_final <- rbind(dat_s, dat_i) %>% 
                                        distinct() %>% 
                                        relocate(practice_shortname, .after = last_col())
                        
                        return(dat_final)
                })
        
        output$tab6_ui_plot_size <- renderUI({
                
                n_size <- length(input$tab6_size) * 200 + 200
                plotlyOutput("tab6_plot_size", height = glue("{n_size}px"))
                
        })
        
        
        output$tab6_plot_size <- renderPlotly({
                shiny::validate(
                        need(input$tab6_size, "Input a value!")
                )
                
                alpha_scale <- 
                        if(length(
                                unique(
                                        dplyr::filter(
                                                tab6_reac1(), industry == "All")$significance2)) == 1 
                           & sum(dplyr::filter(
                                   tab6_reac1(), industry == "All")$significance2) > 0) { 
                                c(1,0) 
                        } else{
                                c(0,1) }
                
                g <- 
                        tab6_reac1() %>% 
                        dplyr::filter(industry == "All") %>% 
                        ggplot(aes(
                                x = Group,
                                y = Decomposition,
                                fill = size,
                                colour = size,
                                alpha = significance)
                        ) +
                        geom_bar(stat = "identity",
                                 position = position_dodge2(preserve = "single",
                                                            padding =  0.075),
                                 width = 0.8) +
                        geom_hline(yintercept = 0, size = 0.45) +
                        scale_y_continuous(
                                breaks = seq(-0.4, 0.4, by = 0.02),
                                limits = c(min(tab6_reac1()$limit_min),
                                           max(tab6_reac1()$limit_max)),
                                expand = expansion(mult = c(0, 0))
                                ) +
                        scale_fill_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                       '#753BBD'), guide = "none") +
                        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                     '#753BBD'), guide = "none") +
                        scale_alpha_manual(values = alpha_scale) +
                        facet_grid(practice_shortname ~ weight, scales = "free_y", space = "free") +
                        coord_flip() +
                        labs(title = "",x = "", y = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#e5faff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing.x = unit(.25, units = "cm"),
                              panel.spacing.y = unit(.15, units = "cm"),
                              plot.title = element_blank(),
                              legend.title = element_blank(),
                              axis.text.x = element_text(size=11, hjust = -0.5),
                              axis.text.y = element_text(size=11),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text.x = element_text(size = 14, face="bold"),
                              strip.text.y = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                        )
                
                
                ggplotly(g, tooltip = c("Decomposition", "significance", "fill")) %>%
                        layout(legend = list(title=list(text='<b> Size: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h')) %>% 
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian')
                        ) 
                
        })
        
        output$tab6_ui_plot_ind <- renderUI({
                
                n_industry <- length(input$tab6_industry) * 200 + 200
                plotlyOutput("tab6_plot_ind", height = glue("{n_industry}px"))
                
        })
        
        
        output$tab6_plot_ind <- renderPlotly({
                shiny::validate(
                        need(input$tab6_industry, "Input a value!")
                )
                
                alpha_scale <- 
                        if(length(
                                unique(
                                        dplyr::filter(
                                                tab6_reac1(), size == "All")$significance2)) == 1 
                           & sum(dplyr::filter(
                                   tab6_reac1(), size == "All")$significance2) > 0) { 
                                c(1,0) 
                        } else{
                                c(0,1) }
                
                g <- 
                        tab6_reac1() %>% 
                        dplyr::filter(size == "All") %>% 
                        ggplot(aes(
                                x = Group,
                                y = Decomposition,
                                fill = industry,
                                colour = industry,
                                alpha = significance)
                        ) +
                        geom_bar(stat = "identity",
                                 position = position_dodge2(preserve = "single",
                                                            padding =  0.075),
                                 width = 0.8) +
                        geom_hline(yintercept = 0, size = 0.45) +
                        scale_y_continuous(
                                breaks = seq(-0.4, 0.4, by = 0.02),
                                limits = c(min(tab6_reac1()$limit_min),
                                           max(tab6_reac1()$limit_max)),
                                expand = expansion(mult = c(0, 0))
                        ) +
                        
                        scale_fill_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                       '#753BBD', '#DF1995', '#FF6900', 
                                                       '#FBE122', '#000000', '#91bbc2', 
                                                       '#caeb7e', '#81dbf1', '#bfa5e1',
                                                       '#f091cc', '#ffb581', '#fdf29c')
                        ) +
                        scale_colour_manual(values = c('#006272', '#97D700', '#00B5E2', 
                                                     '#753BBD', '#DF1995', '#FF6900', 
                                                     '#FBE122', '#000000', '#91bbc2', 
                                                     '#caeb7e', '#81dbf1', '#bfa5e1',
                                                     '#f091cc', '#ffb581', '#fdf29c'),
                                            guide = "none"
                                            ) +
                        scale_alpha_manual(values = alpha_scale, guide = "none") +
                        facet_grid(practice_shortname ~ weight, scales = "free_y", space = "free") +
                        coord_flip() +
                        labs(title = "",x = "", y = "") +
                        theme_bw() +
                        theme(panel.grid.major.y = element_line(colour = "#e5faff"),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.background = element_blank(),
                              panel.spacing.x = unit(.25, units = "cm"),
                              panel.spacing.y = unit(.15, units = "cm"),
                              plot.title = element_blank(),
                              legend.title = element_blank(),
                              axis.text.x = element_text(size=11, hjust = -0.5),
                              axis.text.y = element_text(size=11),
                              axis.title = element_text(size=12,face="bold"),
                              strip.background = element_rect(fill = "#dfe3e6"),
                              strip.text.x = element_text(size = 14, face="bold"),
                              strip.text.y = element_text(size = 14, face="bold"),
                              plot.margin = unit(c(0.5,0.5,0.25,0.25), "cm")
                        )
                
                
                ggplotly(g, tooltip = c("Decomposition", "significance", "fill")) %>%
                        layout(legend = list(title=list(text='<b> Industry: </b>'),
                                             x = 0.0225, y = -0.125, 
                                             orientation = 'h')) %>% 
                        config(displaylogo = FALSE,
                               displayModeBar = TRUE,
                               modeBarButtonsToRemove = c('zoom2d','pan2d','select2d',
                                                          'lasso2d','zoomIn2d','zoomOut2d',
                                                          'toggleSpikelines', 'hoverClosestCartesian', 
                                                          'hoverCompareCartesian')
                        )
                
        })
        
        output$tab6_table <-
                renderDT({
                        tab6_reac2() %>% 
                                datatable(extensions = 'Buttons', 
                                          rownames = FALSE,
                                          options = list(dom = 'Brtip',
                                                         lengthMenu = list(c(5, 15, -1), 
                                                                           c('5', '15', 'All')),
                                                         pageLength = 10,
                                                         buttons = list(
                                                                 list(extend = 'copy',
                                                                      buttons = c('copy'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'csv',
                                                                      buttons = c('csv'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = 'print',
                                                                      buttons = c('print'),
                                                                      exportOptions = list(modifiers = list(page = "current"))
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show All',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(-1);
                                                                                        dt.ajax.reload();
                                                                                        }")
                                                                 ),
                                                                 list(extend = "collection",
                                                                      text = 'Show Less',
                                                                      action = DT::JS(
                                                                              "function ( e, dt, node, config ) {
                                                                                        dt.page.len(10);
                                                                                        dt.ajax.reload();}")
                                                                 )
                                                         )
                                          )
                                )
                })


})

