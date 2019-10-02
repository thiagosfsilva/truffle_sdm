  # -------------------------------------------------------------------------
  # evaluate - table and boxplot
  # mauricio vancine - mauricio.vancine@gmail.com
  # 26-06-2019
  # -------------------------------------------------------------------------
  
  # preparate r -------------------------------------------------------------
  # memory
  rm(list = ls())
  
  # packages
  library(tidyverse)
  library(wesanderson)
  
  # directory
  setwd("/home/mude/data/curso_mde_9cbh")
  dir()
  
  # import evaluate data ----------------------------------------------------
  # directory
  setwd("05_sdm_multiple")
  dir()
  
  # import
  eva <- purrr::map_df(dir(pattern = "eval_", recursive = TRUE), readr::read_csv)
  eva
  
  # evaluate analysis -------------------------------------------------------
  for(i in eva$species %>% unique){
    
    # directory
    setwd(i); setwd("01_evaluation")
    
    # select specie
    setwd("00_raw")
    eva_sp <- eva %>% 
      dplyr::filter(species == i)
    
    # tables
    # directory
    setwd("..")
    dir.create("01_tables")
    setwd("01_tables")
    
    # table
    eva_table <- eva %>% 
      dplyr::mutate(species = species %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) %>% 
      dplyr::group_by(species, algorithm) %>% 
      dplyr::summarise(tss_mean = mean(tss_spec_sens) %>% round(3), 
                       tss_sd = sd(tss_spec_sens) %>% round(3),
                       auc_mean = mean(auc) %>% round(3), 
                       auc_sd = sd(auc) %>% round(3))
    eva_table
    
    # export
    readr::write_csv(eva_table, paste0("evaluation_summary_table_", i, ".csv"))
    
    # boxplots
    # directory
    setwd("..")
    dir.create("02_boxplot")
    setwd("02_boxplot")
    
    for(j in c("tss_spec_sens", "auc")){
    
      # information
      print(paste(i, j))
      
      # plot  
      ggplot(data = eva_sp) + 
        aes_string(x = "algorithm", y = j, color = "algorithm") +
        geom_boxplot(size = .5, fill = "gray90", color = "black") +
        geom_jitter(width = 0.2, size = 4, alpha = .7) +
        scale_color_manual(values = wesanderson::wes_palette(name = "Darjeeling1", n = eva$algorithm %>% unique %>% length, 
                                                             type = "continuous")) +
        labs(x = "Algorithms", 
             y = stringr::str_to_upper(j) %>% stringr::str_replace("_", " "), 
             title = i %>% stringr::str_to_title() %>% stringr::str_replace("_", " ")) + 
        ylim(c(0, 1.05)) + 
        theme_bw() +
        geom_hline(yintercept = ifelse(j == "tss_spec_sens", .5, .75), color = "red") +
        theme(legend.position = "none",
              plot.title = element_text(face = "bold.italic", size = 20), 
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 15), 
              axis.title = element_text(size = 17))
      ggsave(paste0("boxplot_jitter_an_", j, "_", i, ".tiff"), he = 20, wi = 30, un = "cm", dpi = 300)
    
    }
    
  }
  
  # end ---------------------------------------------------------------------