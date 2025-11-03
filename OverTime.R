packages <- c(
  "rstudioapi", "stringr", "ggplot2", "cowplot", "tidyr", "dplyr", "data.table", "tibble", "rstatix",
  "readxl", "writexl", "gridExtra", "broom", "lme4", "afex"
)
invisible(lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}))

rm(list = ls())

skeleton <- list(
  c("eyeleft", "eyeright"),
  c("eyeleft", "nose"),
  c("eyeright", "nose"),
  c("nose", "neck"),
  c("neck", "shoulderleft"),
  c("neck", "shoulderright"),
  c("shoulderright", "elbowright"),
  c("elbowright", "wristright"),
  c("wristright", "knuckleright"),
  c("knuckleright", "fingerright"),
  c("shoulderleft", "elbowleft"),
  c("elbowleft", "wristleft"),
  c("wristleft", "knuckleleft"),
  c("knuckleleft", "fingerleft"),
  c("shoulderright", "hipright"),
  c("hipright", "kneeright"),
  c("kneeright", "ankleright"),
  c("ankleright", "heelright"),
  c("ankleright", "toeright"),
  c("heelright", "toeright"),
  c("shoulderleft", "hipleft"),
  c("hipleft", "hipright"),
  c("hipleft", "kneeleft"),
  c("kneeleft", "ankleleft"),
  c("ankleleft", "heelleft"),
  c("ankleleft", "toeleft"),
  c("heelleft", "toeleft")
)


script_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
df_list <- list()
all_files <- list.files(path = script_path, pattern = "*.csv")
specific_files <- all_files %>% sort()

for (file in specific_files) {
  temp_df <- read.csv(file.path(script_path, file), header = TRUE)

  names(temp_df) <- paste(temp_df[1, ], temp_df[2, ], sep = "_")
  temp_df <- temp_df[-c(1, 2), ]
  df_list[[length(df_list) + 1]] <- temp_df %>%
    mutate(filename = file %>% str_extract(".*(?=.csv)"), .before = bodyparts_coords)
}

shortDF<- do.call(rbind, df_list) 

combined_df  = shortDF %>%
  pivot_longer(
    cols = -c(bodyparts_coords, filename),
    names_to = c("body", "coords"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    bodyparts_coords = bodyparts_coords %>% as.numeric(),
    value = value %>% as.numeric()
  ) %>%
  pivot_wider(
    names_from = coords,
    values_from = value
  )

# Calc CoM
# 1. Define Body Segments
# Head: midpoint between top of head and neck
# Torso: midpoint between shoulders and hips
# Upper arm: midpoint between shoulder and elbow
# Forearm: midpoint between elbow and wrist
# Thigh: midpoint between hip and knee
# Calf: midpoint between knee and ankle
# 2. Assign Mass Proportions
# Use standard values (from Winter, Dempster, etc.). Example:
#   
# Head: 8%
# Torso: 50%
# Each arm: 5%
# Each forearm: 3%
# Each thigh: 10%
# Each calf: 4%

CoMDF = shortDF %>%
  mutate(across(-filename, as.numeric))%>%
  mutate(head_x = nose_x, head_y = nose_y,
         head_w = 8/100,
         
         torso_x <- mean(c(shoulderleft_x, shoulderright_x, hipright_x, hipleft_x)),
         torso_y <- mean(c(shoulderleft_y, shoulderright_y, hipright_y, hipleft_y)),
         torso_w = .5
         
         upperarmleft_x <- mean(c(shoulderleft_x, elbowleft_x)), upperarmright_x <- mean(c(shoulderright_x, elbowright_x)),
         upperarmleft_y <- mean(c(shoulderleft_y, elbowleft_y)), upperarmright_y <- mean(c(shoulderright_y, elbowright_y)),
         
         forearmleft_x <- mean(c(wristleft_x, elbowleft_x)), forearmright_x <- mean(c(wristright_x, elbowright_x)),
         forearmleft_y <- mean(c(wristleft_y, elbowleft_y)), forearmright_y <- mean(c(wristright_y, elbowright_y)),
         
         thighleft_x <- mean(c(hipleft_x, kneeleft_x)), thighright_x <- mean(c(hipright_x, kneeright_x)),
         thighleft_y <- mean(c(hipleft_y, kneeleft_y)), thighright_y <- mean(c(hipright_y, kneeright_y)),
         
         calfleft_x <- mean(c(ankleleft_x, kneeleft_x)), calfright_x <- mean(c(ankleright_x, kneeright_x)),
         calfleft_y <- mean(c(ankleleft_y, kneeleft_y)), calfright_y <- mean(c(ankleright_y, kneeright_y)),
         
  ) %>%
  mutate(CoM)

ggplot(combined_df %>% filter(body %in% c("toeleft", "toeright", 
                                          "wristright", #"wristleft", 
                                          "eyeleft"#, "eyeright"
                                          ))) +
  geom_point(aes(x = x, y = y, color = bodyparts_coords), show.legend = F) +
  facet_wrap(body ~ filename, ncol=length(unique(combined_df$filename))) +
  scale_x_reverse() + scale_y_reverse()

x_min <- min(combined_df$x, na.rm = TRUE)
x_max <- max(combined_df$x, na.rm = TRUE)
y_min <- min(combined_df$y, na.rm = TRUE)
y_max <- max(combined_df$y, na.rm = TRUE)

for (i in seq(0,60)) {
  
  
  filtered_df <- combined_df %>% filter(bodyparts_coords == i)
  
  
  # ggplot(filtered_df) +
  #   geom_point(aes(x = x, y = y, color = bodyparts_coords), color= "white",show.legend = F) +
  #   scale_x_reverse(limits = c(x_max,x_min), breaks = seq.int(x_min,x_max,length.out=3) %>% round()) +
  #   scale_y_reverse(limits = c(y_max,y_min), breaks = seq.int(y_min,y_max,length.out=5) %>% round())+
  #   facet_wrap( ~ filename, ncol=length(unique(combined_df$filename)), drop = F) +
  #   theme_void()+
  #   theme(
  #     plot.background = element_rect(fill = "black", color = NA),
  #     panel.background = element_rect(fill = "black", color = NA),
  #     panel.border = element_rect(color = "white", fill = NA, linewidth = 0.5),
  #     strip.background = element_rect(fill = "black", color = "white"),  # Box around facet label
  #     strip.text = element_text(color = "white", size = 16),                        # Facet label text in white
  #     axis.text = element_text(color = "white"),
  #     axis.title = element_text(color = "white"),
  #     text = element_text(color = "white")
  #   )
  
  
  # Create connection data
  connections <- do.call(rbind, lapply(skeleton, function(pair) {
    part1 <- filtered_df %>% filter(body == pair[1])
    part2 <- filtered_df %>% filter(body == pair[2])
    inner_join(part1, part2, by = "filename", suffix = c("_1", "_2")) %>%
      mutate(pair = paste(pair, collapse = "-"))
  }))
  
  
  # Plot with facet_wrap
  ggplot() +
    geom_point(data = filtered_df, aes(x = x, y = y), color = "white") +
    geom_segment(data = connections, aes(x = x_1, y = y_1, xend = x_2, yend = y_2), color = "white") +
    facet_wrap(~filename,nrow=1) +
    scale_x_reverse(limits = c(x_max,x_min), breaks = seq.int(x_min,x_max,length.out=3) %>% round()) +
    scale_y_reverse(limits = c(y_max,y_min), breaks = seq.int(y_min,y_max,length.out=5) %>% round())+
    facet_wrap( ~ filename, ncol=length(unique(combined_df$filename)), drop = F) +
    theme_void()+
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      panel.border = element_rect(color = "white", fill = NA, linewidth = 0.5),
      strip.background = element_rect(fill = "black", color = "white"),  # Box around facet label
      strip.text = element_text(color = "white", size = 16),                        # Facet label text in white
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      text = element_text(color = "white")
    )
  
  
  
  ggsave(paste0(sprintf("%03d", i),".png"), 
         path=file.path(script_path,"pngs"))
  
}


############################

# Create a data frame of connections
connections <- do.call(rbind, lapply(skeleton, function(pair) {
  part1 <- filtered_df %>% filter(body == pair[1])
  part2 <- filtered_df %>% filter(body == pair[2])
  if (nrow(part1) > 0 && nrow(part2) > 0) {
    data.frame(
      x = c(part1$x, part2$x),
      y = c(part1$y, part2$y),
      group = paste(pair, collapse = "-")
    )
  }
}))


# Create connection data
connections <- do.call(rbind, lapply(skeleton, function(pair) {
  part1 <- filtered_df %>% filter(body == pair[1])
  part2 <- filtered_df %>% filter(body == pair[2])
  inner_join(part1, part2, by = "filename", suffix = c("_1", "_2")) %>%
    mutate(pair = paste(pair, collapse = "-"))
}))


# Plot with facet_wrap
ggplot() +
  geom_point(data = filtered_df, aes(x = x, y = y), color = "white") +
  geom_segment(data = connections, aes(x = x_1, y = y_1, xend = x_2, yend = y_2), color = "white") +
  facet_wrap(~filename,nrow=1) +
  scale_x_continuous(limits = c(x_min,x_max), breaks = seq.int(x_min,x_max,length.out=3) %>% round()) +
  scale_y_reverse(limits = c(y_max,y_min), breaks = seq.int(y_min,y_max,length.out=5) %>% round())+
  facet_wrap( ~ filename, ncol=length(unique(combined_df$filename)), drop = F) +
  theme_void()+
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    panel.border = element_rect(color = "white", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = "black", color = "white"),  # Box around facet label
    strip.text = element_text(color = "white", size = 16),                        # Facet label text in white
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    text = element_text(color = "white")
  )


