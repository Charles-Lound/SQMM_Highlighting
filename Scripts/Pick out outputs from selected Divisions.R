
# Pick out outputs from selected Divisions




library(tidyverse)
library(readxl)
library(plotly)


username <- Sys.getenv("USERPROFILE")

sqmmwkbk <- paste0(username, "/Office for National Statistics/Data Quality Hub - ONS_Quality_Reviews_(formerly_deep_dives)/Pause&Review/2023-11-14_SQMM2023_results (pause & review work).xlsx")
sqmmsheet <- "detailed_summary_results"

highlight = c("Analytical Hub", "Crime Income and Wealth", "Sub-National Statistics and Analysis")

detailed_results_wide <- read_excel(sqmmwkbk, sheet = sqmmsheet) %>%
  select(sqmm2023_expected_return_name, division_2023, mean_score, sources_score, meth.sys_score, proc_score,users.ppl_score, qual_score) %>%
  rename(Output=sqmm2023_expected_return_name) %>%
  mutate('1: Sources'  = sources_score,
         '2: Methods & Systems' = meth.sys_score,
         '3: Processes'= proc_score,
         '4: Users & People' = users.ppl_score,
         '5: Statistical Quality'= qual_score,
         'Overall'  = mean_score
         ) %>%
  mutate(Division = ifelse(division_2023 %in% highlight, division_2023, "Other")) %>%
  mutate(Division = factor(Division, levels = c(highlight, "Other")))%>%
  mutate(dotsize = ifelse(division_2023 %in% highlight, 1.5, 1)) %>%
  mutate(dotcolor = ifelse(division_2023 %in% highlight, division_2023, "gray")) %>%
  select(!c(mean_score, sources_score, meth.sys_score, proc_score,users.ppl_score, qual_score))
  
detailed_results <-  pivot_longer(detailed_results_wide, cols=c('1: Sources','2: Methods & Systems','3: Processes','4: Users & People','5: Statistical Quality','Overall'), names_to = "Theme", values_to = "Score")%>%
                     arrange(desc(Division))

# To do: automate the colours
# values=c("#1b9e77","#d95f02","#7570b3","#e7298a", "gray")

p <-ggplot(detailed_results, aes(x=Theme, y=Score, label= Output, color=Division)) +
    geom_point(position = position_jitter(seed=1)) + theme_minimal()+scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3", "gray"))+
    ylim(1,5) + theme(legend.position="none")

ggplotly(p)


q <-ggplot(subset(detailed_results,Theme =="5: Statistical Quality"), aes(x=Score, y=Theme, label= Output, color=Division, size = dotsize)) +
  geom_point(position = position_jitter(seed=1)) + theme_minimal()+scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3", "gray"))+
  xlim(1,5) + theme(legend.position="none", axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))+
  geom_text(aes(label=ifelse(Score < 0 & Division %in% highlight,Output,'')), position = position_jitter(seed=1), vjust=30)

ggplotly(q)

detailed_results_wide$sextilegp <- ntile(detailed_results_wide$Overall, 6)
t <- table(detailed_results_wide$sextilegp, detailed_results_wide$Division)

write.table(t, file='ppa.txt')
