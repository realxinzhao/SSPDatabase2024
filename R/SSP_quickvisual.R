library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
require(readr)
require(readxl)
# gcamdata is requred to use gcamdata::left_join_error_no_match()

# ggplot theme ----

windowsFonts("Arial" = windowsFont("Arial"))
theme0 <- theme(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= "Arial", size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15,
                             vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  #axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10) ,
  panel.spacing = unit(0.5, "lines")
  #axis.title.x = element_blank()
)

## func export fig ----
outdir <- "output/"
Write_png <- function(.plot, name, w = 9000, h = 4500, r = 600){
  png(paste0(outdir,name,".png"), width = w, height = h, res = r)
  print(.plot)
  dev.off()
}



## Load AR6 source data ----

# need to download the source data from SSP data base explorer
#SSP <- readxl::read_excel("data/SSP/SSP-Review-Phase-1.xlsx", sheet = 2)

# SSP %>% filter(Variable %in% c("Population", "GDP|PPP")) %>% saveRDS("data/SSP/SSP_popgdp.RDS")

# Otherwise use saved RDS (smaller)
SSP <- readRDS("data/SSP/SSP_popgdp.RDS")

# mappings----
iso_GCAM_regID <- readr::read_csv("data/SSP/iso_GCAM_regID.csv", comment = "#")
Regmapping <- readr::read_csv("data/SSP/Regmapping.csv", comment = "#")

# Create a region mapping to GCAM or more aggregated regions
SSP %>% distinct(Region) %>%
  filter(!grepl("\\(|World", Region)) %>%
  mutate(dplyr::across(c(Region), iconv, to = 'ASCII//TRANSLIT')) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "\\'", replacement = "")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "\\?", replacement = "")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Laos", replacement = "Lao Peoples Democratic Republic")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "United States", replacement = "United States of America")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "South Korea", replacement = "Korea, Republic of")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "North Korea", replacement = "Korea, Democratic Peoples Republic of")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Runion", replacement = "Reunion")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Cte dIvoire", replacement = "Cote dIvoire")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Libya", replacement = "Libyan Arab Jamahiriya")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Syria", replacement = "Syrian Arab Republic")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Iran", replacement = "Iran, Islamic Republic of")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Czechia", replacement = "Czech Republic")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Tanzania", replacement = "Tanzania, United Republic of")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "North Macedonia", replacement = "Macedonia, the former Yugoslav Republic of")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Democratic Republic of the Congo", replacement = "Congo, the Democratic Republic of the")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Moldova", replacement = "Moldova, Republic of")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Macao", replacement = "Macau")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Micronesia", replacement = "Micronesia, Federated States of")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Antigua and Barbuda", replacement = "Antigua & Barbuda")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Cabo Verde", replacement = "Cape Verde")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Macao", replacement = "Macau")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Palestine", replacement = "Palestinian Territory, Occupied")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Timor-Leste", replacement = "Timor Leste")) %>%
  mutate(dplyr::across(c(Region), gsub, pattern = "Curaao", replacement = "Curacao")) %>%
  # map Eswatini to Mozambique
  mutate(dplyr::across(c(Region), gsub, pattern = "Eswatini", replacement = "Mozambique")) %>%
  gcamdata::left_join_error_no_match(iso_GCAM_regID %>% select(Region = country_name, GCAM_region_ID) %>% distinct()) %>%
  mutate(RegID = paste0("G", GCAM_region_ID)) %>%
  left_join(Regmapping %>% select(RegID, REG5_AR6)) ->
  SSPRegMapping


SSP %>% filter(Variable %in% c("Population", "GDP|PPP")) %>%
  mutate(Scenario = gsub(" \\(2013\\)| - Review Phase 1", "", Scenario),
         Version = if_else(grepl("2013", Model), "Old2013", "New2023"),
         Model = gsub(" 2013| 2023| GDP| POP", "", Model)) %>%
  right_join(
    SSPRegMapping %>% distinct(Region, REG5_AR6)
  ) %>%
  #filter(grepl("R5|World", Region)) %>%
  gcamdata::gather_years() %>% drop_na() %>%
  group_by_at(vars(-value, -Region)) %>%
  summarize(value = sum(value),.groups = "drop") %>%
  rename(Region = REG5_AR6)->
  SSP_POP_GDP


SSP_POP_GDP %>% filter(Variable == "Population", Model == "IIASA-WiC") %>%
  mutate(value = value / 1000) %>%
  spread(Version, value) %>%
  mutate(Change = New2023 - Old2013) %>%
  gather(Version, value, Change, New2023, Old2013) %>%
  filter(Region != "World") %>%
  ggplot() + facet_grid(Version~Scenario, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = Region), stat = "identity", color = 1, size = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Billion People", x = "Year") +
  theme_bw() + theme0 +
  theme(panel.grid.major = element_line(linetype = 5, size = 0.2, color = "grey"),
        axis.text.x = element_text(size = 13))+
  ggtitle("Changes in SSP population growth projections (New2023 vs. Old2013; column panels)") +
  labs(subtitle = "1 Models (IIASA-WiCD) \n4 SSP(1-5; column panels)\n5 Region (R5; filled bars)") +
  labs(caption = c(
    "\nData source: SSP Database 2024 (Review Phase1 for New2023)\nVariable: Population")) -> pp

pp %>% Write_png("SSPUpdates_POP", h = 3000, w = 4400, r = 300)


SSP_POP_GDP %>% filter(Variable == "GDP|PPP", Model == "IIASA") %>%
  mutate(value = value / 1000) %>%
  #filter(Region == "World") %>% #filter(Version == "New2023") %>%
  ggplot() + facet_grid(Region~Scenario, scales = "fixed") +
  geom_line(aes(x = year, y = value, linetype = Version, color = Model,
                group = interaction(Model, Scenario, Version)), size = 1.2) +
  labs(y = "Billion People", x = "Year") +
  theme_bw() + theme0

library(RColorBrewer)
library(scales)

c(brewer.pal(5, "Set1"), "black") -> mycol

SSP_POP_GDP %>% filter(Variable == "GDP|PPP") %>%
  select(-Unit) %>% filter(year >= 2010) %>%
  group_by_at(vars(-value, -year)) %>%
  mutate(value = value / value[year == 2025]) %>% ungroup() %>%
  spread(Version, value) %>%
  mutate(`Change (Old = 1)` = New2023 / Old2013) %>%
  gather(Version, value, `Change (Old = 1)`, New2023, Old2013)-> df

df %>% filter(Version != "Change (Old = 1)") %>%
  ggplot() + facet_grid(Model~Scenario, scales = "free_y") +
  geom_line(aes(x = year, y = value, linetype = Version, color = Region,
                group = interaction(Model, Scenario, Version, Region)), size = 1.2) +
  labs(y = "2025 = 1", x = "Year") +
  scale_color_manual(values = mycol) +
  theme_bw() + theme0 +
  theme(panel.grid.major = element_line(linetype = 5, size = 0.2, color = "grey"),
        axis.text.x = element_text(size = 13), panel.spacing.y = unit(0.8, "lines")) +
  ggtitle("(B) SSP GDP growth updates (New2023 vs. Old2013; linetypes)") +
  labs(subtitle = "2 Models (IIASA & OECD; row panels) \n4 SSP(1-5; column panels)\n5 Region (R5; color)") +
  labs(caption = c(
    "\nData source: SSP Database 2024 (Review Phase1 for New2023)\nVariable: GDP|PPP")) -> p2


df %>% filter(Version == "Change (Old = 1)") %>%
  ggplot() + facet_grid(Model~Scenario, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = 5) +
  geom_line(aes(x = year, y = value, linetype = Version, color = Region,
                group = interaction(Model, Scenario, Version, Region)), size = 1.2) +
  labs(y = "Old2013 = 1", x = "Year") +
  scale_color_manual(values = mycol) +
  theme_bw() + theme0 +
  theme(panel.grid.major = element_line(linetype = 5, size = 0.2, color = "grey"),
        axis.text.x = element_text(size = 13), panel.spacing.y = unit(0.8, "lines"))+
  ggtitle("(A) Changes in SSP GDP growth projections (Old2013 = 1; lines)") +
  labs(subtitle = "2 Models (IIASA & OECD; row panels) \n4 SSP(1-5; column panels)\n5 Region (R5; color)") +
  labs(caption = c(
    "\nData source: SSP Database 2024 (Review Phase1 for New2023)\nVariable: GDP|PPP")) -> p1

(p1 + theme(legend.position = "none")) / p2 + patchwork::plot_layout(guides = "collect") -> pp

pp %>% Write_png("SSPUpdates_GDP", h = 5000, w = 4400, r = 300)





