
# Choose projection
projection <- 3857 # ATTAINS projection

source('src/getParkUnits.R')
source('src/getNPCA.R')
# Pull in NPCA regions
npca_regions <- getNPCA()  
# Pull in park boundaries
parks <- getParkUnits() %>%
  sf::st_transform(projection) %>%
  dplyr::select(UNIT_CODE)
park_lines <- readRDS('data/nps_boundary_lines.RDS')

attains_assmnt_parms <- sf::st_read(dsn = 'data/ATTAINS_Assessment_20220809.gpkg',
                                    layer = "attains_au_assmnt_parms")


# remove unnecessary columns
col_remove <- c("GRIDCODE", "submissionid", "GLOBALID", "orig_fid", "SOURCEFC", "Shape", "Shape_Area", "Shape_Length", "AreaSqKM")

# EXAMPLES OF EACH ATTAINS GEOSPATIAL TYPE
nps_attains_points <- readRDS('data/00_nps_attains_point.RDS')%>% dplyr::select(c(UNIT_CODE, nhdplusid, everything(.)))
nps_attains_lines <- readRDS('data/00_nps_attains_line.RDS') %>% dplyr::select(c(UNIT_CODE, nhdplusid, everything(.))) 
nps_attains_areas <- readRDS('data/00_nps_attains_area.RDS') %>% dplyr::select(c(UNIT_CODE, nhdplusid, everything(.))) 
nps_noncontus_attains_points <- readRDS('data/00_nps_noncontus_attains_point.RDS') %>% dplyr::select(c(UNIT_CODE, everything(.)))
nps_noncontus_attains_lines <- readRDS('data/00_nps_noncontus_attains_line.RDS') %>% dplyr::select(c(UNIT_CODE, everything(.))) 
nps_noncontus_attains_areas <- readRDS('data/00_nps_noncontus_attains_area.RDS') %>% dplyr::select(c(UNIT_CODE, everything(.))) 

         cols <- c("#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663",
                   "#7C221D", "#E7331A", "#F16A16", "#FFC82F", "#0D98BA", "#0074A5", "#2E5895",
                   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#1D0255", "#2B0071", "#022394", "#43B54C", "#FAEF44", "#F39E3A", "#EE2939",
                   "#CC99C9", "#9EC1CF", "#9EE09E", "#FDFD97", "#FEB144", "#FF6663")
                   
zion_area <- nps_attains_areas %>% filter(UNIT_CODE == "ZION") %>% group_by(assessmentunitidentifier) %>% summarize() %>% nngeo::st_remove_holes()
shen_lines <- nps_attains_lines %>% filter(UNIT_CODE == "SHEN") %>% group_by(assessmentunitidentifier) %>% summarize()
cong_points <- nps_attains_points %>% filter(UNIT_CODE == "CONG") %>% summarize(assessmentunitidentifier)

lake_area <- nps_attains_areas %>% filter(UNIT_CODE %in% c("GRCA", "LAKE")) %>% group_by(assessmentunitidentifier) %>% summarize() %>% nngeo::st_remove_holes()
lake_lines <- nps_attains_lines %>% filter(UNIT_CODE %in% c("GRCA", "LAKE")) %>% group_by(assessmentunitidentifier) %>% summarize()
lake_points <- nps_attains_points %>% filter(UNIT_CODE %in% c("GRCA", "LAKE"))

line <- ggplot()+
  geom_sf(data = filter(parks, UNIT_CODE == "SHEN"), color = "grey", size = 3, alpha = 0.3) +
  geom_sf(data = shen_lines, aes(color = assessmentunitidentifier), size = 300) +
  scale_color_manual(values=cols) +
  guides(fill = FALSE, color = FALSE, size = FALSE) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.5) +
  theme_void()

area <- ggplot()+
  geom_sf(data = filter(parks, UNIT_CODE == "ZION"), color = "grey", size = 3, alpha = 0.3) +
  geom_sf(data = zion_area, aes(fill = assessmentunitidentifier), color = "white") +
  #scale_color_viridis_d() +
  scale_fill_manual(values=cols) +
  guides(fill = FALSE, color = FALSE, size = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  theme_void()

point <- ggplot()+
  geom_sf(data = filter(parks, UNIT_CODE == "CONG"), color = "grey", size = 3, alpha = 0.3) +
  geom_sf(data = cong_points, aes(color=assessmentunitidentifier), size = 3) +
  scale_fill_manual(values=cols) +
  guides(fill = FALSE, color = FALSE, size = FALSE) +
  annotation_scale(location = "tr", width_hint = 0.5) +
  theme_void()

all <- ggplot()+
  geom_sf(data = filter(parks, UNIT_CODE %in% c("GRCA", "LAKE")), color = "grey", size = 3, alpha = 0.3) +
  geom_sf(data = lake_area, aes(fill = assessmentunitidentifier), color = "white") +
  geom_sf(data = lake_lines, aes(color = assessmentunitidentifier), size = 300) +
  geom_sf(data = lake_points, aes(color = assessmentunitidentifier), size = 3) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  guides(fill = FALSE, color = FALSE, size = FALSE) +
  annotation_scale(location = "br", width_hint = 0.5) +
  theme_void()

ggarrange(point,line, area, all)


# WHAT IS THE BREAKDOWN OF ASSESSMENT TYPES ACROSS PARK UNITS

# remove unnecessary columns
col_remove <- c("GRIDCODE", "submissionid", "GLOBALID", "orig_fid", "SOURCEFC", "Shape", "Shape_Area", "Shape_Length", "AreaSqKM")

nps_attains_points <- readRDS('data/00_nps_attains_points.RDS') %>% dplyr::select(-col_remove) %>% dplyr::select(c(UNIT_CODE, FEATUREID, everything(.))) %>%
  sf::st_join(., npca_regions, left = TRUE) %>%
  dplyr::as_tibble(.) %>%
  mutate(assessment_type = "POINT")
nps_attains_lines <- readRDS('data/00_nps_attains_lines.RDS') %>% dplyr::select(-col_remove) %>% dplyr::select(c(UNIT_CODE, FEATUREID, everything(.))) %>%
  sf::st_join(., npca_regions, left = TRUE) %>%
  dplyr::as_tibble(.) %>%
  mutate(assessment_type = "LINE")
nps_attains_areas <- readRDS('data/00_nps_attains_areas.RDS') %>% dplyr::select(-col_remove) %>% dplyr::select(c(UNIT_CODE, FEATUREID, everything(.))) %>%
  sf::st_join(., npca_regions, left = TRUE) %>%
  dplyr::as_tibble(.) %>%
  mutate(assessment_type = "AREA")
nps_noncontus_attains_points <- readRDS('data/00_nps_noncontus_attains_points.RDS') %>% dplyr::select(c(UNIT_CODE, everything(.))) %>%
  sf::st_join(., npca_regions, left = TRUE) %>%
  dplyr::as_tibble(.) %>%
  mutate(assessment_type = "POINT")
nps_noncontus_attains_lines <- readRDS('data/00_nps_noncontus_attains_lines.RDS') %>% dplyr::select(c(UNIT_CODE, everything(.))) %>%
  sf::st_join(., npca_regions, left = TRUE) %>%
  dplyr::as_tibble(.) %>%
  mutate(assessment_type = "LINE")
nps_noncontus_attains_areas <- readRDS('data/00_nps_noncontus_attains_areas.RDS') %>% dplyr::select(c(UNIT_CODE, everything(.))) %>%
  sf::st_join(., npca_regions, left = TRUE) %>%
  dplyr::as_tibble(.) %>%
  mutate(assessment_type = "AREA")

# this represents all assessment units that are physically within the park boundaries
nps_all_attains <- bind_rows(nps_attains_areas, nps_attains_lines, nps_attains_points,
                             nps_noncontus_attains_areas, nps_noncontus_attains_lines, nps_noncontus_attains_points) %>%
  mutate(office = ifelse(is.na(office) & state =="TX", "Texas",
                         ifelse(is.na(office) & state =="MN", "Midwest", office)))

nps_aus <- nps_all_attains %>%
  distinct(assessmentunitidentifier, .keep_all = TRUE) %>%
  mutate(for_pie = case_when(ircategory %in% c("5A","5","4C","4B", "4A") ~ "Impaired",
                             ircategory == "3" ~ "Unknown",
                             ircategory %in% c("1","2") ~ "Good"))  %>%
  group_by(for_pie) %>%
  summarize(count=n()) %>%
  ungroup() %>%
  mutate(for_pie = factor(x = for_pie, levels = for_pie)) %>% 
  mutate(prop = count/sum(count)) %>%  
  mutate(ypos = cumsum(prop) - 0.5*prop) %>%
  mutate(legend = paste0(for_pie, " (", percent(prop), ")"))

ggplot(data=nps_aus, aes(x="", y=count, fill=legend)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("#059FA4", "#DC851E", "#A1A522")) + 
  guides(fill=guide_legend(title = "AUs in All Parks")) +
  theme_void() + # remove background, grid, numeric label
  theme(text = element_text(size = 20)) 

# By NPCA Region:
offices <- unique(nps_all_attains$office)
plots <- vector("list", length = length(unique(nps_all_attains$office)))

for(i in 1:length(unique(nps_all_attains$office))){
  
  nps_aus_office <- nps_all_attains %>%
    filter(office == offices[i]) %>%
    distinct(assessmentunitidentifier, .keep_all = TRUE) %>%
    mutate(for_pie = case_when(ircategory %in% c("5A","5","4C","4B", "4A") ~ "Impaired",
                               ircategory == "3" ~ "Unknown",
                               ircategory %in% c("1","2") ~ "Good"))  %>%
    group_by(for_pie) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(for_pie = factor(x = for_pie, levels = for_pie)) %>% 
    mutate(prop = count/sum(count)) %>%  
    mutate(ypos = cumsum(prop) - 0.5*prop) %>%
    mutate(legend = paste0(for_pie, " (", percent(prop), ")"))
  
  plots[[i]] <- ggplot(data=nps_aus_office, aes(x="", y=count, fill=legend)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("#059FA4", "#DC851E", "#A1A522")) +
    guides(fill=guide_legend(title = paste0("AUs in ", offices[i], " Region"))) +
    theme_void() + # remove background, grid, numeric label
    theme(text = element_text(size = 20)) 
  
}

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]]
plots[[11]]

# What are the top five reasons for an AU to be impaired:
top_imps <- nps_all_attains %>%
  dplyr::left_join(
    # remove duplicate/unnecessary columns:
    dplyr::select(attains_assmnt_parms, -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
    by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(parametercategorycode %in% c("4A", "4C", "5", "4B", "5A")) %>%
  dplyr::distinct(assessmentunitidentifier, parametergroupname) %>%
  dplyr::group_by(parametergroupname) %>%
  dplyr::summarize(total_aus = n())

top_imps_office <- nps_all_attains %>%
  dplyr::left_join(
    # remove duplicate/unnecessary columns:
    dplyr::select(attains_assmnt_parms, -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
    by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(parametercategorycode %in% c("4A", "4C", "5", "4B", "5A")) %>%
  dplyr::distinct(assessmentunitidentifier, parametergroupname, .keep_all = TRUE) %>%
  dplyr::group_by(office, parametergroupname) %>%
  dplyr::summarize(total_aus = n()) %>%
  slice_max(order_by = total_aus, n=5)

top_imps_office[[1]]
top_imps_office[[2]]
top_imps_office[[3]]
top_imps_office[[4]]
top_imps_office[[5]]
top_imps_office[[6]]
top_imps_office[[7]]
top_imps_office[[8]]
top_imps_office[[9]]
top_imps_office[[10]]
top_imps_office[[11]]

# HOW MANY PARKS HAVE AN IMPAIRMENT?
parks_impaired <- nps_all_attains %>%
  dplyr::left_join(
    # remove duplicate/unnecessary columns:
    dplyr::select(attains_assmnt_parms, -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
    by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(parametercategorycode %in% c("4A", "4C", "5", "4B", "5A")) 
100*(n_distinct(parks_impaired$UNIT_CODE)/n_distinct(parks$UNIT_CODE))

# HOW MANY PARKS HAVE CATEGORY 5 IMPAIRMENTS?
parks_303d <- nps_all_attains %>%
  dplyr::left_join(
    # remove duplicate/unnecessary columns:
    dplyr::select(attains_assmnt_parms, -c(assmnt_joinkey, organizationid, submissionid, orgtype, tas303d, reportingcycle, waterbodyreportlink, GLOBALID)),
    by = "assessmentunitidentifier", multiple = "all") %>%
  dplyr::filter(parametercategorycode %in% c("5", "5A")) 
100*(n_distinct(parks_303d$UNIT_CODE)/n_distinct(parks$UNIT_CODE))



# Which parks don't have any AU data?
no_data <- parks %>%
  filter(!UNIT_CODE %in% nps_all_attains$UNIT_CODE)

