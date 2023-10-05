# Fishery map

# Combining information from labels.tmp and Table 1 from the assessment report
# (fisheries), and also from flags in the doitall (grouping of release groups)

fishery_map <- data.frame(
  fishery_name=c(
    "01.LL.ALL.1",
    "02.LL.ALL.1",
    "03.LL.US.1",
    "04.LL.ALL.4",
    "05.LL.OS.4",
    "06.LL.OS.2",
    "07.LL.ALL.2",
    "08.LL.ALL.3",
    "09.LL.ALL.4",
    "10.LL.AU.5",
    "11.LL.ALL.5",
    "12.LL.ALL.5",
    "13.PS.ASS.4",
    "14.PS.UNA.4",
    "15.PS.ASS.4",
    "16.PS.UNA.4",
    "17.MISC.PH.2",
    "18.HL.PHID.2",
    "19.PS.JP.1",
    "20.PL.JP.1",
    "21.PL.ALL.4",
    "22.PL.ALL.3",
    "23.MISC.ID.2",
    "24.PS.PHID.2",
    "25.PS.ASS.3",
    "26.PS.UNA.3",
    "27.LL.AU.5",
    "28.PL.ALL.2",
    "29.LL.ALL.5",
    "30.PS.ASS.2",
    "31.PS.UNA.2",
    "32.MISC.VN.2",
    "33.Index.1",
    "34.Index.2",
    "35.Index.3",
    "36.Index.4",
    "37.Index.5"))

fishery_map$fishery <- 1:nrow(fishery_map)
fishery_map$region <- as.integer(substring(fishery_map$fishery_name,
                                           nchar(fishery_map$fishery_name)))

# Grouping
fishery_map$group <- "Index"
fishery_map$group[c(20, 21, 22, 28)] <- "PL"
fishery_map$group[c(13, 14, 15, 16, 19, 24, 25, 26, 30, 31)] <- "PS"
fishery_map$group[c(13, 15, 25, 30)] <- "PS ASS"
fishery_map$group[c(14, 16, 26, 31)] <- "PS UNA"
fishery_map$group[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 27, 29)] <- "LL"
fishery_map$group[c(17, 18, 23, 32)] <- "MISC"

fishery_map$tag_recapture_group <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                     13, 13,          # fish 13 14          PS.4
                                     14, 14,          # fish 15 16          PS.4
                                     15, 15,          # fish 17 18 23 24  MISC.2
                                     16, 17, 18, 19,  # fish 19 20 21 22  PL.134
                                     15, 15,          #
                                     20, 20,          # fish 25 26          PS.3
                                     21, 22, 23,      # fish 27 28 29  LL.PL.2.5
                                     24, 24,          # fish 30 31          PS.2
                                     25,              # fish 32
                                     26, 26, 26, 26, 26)

fishery_map$tag_recapture_name <- fishery_map$fishery_name
which(table(fishery_map$tag_recapture_group) > 1) # 13, 14, 15, 20, 24, 26
fishery_map$tag_recapture_name[fishery_map$tag_recapture_group==13] <- "PS.4"
fishery_map$tag_recapture_name[fishery_map$tag_recapture_group==14] <- "PS.4"
fishery_map$tag_recapture_name[fishery_map$tag_recapture_group==15] <- "MISC.2"
fishery_map$tag_recapture_name[fishery_map$tag_recapture_group==20] <- "PS.3"
fishery_map$tag_recapture_name[fishery_map$tag_recapture_group==24] <- "PS.2"
fishery_map$tag_recapture_name[fishery_map$tag_recapture_group==26] <- "Index"

save(fishery_map, file="../app/data/fishery_map.RData")
