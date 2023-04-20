# Fishery map

# Combining information from labels.tmp and Table 1 from the assessment report (fisheries),
# and also from flags in the doitall (grouping of release groups)

fishery_map <- data.frame(
  fishery_name=c("LL ALL 1",
                 "LL ALL 2", "LL US 2",
                 "LL ALL 3", "LL OS 3",
                 "LL OS 7", "LL ALL 7",
                 "LL ALL 8",
                 "LL ALL 4",
                 "LL AU 5", "LL ALL 5",
                 "LL ALL 6",
                 "PS ASS 3", "PS UNS 3",
                 "PS ASS 4", "PS UNS 4",
                 "MISC PH 7", "HL PHID 7",
                 "PS JP 1", "PL JP 1",
                 "PL ALL 3",
                 "PL ALL 8",
                 "MISC ID 7", "PS PHID 7",
                 "PS ASS 8", "PS UNS 8",
                 "LL AU 9",
                 "PL ALL 7",
                 "LL ALL 9",
                 "PS ASS 7", "PS UNA 7", "MISC VN 7",
                 "Survey R1 (Fish 1)",
                 "Survey R2 (Fish 2)",
                 "Survey R3 (Fish 4)",
                 "Survey R4 (Fish 9)",
                 "Survey R5 (Fish 11)",
                 "Survey R6 (Fish 12)",
                 "Survey R7 (Fish 7)",
                 "Survey R8 (Fish 8)",
                 "Survey R9 (Fish 29)"))
fishery_map$fishery <- 1:nrow(fishery_map)
fishery_map$region <- c(1,
                        2,2,
                        3,3,
                        7,7,
                        8,
                        4,
                        5,5,
                        6,
                        3,3,
                        4,4,
                        7,7,
                        1,1,
                        3,
                        8,
                        7,7,
                        8,8,
                        9,
                        7,
                        9,
                        7,7,7,
                        1,2,3,4,5,6,7,8,9)

# Grouping
fishery_map$group <- "Index"
fishery_map$group[c(20, 21, 22, 28)] <- "PL"
fishery_map$group[c(13, 14, 15, 16, 19, 24, 25, 26, 30, 31)] <- "PS"
fishery_map$group[c(13, 15, 25, 30)] <- "PS ASS"
fishery_map$group[c(14, 26, 31)] <- "PS UNASS"
fishery_map$group[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 27, 29)] <- "LL"
fishery_map$group[c(17, 18, 23, 32)] <- "DOM"

# Specify grouping of fisheries for tag return data
# based on file "doitall.condor"
#
# Only fisheries 1:32 have a group as the index fisheries 33-41 do not capture tags.
# To get a fish flag use a negative number for that fishery
# > flagval(par, -(1:41), 32)
# flagtype flag value
#       -1   32     1
#       -2   32     2
#       -3   32     3
#       -4   32     4
#       -5   32     5
#       -6   32     6
#       -7   32     7
#       -8   32     8
#       -9   32     9
#      -10   32    10
#      -11   32    11
#      -12   32    12
#      -13   32    13
#      -14   32    13
#      -15   32    14
#      -16   32    14
#      -17   32    15
#      -18   32    15
#      -19   32    16
#      -20   32    17
#      -21   32    18
#      -22   32    19
#      -23   32    15
#      -24   32    15
#      -25   32    20
#      -26   32    20
#      -27   32    21
#      -28   32    22
#      -29   32    23
#      -30   32    24
#      -31   32    24
#      -32   32    25
#      -33   32    25
#      -34   32    25
#      -35   32    25
#      -36   32    25
#      -37   32    25
#      -38   32    25
#      -39   32    25
#      -40   32    25
#      -41   32    25

# 32 + 9 index fisheries
# Only 32 shown as the index fisheries (33-41) do not capture tags

fishery_map$tag_recapture_group <- fishery_map$fishery
fishery_map$tag_recapture_group[1:32] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                           13, 13,
                                           14, 14,
                                           15, 15,
                                           16, 17, 18, 19,
                                           15, 15,
                                           20, 20,
                                           21, 22, 23,
                                           24, 24,
                                           25)

fishery_map$tag_recapture_name <- fishery_map$fishery_name
which(table(fishery_map$tag_recapture_group) > 1) # 13, 14, 15, 20, 24
fishery_map[fishery_map$tag_recapture_group == 13, "tag_recapture_name"] <- "PS 3"
fishery_map[fishery_map$tag_recapture_group == 14, "tag_recapture_name"] <- "PS 4"
fishery_map[fishery_map$tag_recapture_group == 15, "tag_recapture_name"] <- "MISC PHID 7"
fishery_map[fishery_map$tag_recapture_group == 20, "tag_recapture_name"] <- "PS 8"
fishery_map[fishery_map$tag_recapture_group == 24, "tag_recapture_name"] <- "PS 7"

save(fishery_map, file="../app/data/fishery_map.RData")
