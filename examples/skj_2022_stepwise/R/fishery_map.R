# Fishery map

fishery_map <- data.frame(fishery_name = c("PL JP 1", "PS ALL 1", "LL ALL 1", "PL ALL 2", "PS ALL 2", "LL ALL 2", "PL JP 3", "PS ALL 3", "LL ALL 3", "Dom PH 5", "Dom ID 5", "ID_PH 5", "PL ALL 5", "PS ASSOC_no(PHPH, IDID, VN) 5", "PS UNASSOC_no(PHPH, IDID, VN) 5", "DOM VN 5", "LL ALL 5", "PL ALL 6", "PS ASSOC 6", "PS UNASSOC 6", "LL ALL 6", "PL ALL 4", "LL ALL 4", "PL JP 7", "PS ASSOC 7", "PS UNASSOC 7", "LL ALL 7", "PL All 8", "PS ASSOC 8", "PS UNASSOC 8", "LL ALL 8", "Survey R1 (Fish 1)", "Survey R2 (Fish 4)", "Survey R3 (Fish 7)", "Survey R4 (Fish 22)", "Survey R5 (Fish 12)", "Survey R6 (Fish 19)", "Survey R7 (Fish 24)", "Survey R8 (Fish 28)"))
fishery_map$fishery <- 1:nrow(fishery_map)
fishery_map$region <- c(1,1,1,2,2,2,3,3,3,rep(5,8),6,6,6,6,4,4,7,7,7,7,8,8,8,8,1,2,3,4,5,6,7,8)

# Grouping
fishery_map$group <- "Index"
fishery_map$group[c(1,4,7,13, 18, 22, 24, 28)] <- "PL"
fishery_map$group[c(2,5,8,14,15,19,20,25,26,29,30)] <- "PS"
fishery_map$group[c(14, 19, 25, 29)] <- "PS ASS"
fishery_map$group[c(15, 20, 26, 30)] <- "PS UNASS"
fishery_map$group[c(3,6,9,17,21,23,27,31)] <- "LL"
fishery_map$group[c(10,11,12,16)] <- "DOM"

# Need to add tag recapture group and tag_recapture_name
# From where?

# Add tag recapture group
# Only fisheries 1:31 have a group as the index fisheries 32-39 do not capture tags.
# To get a fish flag use a negative number for that fishery
#flagval(par, -(1:39), 32)
#> flagval(par, -(1:39), 32)
#flagtype flag value
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
#      -14   32    14
#      -15   32    14
#      -16   32    15
#      -17   32    16
#      -18   32    17
#      -19   32    18
#      -20   32    18
#      -21   32    19
#      -22   32    20
#      -23   32    21
#      -24   32    22
#      -25   32    23
#      -26   32    23
#      -27   32    24
#      -28   32    25
#      -29   32    26
#      -30   32    26
#      -31   32    27


# 31 + 8 index fisheries
# Only 31 shown as the index fisheries do not capture tags
fishery_map$tag_recapture_group <- fishery_map$fishery
fishery_map$tag_recapture_group[1:31] <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 14, 15, 16, 17, 18, 18, 19, 20, 21, 22, 23, 23, 24, 25, 26, 26, 27)
# Good to add a name too
fishery_map$tag_recapture_name <- fishery_map$fishery_name
# Except where recapture group != recapture name
#bad_names <- 
# These groups need renaming if more than one fishery in them
which(table(fishery_map$tag_recapture_group)>1) # 14, 18, 23, 26
fishery_map[fishery_map$tag_recapture_group==14,"tag_recapture_name"] <-"PS (PHPH, IDID, VN) 5"
fishery_map[fishery_map$tag_recapture_group==18,"tag_recapture_name"] <-"PS 6"
fishery_map[fishery_map$tag_recapture_group==23,"tag_recapture_name"] <-"PS 7"
fishery_map[fishery_map$tag_recapture_group==26,"tag_recapture_name"] <-"PS 8"

save(fishery_map, file = "../app/data/fishery_map.Rdata")


