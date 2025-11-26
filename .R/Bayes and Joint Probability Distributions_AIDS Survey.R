people=seq(1,300000)
people_aids=sample(people,1500)
people_noaids=setdiff(people,people_aids)
people_aids_pos=sample(people_aids,1500*0.99)
people_aids_neg=setdiff(people_aids,people_aids_pos)
length(people_aids_pos)
length(people_aids_neg)
people_noaids_neg=sample(people_noaids,298500*0.95)
people_noaids_pos=setdiff(people_noaids,people_noaids_neg)
length(people_noaids_neg)
length(people_noaids_pos)
pr_aids_given_pos = (length(people_aids_pos)) /
  ( length(people_aids_pos)+length(people_noaids_pos))
pr_aids_given_pos
pr_aids_given_neg = (length(people_aids_neg)) /
  ( length(people_aids_neg)+length(people_noaids_neg))
pr_aids_given_neg