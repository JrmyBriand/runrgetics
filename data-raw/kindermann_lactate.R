## code to prepare `kindermann_lactate`
# based on available data in Kindermann (1977)
# data were obtained by digitalizing the figures of the original article
# Kindermann, W. (1977). Lactate acidosis with different forms of sports activities. Can. J. Appl. Sports Sci. https://cir.nii.ac.jp/crid/1574231873976904704

library(dplyr)


duration <- c(
9.075630252100842,
19.159663865546218,
32.26890756302521,
45.378151260504204,
62.521008403361336,
107.89915966386553,
218.4615384615386,
516.9230769230772,
815.1724137931034,
1776.0000000000005 )


lactate <- c(
13.152173913043477,
19.34782608695652,
19.673913043478258,
21.956521739130434,
21.84782608695652,
19.45652173913043,
19.565217391304344,
12.934782608695652,
13.369565217391305,
9.021739130434785 )


accumulated_lactate <- lactate - 1 # assuming reting lactate value of 1 mmol/L

kindermann_lactate <- tibble::tibble(duration= duration,
                      lactate = lactate,
                      accumulated_lactate = accumulated_lactate )




usethis::use_data(kindermann_lactate, overwrite = TRUE)
