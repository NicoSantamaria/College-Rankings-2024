college.list <- read.csv("csvs/CSV_12192023-1014.csv")
graduation.rates <- read.csv("csvs/CSV_12192023-918.csv")
faculty.info <- read.csv("csvs/CSV_12192023-64.csv")
financial.aid <- read.csv("csvs/CSV_12192023-560.csv")
pricing <- read.csv("csvs/CSV_12192023-344.csv")
student.diversity.info <- read.csv("csvs/CSV_12192023-955.csv")

ids = college.list$unitid; l = length(ids)
names = college.list$institution.name

graduation.total = vector(mode="integer", length=l)
graduation.pell = vector(mode="integer", length=l)
faculty.total = vector(mode="integer", length=l)
faculty.tenure = vector(mode="integer", length=l)
faculty.tenuretrack = vector(mode="integer", length=l)
average.grantaid = vector(mode="integer", length=l)
net.price = vector(mode="integer", length=l)
student.diversity <- vector(mode="integer", length=l)

for (x in 1:l) {
  index <- which(graduation.rates$unitid == ids[x])
  graduation.total[x] <- graduation.rates$DRVGR2022.Graduation.rate..total.cohort[index]
  graduation.pell[x] <- graduation.rates$DRVGR2022.Pell.Grant.recipients...Overall.graduation.rate.within.150.percent.of.normal.time[index]
  
  index <- which(faculty.info$unitid == ids[x])
  for (y in index) {
    
    if (faculty.info$EAP2021.Occupation.and.faculty.tenure.status[y] == "Instructional staff, total") {
      faculty.total[x] = faculty.info$EAP2021_RV.Full.time.employees..excluding.medical.schools.[y]
    }
    
    if (faculty.info$EAP2021.Occupation.and.faculty.tenure.status[y] == "Instructional staff, Tenured") {
      faculty.tenure[x] = faculty.info$EAP2021_RV.Full.time.employees..excluding.medical.schools.[y]
    }
    
    if (faculty.info$EAP2021.Occupation.and.faculty.tenure.status[y] == "Instructional staff, On Tenure Track") {
      faculty.tenuretrack[x] = faculty.info$EAP2021_RV.Full.time.employees..excluding.medical.schools.[y]
    }
  
  }
  
  index <- which(financial.aid$unitid == ids[x])
  average.grantaid[x] <- financial.aid$SFA2122.Average.amount.of.institutional.grant.aid.awarded.to.full.time.first.time.undergraduates[index]
  
  index <- which(pricing$unitid == ids[x])
  net.price[x] <- pricing$SFA2122.Average.net.price.students.awarded.grant.or.scholarship.aid..2021.22[index]
  
  index <- which(student.diversity.info$unitid == ids[x])
  student.diversity[x] <- 1 - sd(student.diversity.info[index, 4:length(student.diversity.info)])
  
}

faculty.tenure.percent <- faculty.tenure / faculty.total
faculty.tenuretrack.percent <- (faculty.tenuretrack + faculty.tenure) / faculty.total

raw.ranking <- data.frame(
  ids,
  names,
  graduation.total,
  graduation.pell,
  faculty.tenure.percent,
  faculty.tenuretrack.percent,
  average.grantaid,
  net.price,
  student.diversity
  )

graduation.total.rankings <- vector(mode="integer", length=l)
faculty.tenure.rankings = vector(mode="integer", length=l)
graduation.pell.rankings = vector(mode="integer", length=l)
faculty.tenuretrack.rankings = vector(mode="integer", length=l)
average.grantaid.rankings = vector(mode="integer", length=l)
net.price.rankings = vector(mode="integer", length=l)
student.diversity.rankings <- vector(mode="integer", length=l)

graduation.pell.sorted <- sort(graduation.pell, decreasing = TRUE)
graduation.total.sorted <- sort(graduation.total, decreasing = TRUE)
faculty.tenure.sorted <- sort(faculty.tenure.percent, decreasing = TRUE)
faculty.tenuretrack.sorted <- sort(faculty.tenuretrack.percent, decreasing = TRUE)
average.grantaid.sorted <- sort(average.grantaid, decreasing = TRUE)
net.price.sorted <- sort(net.price, decreasing = TRUE)
student.diversity.sorted <- sort(student.diversity, decreasing = TRUE)

for (x in 1:l) {
  graduation.total.rankings[x] <- which(graduation.total[x] == graduation.total.sorted)[1]
  graduation.pell.rankings[x] <- which(graduation.pell[x] == graduation.pell.sorted)[1]
  faculty.tenure.rankings[x] <- which(faculty.tenure.percent[x] == faculty.tenure.sorted)[1]
  faculty.tenuretrack.rankings[x] <- which(faculty.tenuretrack.percent[x] == faculty.tenuretrack.sorted)[1]
  average.grantaid.rankings[x] <- which(average.grantaid[x] == average.grantaid.sorted)[1]
  net.price.rankings[x] <- which(net.price[x] == net.price.sorted)[1]
  student.diversity.rankings[x] <- which(student.diversity[x] == student.diversity.sorted)[1]
}

#weighting:
graduation.total.rankings <- graduation.total.rankings * 3
graduation.pell.rankings <- graduation.pell.rankings * 3
faculty.tenure.rankings <- faculty.tenure.rankings * 5
faculty.tenuretrack.rankings <- faculty.tenuretrack.rankings * 5
average.grantaid.rankings <- average.grantaid.rankings * 2
net.price.rankings <- net.price.rankings * 2
student.diversity.rankings <- student.diversity.rankings * 3

rankings <- data.frame(
  ids,
  names,
  graduation.total.rankings,
  graduation.pell.rankings,
  faculty.tenure.rankings,
  faculty.tenuretrack.rankings,
  average.grantaid.rankings,
  net.price.rankings,
  student.diversity.rankings
)

rankings[is.na(rankings)] <- 1500

sums <- vector(mode="integer", length=l)
for (x in 1:l) {
  sums[x] <- sum(rankings[x,3:length(rankings)])
}

final.rankings <- data.frame(ids, names, sums)
final.rankings <- final.rankings[order(final.rankings$sums),]


Rank <- c(1:25)
Institution <- final.rankings$names[1:25]
Score <- final.rankings$sums[1:25]

to.export <- data.frame(Rank, Institution, Score)


to.export
