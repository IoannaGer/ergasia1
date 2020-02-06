#Εργασία 1
########################################################3
#Κάτω από κάθε ερώτηση να τοποθετήσετε το κώδικα-απάντηση της αντίστοιχης ερώτησης
#Μπορείτε για κάθε απάντηση να χρησιμοποιήσετε οποιοδήποτε μοτίβο κώδικα έχετε διδαχθεί
#An den emfanizontai sosta ta ellinika epilegetai apo to menu tools->global options->code->saving->default code encoding->utf-8
#epeita epilegetai apply kleinete to arxeio kai to ksanaanoigete

#Να υπολογίσετε και να εμφανίσετε τις απαντήσεις για κάθε ένα από τα παρακάτω ερωτήματα


#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset
library(readr)
DelayedFlights <- read_csv("C:\Users\new\Desktop\ergasia1\DelayedFlights.csv")
View(DelayedFlights)

if (require('tidyverse')==FALSE){install.packages("tidyverse")}
library('tidyverse')
if (require('dply')==FALSE){install.packages("dplyr")}
library('dplyr')

str(DelayedFlights)
dim(DelayedFlights)

emp <- c()
for (i in 1:30){
  emp <- c(emp,length(which(is.na(DelayedFlights[,i]))))
}
names(emp) <- 1:30
emp

#Ερώτηση 2: να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων

delay <- matrix(0, nrow = 12, ncol = 31)
  for (i in 1:12){
    for(j in 1:31){
      delay[i,j] <- length(which((DelayedFlights$Month==i) & (DelayedFlights$DayofMonth==j))) 
  }
}
max_delay <- max(delay)
max_delay
which(delay == max_delay)

#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008

mean_summer_delays <- c()
for (i in 6:8) {
  mean_summer_delays <- c(mean_summer_delays, (sum(delay[i,])/length(which(delay[i,] != 0))))
}
names(mean_summer_delays) <- c("June", "July", "August")
mean_summer_delays

#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β

b <- DelayedFlights %>%
  group_by(UniqueCarrier) %>%
  filter(CancellationCode == "B") %>%
  summarize(max_del = length(CancellationCode)) %>%
  arrange(-max_del)
b[1,]

#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων

library(data.table)
D <- data.table(DelayedFlights)
counts <- D[,.(Count = .N), by = FlightNum]
counts <- as.data.frame(counts)
flightnum <- counts %>%
  arrange(-Count)
head(flightnum, n = 10)

#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις

max_dist1 <- DelayedFlights %>%
  filter(Distance == max(Distance)) %>%
  group_by(Dest) %>%
  select(Dest, Distance) %>%
  summarise(most_delays = length(Dest)) %>%
  arrange(-most_delays) 
head(max_dist1, n = 1)

#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)

minutes_delay <- DelayedFlights %>%
  filter(Cancelled == 0) %>%
  group_by(Dest) %>%
  select(Dest, ActualElapsedTime) %>%
  arrange(-ActualElapsedTime)
head(minutes_delay, n = 10)
  
#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών

arrival_delay <- DelayedFlights %>%
  select(UniqueCarrier, LateAircraftDelay) %>%
  arrange(-LateAircraftDelay)
head(arrival_delay, n=1)

#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα

thirteen <- c()
for (i in 1:12){
  thirteen <- c(thirteen, length(which(DelayedFlights$Month ==i & DelayedFlights$DayofMonth == 13 & DelayedFlights$CancellationCode == "A")))
}
names(thirteen) <- 1:12
thirteen

#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008

mean_april_delay <- DelayedFlights %>%
  filter(Month == 4 & DayofMonth >= 10 & DayofMonth <= 23) %>%
  summarise(mean_delay_april = mean(ArrDelay, na.rm=TRUE))
mean_april_delay

#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00

month_delay <- DelayedFlights %>%
  filter(DepTime >= 0600 & DepTime <= 1400) %>%
  select(Month, SecurityDelay) %>%
  arrange(-SecurityDelay)
head(month_delay, n = 1)
  
#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της

estimated_arrival_nov <- DelayedFlights %>%
  filter(DayofMonth >= 1 & DayofMonth <= 10 & Month == 11) %>%
  select(FlightNum, CRSElapsedTime) %>%
  arrange(-CRSElapsedTime)
head(estimated_arrival_nov, n = 1)

#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς

august_carrier_del <- DelayedFlights %>%
  filter(Month == 8 & DayofMonth > 10 & DayofMonth < 21 & CarrierDelay > 30) %>%
  select(Origin)
A <- data.table(august_carrier_del)
counts <- A[,.(Count = .N), by = Origin] 
counts <- as.data.frame(counts)
august <- counts %>%
  arrange(-Count)
head(august, n = 1)


#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε

divflights <- DelayedFlights %>%
  filter(Diverted == 1 & Cancelled == 0) %>%
  select(FlightNum, AirTime) %>%
  arrange(-AirTime)
head(divflights, n = 20)
  
#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης

max_deviation <- DelayedFlights %>%
  select(Month, ArrDelay) %>%
  group_by(Month) %>%
  summarize(deviations = sd(ArrDelay, na.rm=TRUE)) %>%
  arrange(-deviations)
head(max_deviation, n = 1)

