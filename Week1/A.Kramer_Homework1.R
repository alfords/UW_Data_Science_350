#============================================
#  Aleksey Kramer
#  Data Science 350
#  Week 1 - Homework
#============================================

library(logging)

if (interactive()) {
    # Read the file and set DateFormat as Date
    file = read.csv("JitteredHeadCount.csv")
    file$DateFormat = as.POSIXct(file$DateFormat, format="%m/%d/%Y")
    file$DateFormat <- as.Date(file$DateFormat)

    # (1) Compare Weekend vs. Weekday Head Counts
    weekend <- file[file$DayOfWeek > 5, ]
    workweek <- file[file$DayOfWeek < 6,]

    summary(weekend)
    summary(workweek)

    m_weekend <- mean(weekend$HeadCount)
    m_workweek <- mean(workweek$HeadCount)

    # (1) Headcount is 25% higher (on average) on weekends vs. that during the weekdays
    print("Headcount is 25% higher (on average) on weekends vs. that during the weekdays")
    print(1 - m_workweek / m_weekend)

    # (2) Compare Table Occupancy Weekday vs. WorkWeek
    m_weekend <- mean(weekend$TablesOpen)
    m_workweek <- mean(workweek$TablesOpen)

    # (2) There are 2% (on avarage) less tables opened during the workweek vs. that of a weekend
    print("There are 2% (on avarage) less tables opened during the workweek vs. that of a weekend")
    print(1 - m_workweek / m_weekend)

    # (3) Explore Occupancy Per Hour on Weekends, Weekdays, and overall
    plot(file$HeadCount ~ file$Hour, data = weekend, type = "h", main="Weekend Head Count per Hour")
    plot(file$HeadCount ~ file$Hour, data = workweek, type = "h", main="Weekdays Head Count per Hour")
    plot(file$HeadCount ~ file$Hour, data = file, type = "h", main="Total Head Count per Hour")

    total_peak <- file[file$Hour < 5 | file$Hour > 12,]
    total_low <- file[file$Hour > 4 & file$Hour < 13,]

    total_peak <- mean(total_peak$HeadCount)
    total_low <- mean(total_low$HeadCount)

    # (3) There is a 69% difference in head count between peak times and low times in the casino
    #     The peak time starts around 2:00PM and ends around 5:00AM on throughout the week (workdays and weekends)
    print("There is a 69% difference in head count between peak times ( 1:00 PM to 4:00AM) and low times (5:00AM to 12:00PM)")
    print(1 - total_low / total_peak)

    # (4) Exploring most popular games played during workweek vs. those played during weekend
    # The most populare games are CR, S6, and TP on weekends and during workdays
    plot(weekend$HeadCount~weekend$GameCode, type="h", main="Head Count by Game code on Weekends")
    plot(workweek$HeadCount~workweek$GameCode, type="h", main="Head Count by Game code during Workweek")

    # (4) Isolating weekend headcount per popular game
    weekend.CR <- weekend$HeadCount[weekend$GameCode == 'CR']
    weekend.S6 <- weekend$HeadCount[weekend$GameCode == 'S6']
    weekend.TP <- weekend$HeadCount[weekend$GameCode == 'TP']

    # (4) Isolating workday headcount per popular game
    workweek.CR <- workweek$HeadCount[workweek$GameCode == 'CR']
    workweek.S6 <- workweek$HeadCount[workweek$GameCode == 'S6']
    workweek.TP <- workweek$HeadCount[workweek$GameCode == 'TP']

    # (4) There are 31% more CR players on weekend than there is those during weekdays
    # This is by far the most popular game
    print("There are 31% more CR players on weekend than there is those during weekdays")
    print(1 - mean(workweek.CR) / mean(weekend.CR))

    # (4) There are 25% more S6 players on weekends than those during the weekdays
    print("here are 25% more S6 players on weekends than those during the weekdays")
    print(1 - mean(workweek.S6) / mean(weekend.S6))

    # (4) There are 25% more TP players on weekends than those during the weekdays
    print("There are 25% more TP players on weekends than those during the weekdays")
    print(1 - mean(workweek.TP) / mean(weekend.TP))
}







