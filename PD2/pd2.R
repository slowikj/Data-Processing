#install.packages("nycflights13")
#install.packages("dplyr")

library("nycflights13")
library("dplyr")


rozwiazanie_bazowe <- list()
rozwiazanie_sqldf <- list()
rozwiazanie_dplyr <- list()

# 1. SELECT DISTINCT engine FROM planes
rozwiazanie_bazowe[["1"]] <- function() { data.frame("engine"=as.vector(unique(planes$engine))) }
rozwiazanie_sqldf[["1"]] <- function() { sqldf::sqldf("SELECT DISTINCT engine FROM planes") }
rozwiazanie_dplyr[["1"]] <- function() { as.data.frame(distinct(select(planes, engine))) }


#2.  SELECT DISTINCT type, manufacture FROM planes
rozwiazanie_bazowe[["2"]] <- function() { data.frame(unique(planes[c("type", "manufacturer")])) }
rozwiazanie_sqldf[["2"]] <- function() { sqldf::sqldf("SELECT DISTINCT type, manufacturer FROM planes") }
rozwiazanie_dplyr[["2"]] <- function() { as.data.frame(distinct(select(planes, type, manufacturer))) }

#3. SELECT COUNT(*), engine FROM planes GROUP BY engine
rozwiazanie_bazowe[["3"]] <- function() {
  t <- table(planes$engine)
  data.frame(
    "count"=as.vector(t),
    "engine"=names(t))
}

rozwiazanie_dplyr[["3"]] <- function() {
  as.data.frame(
    group_by(planes, engine) %>% 
                  summarize(n())
    )
}

rozwiazanie_sqldf[["3"]] <- function() {
  sqldf::sqldf("SELECT COUNT(*), engine FROM planes GROUP BY engine")
}

#4. SELECT COUNT(*), engine, type FROM planes GROUP BY engine, type
rozwiazanie_bazowe[["4"]] <- function () {
  aggregate(seats ~ engine*type, planes, length)
}

II_rozwiazanie_bazowe_4 <- function() { aggregate(planes["year"], planes[c("type", "engine")], length) }

rozwiazanie_dplyr[["4"]] <- function() {
  group_by(planes, engine, type) %>% summarize(n())
}

rozwiazanie_sqldf[["4"]] <- function() {
  sqldf::sqldf("SELECT COUNT(*), engine, type FROM planes GROUP BY engine, type")
}

#5. SELECT MIN(year), AVG(year), MAX(year), engine, manufacturer FROM planes GROUP BY engine, manufacturer

rozwiazanie_bazowe[["5"]] <- function () {
  df  <- suppressWarnings(data.frame(aggregate(planes["year"],
                   planes[c("engine", "manufacturer")],
                  FUN=function(x) {
                        c("Min"=min(x, na.rm=TRUE),
                        "Avg"=mean(x, na.rm=TRUE),
                        "Max"=max(x, na.rm=TRUE))})))
  
  
  
  df <- (within(df, {year[,"Min"] <- ifelse(year[,"Min"] %in% c(-Inf, Inf, NaN), NA, year[,"Min"])}))
  df <- (within(df, {year[,"Avg"] <- ifelse(year[,"Avg"] %in% c(-Inf, Inf, NaN), NA, year[,"Avg"])}))
  res <- (within(df, {year[,"Max"] <- ifelse(year[,"Max"] %in% c(-Inf, Inf, NaN), NA, year[,"Max"])}))
  res
}

rozwiazanie_dplyr[["5"]] <- function() {
  df <- as.data.frame(
    group_by(planes, engine, manufacturer) %>%
      summarize("Min"=min(year, na.rm=TRUE), "Avg"=mean(year, na.rm=TRUE), "Max"=max(year, na.rm=TRUE)))
  
  df <- within(df, {Min <- ifelse(Min %in% c(-Inf, Inf, NaN), NA, Min)})
  df <- within(df, {Avg <- ifelse(Avg %in% c(-Inf, Inf, NaN), NA, Avg)})
  res <- within(df, {Max <- ifelse(Max %in% c(-Inf, Inf, NaN), NA, Max)})
  res
}

rozwiazanie_sqldf[["5"]] <- function() {
  sqldf::sqldf("SELECT MIN(year), AVG(year), MAX(year), engine, manufacturer FROM planes GROUP BY engine, manufacturer")
}

#6. SELECT * FROM planes WHERE speed IS NOT NULL
rozwiazanie_bazowe[["6"]] <- function() {
  as.data.frame(planes[!is.na(planes$speed),])
}

rozwiazanie_dplyr[["6"]] <- function() {
  as.data.frame(filter(planes, !is.na(speed)))
}

rozwiazanie_sqldf[["6"]] <- function() {
  sqldf::sqldf("SELECT * FROM planes WHERE speed is not NULL")
}

#7. SELECT tailnum, seats, year FROM planes WHERE seats BETWEEN 100 AND 200 AND year >= 2010
rozwiazanie_bazowe[["7"]] <- function() {
  r <- planes[planes$seats >= 100 & planes$seats <= 200 & planes$year >= 2010, c("tailnum", "seats", "year")]
  r <- r[rowSums(is.na(r)) == 0,]
  as.data.frame(r)
}

rozwiazanie_dplyr[["7"]] <- function() {
  as.data.frame(
    select(planes, tailnum, seats, year) %>%
      filter(between(seats, 100, 200), year >= 2010)
  )
}

rozwiazanie_sqldf[["7"]] <- function() {
  sqldf::sqldf("SELECT tailnum, seats, year FROM planes WHERE seats BETWEEN 100 AND 200 AND year >= 2010")
}


# 8. SELECT * FROM planes WHERE manufacturer IN ("BOEING", "AIRBUS", "EMBRAER") AND seats>300
rozwiazanie_bazowe[["8"]] <- function() {
  as.data.frame(
    planes[planes$manufacturer %in% c("BOEING", "AIRBUS", "EMBRAER") & planes$seats > 300,])
}

rozwiazanie_dplyr[["8"]] <- function() {
  as.data.frame(
    filter(planes, manufacturer %in% c("BOEING", "AIRBUS", "EMBRAER"), seats > 300)
  )
}

rozwiazanie_sqldf[["8"]] <- function() {
  sqldf::sqldf("SELECT * FROM planes WHERE manufacturer IN (\"BOEING\", \"AIRBUS\", \"EMBRAER\") AND seats>300")
}

# 9. SELECT manufacturer, COUNT(*) FROM planes WHERE seats > 200 GROUP BY manufacturer
rozwiazanie_bazowe[["9"]] <- function() {
  filtered_planes <- planes[planes$seats > 200,]
  rozwiazanie_bazowe_9 <- as.data.frame(table(filtered_planes$manufacturer))
}

II_rozwiazanie_bazowe_9 <- function() {
  filtered_planes <- planes[planes$seats > 200,]
  aggregate(filtered_planes["seats"], filtered_planes["manufacturer"], length)
}

rozwiazanie_dplyr[["9"]] <- function() {
  as.data.frame(
    filter(planes, seats > 200) %>% 
      group_by(manufacturer) %>%
      summarize(n())
  )
}

rozwiazanie_sqldf[["9"]] <- function() {
  sqldf::sqldf("SELECT manufacturer, COUNT(*) FROM planes WHERE seats > 200 GROUP BY manufacturer")
}

# 10. SELECT manufacturer, COUNT(*) FROM planes GROUP BY manufacturer HAVING COUNT(*) > 10
# I rozwiazanie:
rozwiazanie_bazowe[["10"]] <- function() {
  all <- as.data.frame(as.table(unlist(lapply(split(planes$manufacturer, planes$manufacturer), length))))
  tt <- all[all$Freq > 10,]
  rownames(tt) <- 1:nrow(tt)
  tt
}
  
# II rozwiazanie
II_rozwiazanie_bazowe_10 <- function() {
  all <- as.data.frame(table(planes$manufacturer))
  all[all$Freq > 10, ]
}

rozwiazanie_dplyr[["10"]] <- function() {
  as.data.frame(
    planes %>%
      group_by(manufacturer) %>% 
      summarize(cnt=n()) %>%
      filter(cnt > 10)
  )
}

rozwiazanie_sqldf[["10"]] <- function() {
  sqldf::sqldf("SELECT manufacturer, COUNT(*) FROM planes GROUP BY manufacturer HAVING COUNT(*) > 10")
}

# 11. SELECT manufacturer, COUNT(*) FROM planes WHERE seats > 200 GROUP BY manufacturer HAVING COUNT(*) > 10
rozwiazanie_bazowe[["11"]] <- function() {
  filtered_planes <- as.data.frame(planes[planes$seats > 200,])
  r <- as.data.frame(table(filtered_planes$manufacturer))
  r[r$Freq > 10,]
}

rozwiazanie_dplyr[["11"]] <- function() {
  as.data.frame(
    planes %>% 
      filter(seats > 200) %>% 
      group_by(manufacturer) %>% 
      summarize(cnt=n()) %>% 
      filter(cnt > 10)
  )
}

rozwiazanie_sqldf[["11"]] <- function() {
  sqldf::sqldf("SELECT manufacturer, COUNT(*) FROM planes WHERE seats > 200 GROUP BY manufacturer HAVING COUNT(*) > 10")
}

# 12. SELECT manufacturer, COUNT(*) AS howmany FROM planes GROUP BY manufacturer ORDER BY howmany DESC LIMIT 10
rozwiazanie_bazowe[["12"]] <- function() {
  unordered_data <- as.data.frame(table(planes$manufacturer))
  ordered_data <- unordered_data[order(unordered_data$Freq, decreasing=TRUE),]
  rownames(ordered_data) <- 1:nrow(ordered_data)
  head(ordered_data, 10)
}

rozwiazanie_dplyr[["12"]] <- function() {
  as.data.frame(
    planes %>%
      group_by(manufacturer) %>%
      summarize(howmany=n()) %>%  
      arrange(desc(howmany)) %>% 
      top_n(10)
  )
}

rozwiazanie_sqldf[["12"]] <- function() {
  sqldf::sqldf("SELECT manufacturer, COUNT(*) AS howmany FROM planes GROUP BY manufacturer ORDER BY howmany DESC LIMIT 10")
}

#13. SELECT * FROM planes WHERE year >= 2012 ORDER BY year, seats
rozwiazanie_bazowe[["13a"]] <- function() {
  planes_filtered <- planes[!is.na(planes$year) & planes$year >= 2012, ]
  as.data.frame(planes_filtered[order(planes_filtered$year, planes_filtered$seats),])
}

rozwiazanie_dplyr[["13a"]] <- function() {
  as.data.frame(
    planes %>% filter(!is.na(year),year >= 2012) %>% arrange(year, seats)
  )
}

rozwiazanie_sqldf[["13a"]] <- function() {
  sqldf::sqldf("SELECT * FROM planes WHERE year >= 2012 ORDER BY year, seats")
}

# oraz SELECT * FROM planes WHERE year >= 2012 ORDER BY seats, year
rozwiazanie_bazowe[["13b"]] <- function() {
  planes_filtered <- planes[planes$year >= 2012 & !is.na(planes$year), ]
  as.data.frame(planes_filtered[order(planes_filtered$seats, planes_filtered$year),])
}

rozwiazanie_dplyr[["13b"]] <- function() {
  as.data.frame(
    planes %>% filter(!is.na(year), year >= 2012) %>% arrange(seats, year)
  )
}

rozwiazanie_sqldf[["13b"]] <- function() {
  sqldf::sqldf("SELECT * FROM planes WHERE year >= 2012 ORDER BY seats, year")
}

#14. SELECT planes.*, airlines.* FROM 
# (SELECT DISTINCT carrier, tailnum FROM flights) AS cartail 
# JOIN planes ON cartail.tailnum=planes.tailnum 
# JOIN airlines ON cartail.carrier=airlines.carrier

rozwiazanie_bazowe[["14"]] <- function() {
  certail <- unique(flights[,c("carrier", "tailnum")])
  df <- merge(merge(certail, planes, by=c("tailnum")), airlines, by=c("carrier"))
  as.data.frame(df[, c(colnames(planes), colnames(airlines))])
}

rozwiazanie_dplyr[["14"]] <- function() {
  cartail <- flights %>% select(carrier, tailnum) %>% distinct()
  as.data.frame(
    inner_join(cartail, planes, "tailnum") %>% 
      inner_join(airlines, "carrier")  %>% 
      select(one_of(c(colnames(planes), colnames(airlines))))
  )
}

rozwiazanie_sqldf[["14"]] <- function() {
  sqldf::sqldf("SELECT planes.*, airlines.* FROM 
  (SELECT DISTINCT carrier, tailnum FROM flights) AS cartail 
                                     JOIN planes ON cartail.tailnum=planes.tailnum 
                                     JOIN airlines ON cartail.carrier=airlines.carrier")
}

#15. SELECT flights2.*, weather2.atemp, weather2.ahumid, weather2.apressure FROM 
#(SELECT * FROM flights WHERE origin='EWR') AS flights2 
#LEFT JOIN 
#(SELECT year, month, day, AVG(temp) AS atemp, 
#  AVG(humid) AS ahumid, AVG(pressure) AS apressure 
#  FROM weather WHERE origin='EWR' GROUP BY year, month, day) AS weather2 
#ON flights2.year=weather2.year 
#AND flights2.month=weather2.month 
#AND flights2.day=weather2.day

rozwiazanie_bazowe[["15"]] <- function() {
  flights2 <- flights[flights$origin == "EWR", ]
  
  w <- weather[weather$origin == "EWR", ]
  weather2 <- aggregate(
    w[c("temp", "humid", "pressure")],
    w[c("year", "month", "day")],
    function(x) {
      res <- mean(x, na.rm=TRUE)
      res
    }
  )
  weather2 <- weather2[, c("year", "month", "day", "temp", "humid", "pressure")]
  colnames(weather2) <- c("year", "month", "day", "atemp", "ahumid", "apressure")
  
  res <- merge(flights2, weather2, all.x=TRUE, by=c("year", "month", "day"))
  
  res[, c(colnames(flights2), "atemp", "ahumid", "apressure")]
}

rozwiazanie_dplyr[["15"]] <- function() {
  flights2 <- as.data.frame(
    flights %>% filter(origin=="EWR")
  )
  
  weather2 <- as.data.frame(
    weather %>% 
      filter(origin=="EWR") %>%
      group_by(year, month, day) %>% 
      summarize(atemp=mean(temp, na.rm=TRUE),
                ahumid=mean(humid, na.rm=TRUE),
                apressure=mean(pressure, na.rm=TRUE)) %>%
      select(year, month, day, atemp, ahumid, apressure)
  )
  
  left_join(flights2, weather2, c("year", "month", "day")) %>%
    select(one_of(c(colnames(flights2), "atemp", "ahumid", "apressure")))
}

rozwiazanie_sqldf[["15"]] <- function() {
  sqldf::sqldf("SELECT flights2.*, weather2.atemp, weather2.ahumid, weather2.apressure FROM 
  (SELECT * FROM flights WHERE origin='EWR') AS flights2 
                                     LEFT JOIN 
                                     (SELECT year, month, day, AVG(temp) AS atemp, 
                                     AVG(humid) AS ahumid, AVG(pressure) AS apressure 
                                     FROM weather WHERE origin='EWR' GROUP BY year, month, day) AS weather2 
                                     ON flights2.year=weather2.year 
                                     AND flights2.month=weather2.month 
                                     AND flights2.day=weather2.day")
}
