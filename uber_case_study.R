#####UBER CASE STUDY##
##By :- Kaushal Kashyap
##Application Id:- APFE18801041


# creating work directory
      dir.create(file.path("C:/Users/gs-1551/Downloads/iiit banglore/uber case study", "UberCase"), showWarnings = FALSE)

#setting up the work directory
      setwd(file.path("C:/Users/gs-1551/Downloads/iiit banglore/uber case study", "UberCase"))  

# Downloading Uber Request Data
      fileUrl <- "https://cdn.upgrad.com/UpGrad/temp/76b3b6a4-d87d-4e82-b1c3-3f6e10b9c076/Uber%20Request%20Data.csv"
      download.file(fileUrl, destfile = "Uber Request Data.csv")

# loading relevant packages
      library(readr)
      library(tidyr)
      library(lubridate)
      library(ggplot2)
      library(grid)
      library(gridExtra)

# Loading Data 
      uber_Case <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)

# looking at data
      View(uber_Case)
      str(uber_Case)
      head(uber_Case)

# data Cleaning 
      uber_Case$Pickup.point <- as.factor(uber_Case$Pickup.point)
      uber_Case$Status <- as.factor(uber_Case$Status)
      uber_Case$Driver.id <- as.factor(uber_Case$Driver.id)

# Data Cleaning -- Date formats
      uber_Case$Request.timestamp <- parse_date_time(x = uber_Case$Request.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"), locale = "English")
      uber_Case$Drop.timestamp <- parse_date_time(x = uber_Case$Drop.timestamp, orders = c("%d %m %Y %H%M","%d %m %Y %H:%M:%S"), locale = "English")

# Sorting Data based on Driver.id and Request Time
      uber_Case <- uber_Case[order(uber_Case$Driver.id, uber_Case$Request.timestamp),]

# calculating the Triptime 
      uber_Case$triptime<-as.numeric(uber_Case$Drop.timestamp - uber_Case$Request.timestamp)
      Average_trip_time <- mean(!is.na(uber_Case$triptime))*60     
### 

# Culling out hour of day from Request time and Drop time
      uber_PreFinal <- separate(data = uber_Case, col = "Request.timestamp", into = c("req.date","req.time"), sep = " ")
      uber_Final <- separate(data = uber_PreFinal, col = "Drop.timestamp", into = c("drop.date","drop.time"), sep = " ")
      uber_Final$Req.hrs <- as.factor(substring(uber_Final$req.time,0,2))
      uber_Final$drop.hrs <- as.factor(substring(uber_Final$drop.time,0,2))


# Demand and supply at Airport
      airport_demand <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "Airport"), aes(Req.hrs))+
        geom_bar(fill = "blue") + labs(title ="Demand at Airport(Requests)")+
        theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
      airport_supply <- ggplot(subset(subset(uber_Final, uber_Final$Pickup.point == "Airport"),!is.na(drop.hrs)), aes(drop.hrs))+
        geom_bar(fill = "red") + labs(title ="Supply at Airport(hours)")+
        theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)

#Demand and supply at City
      city_demand <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "City"), aes(Req.hrs))+
        geom_bar(fill = "blue") + labs(title ="Demand at City(Requests)")+
        theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)
      city_supply <- ggplot(subset(subset(uber_Final, uber_Final$Pickup.point == "City"),!is.na(drop.hrs)), aes(drop.hrs))+
        geom_bar(fill = "red") + labs(title ="Supply at City(Drops)")+
        theme(plot.title = element_text(hjust = 0.5))+ylim(0,500)

# Seeing overall demand and supply trend
      grid.arrange(airport_demand, airport_supply ,city_demand ,city_supply, nrow = 2, ncol = 2)


# Demand & Supply together at Airport
      Demand_Supply_Airport <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "Airport")) +
        geom_bar(aes(Req.hrs), fill = c("blue"))+ geom_bar(aes(drop.hrs),fill = c("red"),position = "dodge")+
        ylim(0,500) + labs(title = "From Airport (Blue = Request for Cab, Red = Availability (after Drops))") + 
        theme(plot.title = element_text(hjust = 0.5)) + xlab("Hrs of the day")

# Demand & Supply together at City
      Demand_Supply_City <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "City")) +
        geom_bar(aes(Req.hrs), fill = c("blue"))+ geom_bar(aes(drop.hrs),fill = c("red"),position = "dodge")+
        ylim(0,500) + labs(title = "From City (Blue = Request for Cab, Red = Availability (after Drops))") + 
        theme(plot.title = element_text(hjust = 0.5)) + xlab("Hrs of the day")

# Demand and Supply together for Comparision
      grid.arrange(Demand_Supply_Airport, Demand_Supply_City, ncol = 2)

# Plotting the Data to see the trends in request time vs Status 
# when pick up is at Airport
      request_airport <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "Airport"), aes(Req.hrs, fill = Status))
      request_airport <- request_airport + geom_bar() + ylim(0,500) + labs(title = "From Airport") + theme(plot.title = element_text(hjust = 0.5))

# when pick up at the city
      request_city <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "City"), aes(Req.hrs, fill = Status))
      request_city <- request_city + geom_bar() + ylim(0,500) + labs(title = "From City") + theme(plot.title = element_text(hjust = 0.5))

# combining Plots  
  grid_arrange <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
      plots <- list(...)
      position <- match.arg(position)
      g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
      legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
      lheight <- sum(legend$height)
      lwidth <- sum(legend$width)
      gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
      gl <- c(gl, nrow = nrow, ncol = ncol)
      
      combined <- switch(position,
                         "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                                legend,
                                                ncol = 1,
                                                heights = unit.c(unit(1, "npc") - lheight, lheight)),
                         "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                               legend,
                                               ncol = 2,
                                               widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
      grid.newpage()
      grid.draw(combined)
      
    }

# combined plot
      grid_arrange(request_airport, request_city)

# Cancelled and No car available Scenarios at Airport
      cancel_air <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "Airport" & uber_Final$Status == "Cancelled"), aes(Req.hrs))+
        geom_bar(fill = "blue") + labs(title = "Cancel Status at Airport")+ theme(plot.title = element_text(hjust = 0.5))
      
      nocar_air <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "Airport" & uber_Final$Status == "No Cars Available"), aes(Req.hrs))+
        geom_bar(fill = "red") + labs(title = "No Cars Available at Airport")+ theme(plot.title = element_text(hjust = 0.5))


      grid.arrange(cancel_air, nocar_air, ncol = 2)

# Cancelled and No car available Scenarios at city
      cancel_city <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "City" & uber_Final$Status == "Cancelled"), aes(Req.hrs))+
        geom_bar(fill = "blue") + labs(title = "Cancel Status at City")+ theme(plot.title = element_text(hjust = 0.5))
      nocar_city <- ggplot(subset(uber_Final, uber_Final$Pickup.point == "City" & uber_Final$Status == "No Cars Available"), aes(Req.hrs))+
        geom_bar(fill = "red") + labs(title = "No Cars Available at City")+ theme(plot.title = element_text(hjust = 0.5))

      grid.arrange(cancel_city, nocar_city, ncol = 2)


# Exporting clean data frame for operations in tableau
      write.csv(uber_Final, "UberData.csv", na = "")


