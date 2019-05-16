
library(dplyr)
library(ggplot2)
df <- read.csv("http://www.football-data.co.uk/new/POL.csv")[,c("Date" ,"HomeTeam", "AwayTeam" ,"FTHG", "FTAG" ,"FTR", "HTHG", "HTAG", 
                                                                "HTR", "Referee", "HS" , "AS", "HST", "AST", "HF", "AF", "HC" ,"AC" ,"HY", "AY", "HR" , "AR")]

df <- read.csv("http://www.football-data.co.uk/mmz4281/1819/E0.csv")[,c("Date" ,"HomeTeam", "AwayTeam" ,"FTHG", "FTAG" ,"FTR", "HTHG", "HTAG", 
                                                                        "HTR", "Referee", "HS" , "AS", "HST", "AST", "HF", "AF", "HC" ,"AC" ,"HY", "AY", "HR" , "AR")]
df_old <-  read.csv("http://www.football-data.co.uk/mmz4281/1718/E0.csv")[,c("Date" ,"HomeTeam", "AwayTeam" ,"FTHG", "FTAG" ,"FTR", "HTHG", "HTAG", 
                                                                                  "HTR", "Referee", "HS" , "AS", "HST", "AST", "HF", "AF", "HC" ,"AC" ,"HY", "AY", "HR" , "AR")]

df <- rbind(df, df_old)

# HS = Home Team Shots
# AS = Away Team Shots
# HST = Home Team Shots on Target
# AST = Away Team Shots on Target
# HHW = Home Team Hit Woodwork
# AHW = Away Team Hit Woodwork
# HC = Home Team Corners
# AC = Away Team Corners
# HF = Home Team Fouls Committed
# AF = Away Team Fouls Committed
# HFKC = Home Team Free Kicks Conceded
# AFKC = Away Team Free Kicks Conceded
# HO = Home Team Offsides
# AO = Away Team Offsides
# HY = Home Team Yellow Cards
# AY = Away Team Yellow Cards
# HR = Home Team Red Cards
# AR = Away Team Red Cards
# HBP = Home Team Bookings Points (10 = yellow, 25 = red)
# ABP = Away Team Bookings Points (10 = yellow, 25 = red)

##FAULE ----
#Najwięcej fauli

Home <- df[,c("HomeTeam", "HF")] %>% setNames(c("Team", "Fouls"))
Away <- df[,c("AwayTeam", "AF")] %>% setNames(c("Team", "Fouls"))
  
fouls <- rbind(Home, Away)
aggregate(fouls$Fouls, list(fouls$Team), sum) %>% ggplot() + geom_bar(aes(x = reorder( Group.1, x), y = x), stat= "identity", fill="#00CC66") + coord_flip() + theme_minimal() + 
  geom_label(aes(label=x, y = x - 5, x = Group.1),
             position = position_stack(vjust = 0.95),
             size = 3.5,
             colour = "#00CC66") + scale_x_discrete(name = "Drużyna") +
  scale_y_continuous(name = "Suma fauli w sezonie") + ggtitle("Faule w sezonie. Premier League 2018/19.")

#ŻÓŁTE KARTKI ----
#Najwięcej kartek zółte

Home <- df[,c("HomeTeam", "HY")] %>% setNames(c("Team", "Yellows"))
Away <- df[,c("AwayTeam", "AY")] %>% setNames(c("Team", "Yellows"))

yellows <- rbind(Home, Away)
aggregate(yellows$Yellows, list(yellows$Team), sum) %>% ggplot() + geom_bar(aes(x = reorder( Group.1, x), y = x), stat= "identity", fill="#FFCC00") + coord_flip() + theme_minimal() + 
  geom_label(aes(label=x, y = x - 5, x = Group.1),
             position = position_stack(vjust = 0.95),
             size = 3.5,
             colour = "#FFCC00") + scale_x_discrete(name = "Drużyna") +
  scale_y_continuous(name = "Suma żółtych kartek w sezonie") + ggtitle("Żółte kartki w sezonie. Premier League 2018/19.") -> yellows

ggsave(yellows, filename = "D:/data_football/eng/podsumowanie sezonu/zoltekartki.png", width = 8, height = 4 )

#CZERWONE KARTKI ----
#Najwięcej kartek czerwone

Home <- df[,c("HomeTeam", "HR")] %>% setNames(c("Team", "Reds"))
Away <- df[,c("AwayTeam", "AR")] %>% setNames(c("Team", "Reds"))

reds <- rbind(Home, Away)
aggregate(reds$Reds, list(reds$Team), sum) %>% ggplot() + geom_bar(aes(x = reorder( Group.1, x), y = x), stat= "identity", fill="#FF0000") + coord_flip() + theme_minimal() + 
  geom_label(aes(label=x, y = x, x = Group.1),
             position = position_stack(vjust = 0.95),
             size = 3.5,
             colour = "#FF0000")+ scale_x_discrete(name = "Drużyna") +
  scale_y_continuous(name = "Suma czerwonych kartek w sezonie") + ggtitle("Czerwone kartki w sezonie. Premier League 2018/19.") -> reds

ggsave(reds, filename = "D:/data_football/eng/podsumowanie sezonu/czerwonekartki.png", width = 8, height = 4 )


#GOLE NA MECZ ----
#Srednia liczba goli na mecz
Home <- df[,c("HomeTeam", "FTHG")] %>% setNames(c("Team", "Gole"))
Away <- df[,c("AwayTeam", "FTAG")] %>% setNames(c("Team", "Gole"))

gole <- rbind(Home, Away)


gole %>% group_by(Team) %>% summarise(mean(Gole)) %>% as.data.frame() %>% setNames(c("Team", "Gole")) %>% mutate(kierunek = "Strzelone") %>% arrange(desc(Gole)) -> gole_df

#Srednia liczba goli straconych 
Home <- df[,c("HomeTeam", "FTAG")] %>% setNames(c("Team", "Gole"))
Away <- df[,c("AwayTeam", "FTHG")] %>% setNames(c("Team", "Gole"))

gole <- rbind(Home, Away)


gole %>% group_by(Team) %>% summarise(mean(Gole)) %>% as.data.frame() %>% setNames(c("Team", "Gole")) %>% mutate(kierunek = "Stracone") %>% arrange(desc(Gole)) -> gole_df_stracone


gole <- rbind(gole_df, gole_df_stracone)


gole$Team <- factor(gole$Team , levels = gole$Team[gole$kierunek=="Stracone"] [order(gole$Gole[gole$kierunek=="Stracone"])])

gole %>% ggplot() + geom_bar(aes(x = Team,  y = Gole, fill = kierunek), stat= "identity", position = "dodge")  + theme_minimal() + 
  scale_x_discrete(name = "Drużyna") +
  scale_y_continuous(name = "Średnia liczba goli") + ggtitle("Średnia liczba goli w sezonie. Premier League 2018/19.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust =1 )) + scale_fill_discrete(guide = guide_legend(title = "Gole")) +
 geom_hline(yintercept = mean(gole$Gole[gole$kierunek == "Strzelone"])) +
  geom_text(aes(x= "Fulham", label="średnia", y=mean(gole$Gole[gole$kierunek == "Strzelone"])), vjust = 1.2, size = 2 ) -> gole_pic

ggsave(gole_pic, filename = "D:/football/golesredniei.png", width = 8, height = 4 )
ggsave(gole_pic, filename = "D:/data_football/eng/podsumowanie sezonu/golesredniei.png", width = 8, height = 4 )

#STRZŁY NA MECZ ----

#Srednia liczba strzałów na mecz

#Srednia liczba strzałów na mecz
Home <- df[,c("HomeTeam", "HS")] %>% setNames(c("Team", "Strzaly"))
Away <- df[,c("AwayTeam", "AS")] %>% setNames(c("Team", "Strzaly"))

strzaly <- rbind(Home, Away)


strzaly %>% group_by(Team) %>%
  summarise(mean(Strzaly)) %>% 
  as.data.frame() %>% 
  setNames(c("Team", "Strzaly")) %>% 
  mutate(kierunek = "Wszystkie") %>%
  arrange(desc(Strzaly)) -> strzaly_df


#Srednia liczba goli straconych 
Home <- df[,c("HomeTeam", "HST")] %>% setNames(c("Team", "Strzaly"))
Away <- df[,c("AwayTeam", "AST")] %>% setNames(c("Team", "Strzaly"))

strzaly <- rbind(Home, Away)


strzaly %>% group_by(Team) %>%
  summarise(mean(Strzaly)) %>% 
  as.data.frame() %>% 
  setNames(c("Team", "Strzaly")) %>% 
  mutate(kierunek = "Celne") %>%
  arrange(desc(Strzaly)) -> strzaly_df_celne

strzaly <- rbind(strzaly_df, strzaly_df_celne)

strzaly$Team <- factor(strzaly$Team , levels = strzaly$Team[strzaly$kierunek=="Wszystkie"] [order(strzaly$Strzaly[strzaly$kierunek=="Wszystkie"])])

g <- ggplot_build(gole_pic)
unique(g$data[[1]]["fill"])

strzaly %>% ggplot() + geom_bar(aes(x = Team,  y = Strzaly, fill = kierunek), stat= "identity", position = "dodge")  + theme_minimal() + 
  scale_x_discrete(name = "Drużyna") +
  scale_y_continuous(name = "Średnia liczba strzałów") + ggtitle("Średnia liczba strzałów na mecz w sezonie. Premier League 2018/19.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust =1 )) + 
  scale_fill_discrete(guide = guide_legend(title = "Strzały")) +
  geom_hline(yintercept = mean(strzaly$Strzaly[strzaly$kierunek == "Wszystkie"]), colour = "#00BFC4") +
  geom_text(aes(x= "Chelsea", label="średnia wszystkich", y=mean(strzaly$Strzaly[strzaly$kierunek == "Wszystkie"])), 
            vjust = 1.2, size = 2 ) +
  geom_hline(yintercept = mean(strzaly$Strzaly[strzaly$kierunek == "Celne"]), colour = "#F8766D") +
  geom_text(aes(x= "Burnley", label="średnia celnych", y=mean(strzaly$Strzaly[strzaly$kierunek == "Celne"])), 
            vjust = 1.2, size = 2 ) -> strzaly_mean

ggsave(strzaly_mean, filename = "D:/football/strzalysrednia.png", width = 8, height = 4 )
ggsave(strzaly_mean, filename = "D:/data_football/eng/podsumowanie sezonu/strzalysrednia.png", width = 8, height = 4 )

#POŁOWA A KONIEC MECZU ----
### __WSZYSTKIE ----
#Polowa - caly mecz

temp <- df[,c( "Date" ,"HomeTeam", "AwayTeam" ,"FTHG", "FTAG" ,"FTR", "HTHG", "HTAG", "HTR")]

data.frame( game = c("D->H", "D->A","D->D","H->D","H->H","H->A","A->A","A->D","A->H" ),
            count = c(nrow(temp[temp$HTR == "D" & temp$FTR == "H", ]),
                    nrow(temp[temp$HTR == "D" & temp$FTR == "A", ]),
                    nrow(temp[temp$HTR == "D" & temp$FTR == "D", ]),
                    nrow(temp[temp$HTR == "H" & temp$FTR == "D", ]),
                    nrow(temp[temp$HTR == "H" & temp$FTR == "H", ]),
                    nrow(temp[temp$HTR == "H" & temp$FTR == "A", ]),
                    nrow(temp[temp$HTR == "A" & temp$FTR == "A", ]),
                    nrow(temp[temp$HTR == "A" & temp$FTR == "D", ]),
                    nrow(temp[temp$HTR == "A" & temp$FTR == "H", ]))
            ) %>% mutate(prc = paste0(round(count / nrow(temp), 3) * 100, " %")) %>% arrange(desc(count)) -> polowa_caly


data.frame( game = c("D->H", "D->A","D->D","H->D","H->H","H->A","A->A","A->D","A->H" ),
            count = c(nrow(temp[temp$HTR == "D" & temp$FTR == "H", ]),
                      nrow(temp[temp$HTR == "D" & temp$FTR == "A", ]),
                      nrow(temp[temp$HTR == "D" & temp$FTR == "D", ]),
                      nrow(temp[temp$HTR == "H" & temp$FTR == "D", ]),
                      nrow(temp[temp$HTR == "H" & temp$FTR == "H", ]),
                      nrow(temp[temp$HTR == "H" & temp$FTR == "A", ]),
                      nrow(temp[temp$HTR == "A" & temp$FTR == "A", ]),
                      nrow(temp[temp$HTR == "A" & temp$FTR == "D", ]),
                      nrow(temp[temp$HTR == "A" & temp$FTR == "H", ]))
) %>% mutate(prc = round(count / nrow(temp), 3) ) %>% arrange(desc(count)) -> polowa_caly

polowa_caly %>% ggplot() + geom_bar(aes(x = reorder( game,prc), y = prc), stat= "identity", fill="#6df88f")  + theme_minimal() + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.30, by=0.05), name = "Procent spotkań")+
  geom_label(aes(label=count, y = prc, x = game),
             position = position_stack(vjust = 0.95),
             size = 3.5,
             colour = "#6df88f") + scale_x_discrete(name = "Połowa -> Koniec meczu") +
 ggtitle("Połowa a wynik końcowy. Premier League 2018/19.") -> polowa_koniec

ggsave(polowa_koniec, filename = "D:/data_football/eng/podsumowanie sezonu/polowaakoniec.png", width = 8, height = 4 )

### __PRZEWAGA 2 GOLI ----
#Polowa - caly mecz - z przewaga 2 goli do połowy

temp <- df[((df$HTHG - df$HTAG) > 2) | ((df$HTAG - df$HTHG) > 2)
  ,c( "Date" ,"HomeTeam", "AwayTeam" ,"FTHG", "FTAG" ,"FTR", "HTHG", "HTAG", "HTR")]

data.frame( game = c("D->H", "D->A","D->D","H->D","H->H","H->A","A->A","A->D","A->H" ),
            count = c(nrow(temp[temp$HTR == "D" & temp$FTR == "H", ]),
                      nrow(temp[temp$HTR == "D" & temp$FTR == "A", ]),
                      nrow(temp[temp$HTR == "D" & temp$FTR == "D", ]),
                      nrow(temp[temp$HTR == "H" & temp$FTR == "D", ]),
                      nrow(temp[temp$HTR == "H" & temp$FTR == "H", ]),
                      nrow(temp[temp$HTR == "H" & temp$FTR == "A", ]),
                      nrow(temp[temp$HTR == "A" & temp$FTR == "A", ]),
                      nrow(temp[temp$HTR == "A" & temp$FTR == "D", ]),
                      nrow(temp[temp$HTR == "A" & temp$FTR == "H", ]))
) %>% mutate(prc = round(count / nrow(temp), 3) ) %>% arrange(desc(count))  -> polowa_caly_2_gole

polowa_caly_2_gole %>% ggplot() + geom_bar(aes(x = reorder( game,prc), y = prc), stat= "identity", fill="#6df88f")  + theme_minimal() + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.80, by=0.10), name = "Procent spotkań")+
  geom_label(aes(label=count, y = prc, x = game),
             position = position_stack(vjust = 0.95),
             size = 3.5,
             colour = "#6df88f") + scale_x_discrete(name = "Połowa -> Koniec meczu") +
  ggtitle("Połowa a wynik końcowy (2 gole róznicy). Premier League 2018/19.") -> polowa_koniec_2_gole

ggsave(polowa_koniec_2_gole, filename = "D:/data_football/eng/podsumowanie sezonu/polowaakoniec2gole.png", width = 8, height = 4 )

### __SKUTCZNOŚĆ ----
#Skutecznośc strzałów

shots <- df[,c("Date" ,"HomeTeam", "AwayTeam" ,"FTHG", "FTAG" ,"FTR", "HTHG", "HTAG", "HTR", "HS", "AS", "HST", "AST","HF")]


### __Niespodziewany wynik ----
#Najbardziej niespodziewany wynik pod względem strzałów

shots <- df[,c("Date" ,"HomeTeam", "AwayTeam" ,"FTHG", "FTAG" ,"FTR", "HTHG", "HTAG", "HTR", "HS", "AS", "HST", "AST","HF")]

shots %>% mutate(SD = HS - AS, SDT = HST  - AST ) -> shots

shots[shots$FTR == "A",] %>% arrange(desc(SD))
shots[shots$FTR == "A",] %>% arrange(SD)
shots[shots$FTR == "A",] %>% arrange(desc(SDT))

#levels(shots$FTR) <- c("H", "D", "A")
g1 <- subset(shots, shots$FTR == "A" & AS  < 4  & SD > 15)
g2 <- subset(shots, shots$FTR == "H" & SD < -17 )
g3 <- subset(shots, shots$FTR == "D" & SD > 20 )

shots_temp <- shots[-(which(shots$HomeTeam == "Burnley" & shots$AwayTeam == "Man City")),]
  
ggplot(shots_temp, aes(x=HS, y = AS, col = FTR)) + geom_point(size=8, alpha = 0.7) + scale_x_continuous(name = "Strzały gospodarzy", breaks = c(0:(max(shots$HS)))) +
  scale_y_continuous(name = "Strzały gości", breaks = c(0:(max(shots$AS)))) + theme_minimal() +
  geom_text(data=g1, label=paste0(g1$HomeTeam, "-", g1$AwayTeam), vjust=1.2, col = "black") + 
  geom_text(data=g2, label=paste0(g2$HomeTeam, "-", g2$AwayTeam), vjust=1.2, col = "black") +
  geom_text(data=g3, label=paste0(g3$HomeTeam, "-", g3$AwayTeam), vjust=1.2, col = "black") + 
   scale_colour_discrete(name  ="Wynik meczu",
                          breaks=c("A", "D", "H"),
                          labels=c("Wygrana gości", "Remis", "Wygrana gospodarzy")) + 
  ggtitle("Strzały gospodrzy i gości w meczu. Premier League 18/19") -> niespodziewany_wynik

ggsave(niespodziewany_wynik, filename = "D:/football/niespodziewany_wynik.png", width = 12, height = 10 )
ggsave(niespodziewany_wynik, filename = "D:/data_football/eng/podsumowanie sezonu/niespodziewany_wynik.png", width = 8, height = 4 )



ggplot(shots, aes(x=HST, y = AST, col = FTR)) + geom_point(size=2) + scale_x_continuous(name = "Strzały celne gospodarzy", breaks = c(0:(max(shots$HST)))) +
   scale_y_continuous(name = "Strzały celne gości", breaks = c(0:(max(shots$AST)))) + theme_minimal()










###Strzały na gol ----
#Suma liczba strzałów na mecz
Home <- df[,c("HomeTeam", "HS")] %>% setNames(c("Team", "Strzaly"))
Away <- df[,c("AwayTeam", "AS")] %>% setNames(c("Team", "Strzaly"))

strzaly <- rbind(Home, Away)


strzaly %>% group_by(Team) %>%
  summarise(sum(Strzaly)) %>% 
  as.data.frame() %>% 
  setNames(c("Team", "Strzaly")) %>% 
  mutate(kierunek = "Wszystkie") %>%
  arrange(desc(Strzaly)) -> strzaly_df


#Srednia liczba goli straconych 
Home <- df[,c("HomeTeam", "HST")] %>% setNames(c("Team", "Strzaly"))
Away <- df[,c("AwayTeam", "AST")] %>% setNames(c("Team", "Strzaly"))

strzaly <- rbind(Home, Away)


strzaly %>% group_by(Team) %>%
  summarise(sum(Strzaly)) %>% 
  as.data.frame() %>% 
  setNames(c("Team", "Strzaly")) %>% 
  mutate(kierunek = "Celne") %>%
  arrange(desc(Strzaly)) -> strzaly_df_celne


#Srednia liczba goli na mecz
Home <- df[,c("HomeTeam", "FTHG")] %>% setNames(c("Team", "Gole"))
Away <- df[,c("AwayTeam", "FTAG")] %>% setNames(c("Team", "Gole"))

gole <- rbind(Home, Away)


gole %>% 
  group_by(Team) %>%
  summarise(sum(Gole)) %>% 
  as.data.frame() %>% 
  setNames(c("Team", "Gole")) %>%
  mutate(kierunek = "Strzelone") %>%
  arrange(desc(Gole)) -> gole_df







#Srednia liczba oddanych strzałów na gol
a <- merge(strzaly_df, gole_df, by = "Team")
a %>% mutate(ratio = Strzaly / Gole ) -> a
#Srednia liczba oddanych celnych strzałów na gol

b <- merge(strzaly_df_celne, gole_df, by = "Team")
b %>% mutate(ratio = Strzaly / Gole ) -> b

final <- rbind(a[,c("Team","kierunek.x",   "ratio")],
               b[,c("Team","kierunek.x",   "ratio")])



final$Team <- factor(final$Team , levels = final$Team[final$kierunek.x=="Wszystkie"] [order(final$ratio[final$kierunek.x=="Wszystkie"])])

final %>% ggplot() + geom_bar(aes(x = Team,  y = ratio, fill = kierunek.x), stat= "identity", position = "dodge")  + theme_minimal() + 
  scale_x_discrete(name = "Drużyna") +
  scale_y_continuous(name = "Średnia liczba strzałów na gol",labels = seq(0,20,1),
                     breaks = seq(0,20,1)) + ggtitle("Średnia liczba strzałów na gol w sezonie. Premier League 2018/19.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust =1 )) + 
  scale_fill_discrete(guide = guide_legend(title = "Strzały")) +
  geom_hline(yintercept = mean(final$ratio[final$kierunek == "Wszystkie"]), colour = "#00BFC4") +
  geom_text(aes(x= "Fulham", label="średnia wszystkich", y=mean(final$ratio[final$kierunek == "Wszystkie"])), 
            vjust = 1.2, size = 3 ) +
  geom_hline(yintercept = mean(final$ratio[final$kierunek.x == "Celne"]), colour = "#F8766D") +
  geom_text(aes(x= "Liverpool", label="średnia celnych", y=mean(final$ratio[final$kierunek == "Celne"])), 
            vjust = 1.2, size = 3 ) -> strzaly_na_gol

ggsave(strzaly_na_gol, filename = "D:/football/strzalynagol.png", width = 10, height = 8 )
ggsave(strzaly_na_gol, filename = "D:/data_football/eng/podsumowanie sezonu/strzalynagol.png", width = 8, height = 4 )

