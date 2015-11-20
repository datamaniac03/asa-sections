
###--------------------------------------------------
### ASA Section revenues and membership
###--------------------------------------------------

### Revenue data:
### http://www.asanet.org/documents/ASA/pdfs/ASA_2014_Audit.pdf,
### Schedule 3

### Membership data:
### http://www.asanet.org/sections/CountsLastFiveYears.cfm

###----------------------------------------
### Setup
###--------------------------------------------------

library(ggplot2)
library(scales)
library(MASS)
library(stringr)

my.colors <- function (palette = "cb") {
    cb.palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    rcb.palette <- rev(cb.palette)
    bly.palette <- c("#E69F00", "#0072B2", "#999999", "#56B4E9",
                     "#009E73", "#F0E442", "#D55E00", "#CC79A7")
    if (palette == "cb")
        return(cb.palette)
    else if (palette == "rcb")
        return(rcb.palette)
    else if (palette == "bly")
        return(bly.palette)
    else stop("Choose cb, rcb, or bly ony.")
}

theme_set(theme_minimal())

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)

###--------------------------------------------------


###--------------------------------------------------
### Data
###--------------------------------------------------

data <- read.csv("data/asa-section-membership.csv", header=TRUE)
data$Sname <- str_replace(data$Sname, "Comm/Urban", "Comm\\\nUrban")
data$Sname <- str_replace(data$Sname, "Mental Health", "Mental\\\nHealth")
data$Section <- str_replace(data$Section, "\\(.*\\)", "")

data$Sname <- str_trim(data$Sname)


###--------------------------------------------------
### Figures
###--------------------------------------------------

pdf(file="figures/membership-v-revenues.pdf", height=5, width=7)
p <- ggplot(data, aes(x=X2014, y=Revenues, label=Sname))

p + geom_smooth(method = "rlm", se = FALSE,
                color = "gray80") +
    geom_point(aes(color = Journal)) +
    geom_text(data=subset(data, Revenues > 7000),
              size = 2,
              aes(x=X2014+10,
                  hjust = 0,
                  lineheight = 0.7)) +
    scale_y_continuous(labels = dollar) +
    scale_color_manual(values = my.colors("bly")) +
    labs(x="Membership",
         y="Revenues",
         color = "Section has own Journal") +
    theme(legend.position = "bottom") +
    ggtitle("ASA Sections, Membership vs Revenues")
dev.off()



pdf(file="figures/membership-v-balance.pdf", height=5, width=7)
p <- ggplot(data, aes(x=X2014, y=Ending, label=Sname))
p + geom_smooth(method="rlm", se=FALSE, color = "gray80") +
    geom_point(aes(color = Journal)) +
    geom_text(data=subset(data, Ending > 6000),
              size = 2,
              aes(x=X2014+10,
                  hjust = 0,
                  lineheight = 0.7)) +
    scale_y_continuous(labels = dollar) +
    scale_x_continuous(labels = comma) +
    scale_color_manual(values = my.colors("bly")) +
    labs(x="Membership",
         y="End of Year Balance",
         color = "Section has own Journal") +
        theme(legend.position = "bottom") +
    ggtitle("ASA Sections 2014")
dev.off()


pdf(file="figures/revenues-v-expenses.pdf", height=8, width=7, pointsize = 14)
p <- ggplot(data, aes(x=Revenues, y=Expenses, label = Sname))
p + geom_smooth(method="lm", se=FALSE, color = "gray80") +
    geom_point(aes(color = Journal,
                   size = X2014),
               pch = 21) +
    geom_text(data=subset(data, Expenses/Revenues > 1.3 | Expenses/Revenues < 0.7 | Revenues > 10000),
              size = 2.2,
              aes(x = Revenues + 35,
                  hjust = 0,
                  lineheight = 0.7)) +
    scale_color_manual(values = my.colors("bly")) +
    scale_x_continuous(trans = log2_trans(), breaks = c(500, 1000, 5000, 20000) ,
                       labels = dollar) +
    scale_y_continuous(trans = log2_trans(), breaks = c(1000, 5000, 20000),
                       labels = dollar) +
    labs(x="Logged Dollar Revenues",
         y="Logged Dollar Expenses",
         color = "Section has own Journal",
         size = "Membership") +
    theme(legend.position = "bottom") +
    ggtitle("ASA Sections, Revenues vs Expenses (2014)")
dev.off()


pdf(file="figures/membership-v-balance-no-j.pdf", height=8.5, width=10)
p <- ggplot(subset(data, Journal == "No"), aes(x=X2014, y=Ending, label=Sname))
p + geom_smooth(method="lm", se=FALSE) +
    geom_point() +
    geom_text(data=subset(data, Journal == "No" & Ending > 7000),
              size = 2,
              aes(hjust = -0.15)) +
    scale_y_continuous(labels = dollar) +
    scale_x_continuous(labels = comma) +
    labs(x="Membership",
         y="Ending Balance") +
    ggtitle("ASA Sections without Own Journal,\nMembership vs EOY Balance")
dev.off()


pdf(file="figures/eoy-balance.pdf", height=12, width = 10)
p <- ggplot(data, aes(x=Ending, y=reorder(Section, Ending, ordered = TRUE)))

p + geom_vline(color = "gray70") +
    geom_point() +
    scale_x_continuous(labels = dollar) +
    labs(x = "End of Year Balance",
         y = "") + ggtitle("End of Year Balance")
dev.off()
