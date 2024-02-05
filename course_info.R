######## Course info ########
library(tidyverse)

# Start of semester
start_semester <- "2024-02-26"

# Week of mid-semester break
mid_semester_break <- "2024-04-01"

# Schedule
schedule <- tibble(
    Week = seq(12),
    Topic = c(
        "Foundations of machine learning",
        "Visualising your data and models",
        "Re-sampling and regularisation",
        "Logistic regression and discriminant analysis",
        "Trees and forests",
        "Neural networks and deep learning",
        "Explainable artificial intelligence (XAI)",
        "Support vector machines and nearest neighbours",
        "K-nearest neighbours and hierarchical clustering",
        "Model-based clustering and self-organising maps",
        "Evaluating your clustering model",
        "Project presentations by Masters students"
    ),
    Reference = c(
        "ISLR 2.1, 2.2",
        "Cook and Laa Ch 1, 3, 4, 5, 6, 13",
        "ISLR 5.1, 5.2, 6.2, 6.4",
        "ISLR 4.3, 4.4",
        "ISLR 8.1, 8.2",
        "ISLR 10.1-10.3, 10.7",
        "Molnar 8.1, 8.5, 9.2-9.6",
        "ISLR 9.1-9.3",
        "HOML Ch 20, 21",
        "HOML Ch 22",
        "Cook and Laa Ch 12",
        ""
    ),
    Reference_URL = c(
        "https://www.statlearning.com",
        "https://dicook.github.io/mulgar_book/",
        "https://www.statlearning.com",
        "https://www.statlearning.com",
        "https://www.statlearning.com",
        "https://www.statlearning.com",
        "https://www.statlearning.com",
        "https://www.statlearning.com",
        "https://bradleyboehmke.github.io/HOML/",
        "https://bradleyboehmke.github.io/HOML/",
        "https://dicook.github.io/mulgar_book/",
        ""
    )
)

# Add mid-semester break
calendar <- tibble(
    Date = seq(as.Date(start_semester), by = "1 week", length.out = 13)
) |>
    mutate(
        Week = row_number(),
        Week = if_else(Date < mid_semester_break, Week, Week - 1),
        # Week =
    )

# Add calendar to schedule
schedule <- schedule |>
    left_join(calendar, by = "Week") |>
    mutate(
        Week = if_else(Date == mid_semester_break, NA, Week),
        Topic = if_else(Date == mid_semester_break, "Mid-semester break", Topic),
        Reference = if_else(Date == mid_semester_break, NA, Reference),
        Reference_URL = if_else(Date == mid_semester_break, NA, Reference_URL)
    ) |>
    select(Week, Date, everything())

# Add assignment details
lastmon <- function(x) {
    7 * floor(as.numeric(x - 1 + 4) / 7) + as.Date(1 - 4, origin = "1970-01-01")
}

assignments <- read_csv(here::here("assignments.csv")) |>
    mutate(
        Date = lastmon(Due),
        Moodle = paste0("https://learning.monash.edu/mod/assign/view.php?id=", Moodle),
        File = paste0("assignments/", File)
    )

schedule <- schedule |>
    full_join(assignments, by = "Date") |>
    mutate(Week = if_else(is.na(Week) & Date > "2024-05-20", 13, Week))

show_assignments <- function(week) {
    ass <- schedule |>
        filter(
            Week >= week & (week > Week - 3 | week > 8),
            !is.na(Assignment),
        ) |>
        select(Assignment:File)
    if (NROW(ass) > 0) {
        cat("\n\n## Assignments\n\n")
        for (i in seq(NROW(ass))) {
            cat("* [", ass$Assignment[i], "](../", ass$File[i], ") is due on ",
                format(ass$Due[i], "%A %d %B.\n"),
                sep = ""
            )
        }
    }
}


submit <- function(schedule, assignment) {
    ass <- schedule |>
        filter(Assignment == assignment)
    due <- format(ass$Due, "%e %B %Y") |> stringr::str_trim()
    url <- ass$Moodle
    button <- paste0(
        "<br><br><hr><b>Due: ", due, "</b><br>",
        "<a href=", url, " class = 'badge badge-large badge-blue'>",
        "<font size='+2'>&nbsp;&nbsp;<b>Submit</b>&nbsp;&nbsp;</font><br></a>"
    )
    cat(button)
}
