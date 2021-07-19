overlap_function <- function(df) {
    df %>% #data frame of interest
        arrange(group_var_1, date_time_variable_start) %>% #arrange temporally
        group_by(group_var_1) %>% #group by variables of interest, could be several
        mutate(index = c(0, cumsum(as.numeric(lead(date_time_variable_start)) > #the index shows which time groups the datetimes were collapsed into 
                                      cummax(as.numeric(date_time_variable_start_end)))[-n()])) %>%
        group_by(group_var_1, index) %>%
        summarize(Start = min(StartTime), 
                  End = max(EndTime),
                  TotalHours = round(as.double(difftime(End, Start, units = "hours")), 2),
                  .groups = "drop")
}