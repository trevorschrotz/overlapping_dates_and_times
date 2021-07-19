overlap_function <- function(df) {
    df %>% #data frame of interest
        arrange(group_var_1, date_time_variable_start) %>% #arrange temporally
        group_by(group_var_1) %>% #group by variables of interest, could be several
        mutate(Index = c(0, cumsum(as.numeric(lead(date_time_variable_start)) > #the index shows which time groups the datetimes are collapsed into based on the group_by
                                      cummax(as.numeric(date_time_variable_start_end)))[-n()])) %>%
        group_by(group_var_1, Index) %>%
        summarize(Start = min(date_time_variable_start), 
                  End = max(date_time_variable_start_end),
                  TotalHours = round(as.double(difftime(End, Start, units = "hours")), 2),
                  .groups = "drop")
}
