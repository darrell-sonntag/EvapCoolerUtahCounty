Summer.All.Hourly <- inner_join(Summer2022.23Hourly,visitmaster.list.2,
                              join_by(between(Date,start_date,end_date))) 
                                                                                         
Winter.All.Hourly <- inner_join(Winter2022Hourly,visitmaster.list.2,
                              join_by(between(Date,start_date,end_date)))
