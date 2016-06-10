3rd meeting - 7 january 2016
----

Attendance: 7

This meeting was spent reviewing a bit of Simon's code. The code dugged from a database a 400k row dataframe of the form:

     event | station_id | timestamp
     ------+------------+----------

and the task was to measure the time lag between each serie of events at each station.
The code thus needed to loop through each station, and then proceeded to loop through each row of the subsetted dataframe to measure the lag between each row timestamp.

Most of the discussion thus revolved around the question: when is it necessary to loop and how to do so?
The consensus reached was that: a loop is always preferrable to repeating code (repeats leads to higher chance of introducing errors) but at the same time alternative solutions (such as vectorization) are preferable to loop when possible, and if loop needs be, prefer loops in compiled code to loops in interpreted code (i. e. `*apply` and `foreach` over `for`).
In the case at hand, the loop for measuring the lag between events could be vectorized, while the stations need to be actually looped through.

Additionally some advices were given on how to connect to a database from R (specifically on the usage of dbGetQuery vs dbSendQuery), on the relevance of some packages used in Simon's code, etc.

At the end of the meeting, we decided on the Thurday 21st of January at 4pm in room 3330 (Nordbau) for the next meeting.