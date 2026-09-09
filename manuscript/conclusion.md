# Conclusions


The material in this book was informed by my own work interests and experiences. If you enjoyed reading it and you make practical use of at least some of the material I covered, then I consider my effort to be worthwhile.

Racket is a language that many people use for both fun personal projects and for professional development. I have tried, dear reader, to make the case here that Racket is a practical language that integrates well with my work flows on both Linux and macOS.

Writing software is a combination of a business activity, promoting good for society, and an exploration to try out new ideas for self improvement. I believe that there is sometimes a fine line between spending too many resources tracking many new technologies versus getting stuck using old technologies at the expense of lost opportunities. My hope is that reading this book was an efficient and pleasurable use of your time, letting you try some new techniques and technologies that you had not considered before.

If we never get to meet in person or talk on the telephone, then I would like to thank you now for taking the time to read my book.

{format: mermaid}
~~~~~~
graph TD
    Start([Start]) --> Main[main]
    Main --> Input[/Input num_samples/]
    Input --> Try{Try Block}
    
    Try --> Estimate[estimate_pi]
    
    subgraph estimate_pi_function [estimate_pi]
        Estimate --> CheckVal{num_points <= 0?}
        CheckVal -- Yes --> RaiseErr[Raise ValueError]
        CheckVal -- No --> Loop[Loop num_points times]
        Loop --> GenPoint[Generate random x, y]
        GenPoint --> Inside{x² + y² <= 1?}
        Inside -- Yes --> IncCount[count_inside += 1]
        Inside -- No --> NextLoop[Next iteration]
        IncCount --> NextLoop
        NextLoop --> Loop
        Loop -- Done --> Calc[Return 4 * count_inside / num_points]
    end
    
    Calc --> Time[Calculate elapsed time]
    Time --> Print[Print pi estimate and time]
    Print --> End([End])
    
    RaiseErr --> CatchVal[Catch ValueError]
    CatchVal --> PrintErr[Print Error message]
    PrintErr --> End
    
    Try -- Unexpected Error --> CatchEx[Catch Exception]
    CatchEx --> PrintEx[Print unexpected error]
    PrintEx --> End
~~~~~~
