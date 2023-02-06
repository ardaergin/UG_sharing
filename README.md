# UG_sharing

## Function Description: *Recode Numbers*
This is a function that recodes the text responses to questions that essentially asks for number entries. For instance, the question can be "*How many hours a week do you* [...]?". Here are the changes it makes:

**1) Most basically, it extracts number from the text entries.**    
- "*I join for about 16 hours*" is converted into "*16*".     
         
           
**2) By the same token, if someone indicates 'more than' or 'less than' or something similar, it disregards it.** This choice was made simply because it would be very unreliable to guess the actual number the person had in mind.     
- "*more than 20*" is converted into only "*20*".   

**3) When people indicate in between two numbers, it takes the average of the two numbers indicated.**    
- "*16-18 hours*" or "around 16 to 18 times a year" are converted into "*17*".    




## Function Description: *Recode Text*
This is a function for recoding text entries. It essentially utilizes what is known as the **Jaro-Winkler similarity** between two strings ([link](https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance)).     

You have to supply this function with a vector of possible responses. The function then compares each response with its similarity with all of the elements in the supplied vector. The vector that has the highest similarity is chosen. Although, note that there is also a **threshold** (that you can specify): if there are no similarities, let's say, above 0.85 (Jaro-Winkler similarity can be 1 highest), it deems that no similarities exist, and gets assigned NA. A high threshold value is good for avoiding false positives.     

For instance, if we had a question that asked the respondents which municipality they belong to, and they made a typo and responded with '*Amsterdm*', it is able to correctly categorize this response as '*Amsterdam*'. This happens since Amsterdam has the highest similarity to Amsterdm in the municipalities vector we supplied the function with. E.g.,
- *Amsterdm-Amsterdam* pair has a JW similarity of 0.978
- *Amsterdm-Amstelveen* pair has a JW similarity of 0.825
- *Amsterdm-Rotterdam* pair has a JW similarity of 0.727

Therefore, in the end, 
- to correct typos
- to categorize similar responses under one (e.g., "solar panels" and "solar energy" can get categorized into "solar", if you have "solar" in the vector you supply the function with, although also consider using *regular expressions* [e.g., base::grep] if you do not suspect any typos).
