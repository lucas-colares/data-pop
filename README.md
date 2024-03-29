# Functional diversity in human song
Hey there 👋, I'm Lucas and this is the repository for the data and code supporting the results of the paper **"Functional diversity in human song"**, which you can find in the end of this message.
## The project
The main goal of this project was to evidence that the concepts and applications of **functional diversity**, as known from ecology, can be applied to any subject that can be defined by their **traits**, in this case, **music**.
To learn more about functional diversity concepts and metrics, check this reference:
> Mammola, Stefano, et al. "Concepts and applications in functional diversity." Functional Ecology 35.9 (2021): 1869-1885.

To do this we retrived information for the sound characteristics and popularity of **12,944 songs** released throughout the career of _the top 100 most influential artists of the 2010's decade_.
We were successul at calculating the functional diversity for the 100 artists. 
We went beyond this explanatory approauch and also investigated whether functional diversity has any effect on the popularity of the artists in the Spotify platform, finding a positive trend.
## The data
In this repository, I provide the main data used to conduct our analysis. 

The functional trait database consists on the characteristics of key, mode, time signature, duration (in milliseconds), acousticness, danceability, energy, instrumentalness, liveness, valence, speechiness, loudness, beats per minute, and musical genres for 12,944 songs by the 100 artists mentioned before.
This trait database can be found in two .csv files (*Traits.csv* and *Genres.csv*) at the *datasets* folder.

The popularity database has the number of times each of the 12,944 was played at Spotify, YouTube, last.fm, Genius and Deezer. This can be found in the *Streams.csv* file, inside the *datasets* folder.
## The code
You may find the code to calculate functional diversity for the artists in the *01. calculate functional diversity.R* script, in the *scripts* folder. 

In the same folder you will find the code to conduct our data analysis, as described in the paper cited in the end of this message. This can be found in the *02. statistical analysis.R* file.

Before running any of these two scripts, you will need to run the full *00. setup.R* script (available in the *scripts* folder) to setup the packages you will need to run the other two scripts. 

All the code is written in R and the final plots and tables generated by those are available in the *results* folder. 

Future versions of this repository will also include the code to retrieve the raw data regarding the traits and popularity of the songs from the API of each platform and through web scraping, so stay tuned!
## The paper
Check the original paper for further description of analysis, goals and results:
> Colares, Lucas, et al. "Functional diversity in human song." PLoS One (2024)
