## Basic NBA Tutorials (Python & R)

Welcome to my repo for some commonly requested coding questions relating to NBA statistics/analysis. We're going to hopefully cover a wide range of topics of varying complexity, but nothing too crazy. Of note, there won't be much on scraping here as that's not an area of expertise for me, and I don't want to provide code I can't explain in depth. 

1) The code presented here is written for maximum readability at potentially the expense of maxmimum performance or best practice. This is not prod level code. 
2) These repos are designed to get you off the ground - not to be anywhere close to a finished product. Take the code apart and get wierd with it. 
3) Each folder should have an .md and a .ipynb to show R and python respectively. Whenever possible I tried to literally duplicate the code to help people learn both languages. I generally start in R and then translate to python. All the data is provided.
4) Reach out to me on Twitter with questions/comments/concerns.


## People to Follow/Learn From
[Andrew Patton](https://twitter.com/anpatt7) <- Me  
[Nathan Walker](https://twitter.com/bbstats)    
[Ryan Davis](https://github.com/rd11490/NBA_Tutorials)  
[Krishna Knarsu](https://twitter.com/knarsu3)   
[Kosta Medvedovsky](https://twitter.com/kmedved)  
[Jacob Goldstein](https://twitter.com/JacobEGoldstein) 

### How to Make an SPM
The first item is how to make a statistical plus minus (SPM) model. SPMs are models based on box score data (usually) that attempt to predict a player's RAPM, or [Regularized Adjusted Plus Minus](https://github.com/rd11490/NBA_Tutorials/tree/master/rapm). While RAPM is a somewhat unbiased estimate of a player's +/- value, an SPM can help understand *why* a player might have that +/- value based on their constituent box score components. SPMs you might have heard of include but are not limited to [Daniel Myers'](https://twitter.com/DSMok1) [BPM](https://www.basketball-reference.com/about/bpm.html) and [Jacob Goldstein's](https://twitter.com/JacobEGoldstein) [PIPM](https://www.bball-index.com/current-pipm/) (Although PIPM contains some non-SPM style lineup information). SPMs are traditionally based in a standard linear regression framework, and do not necessarily need the possession or stint level information needed for calculating RAPM. 

### How to Make a Shot Quality Model
Our shot quality model takes a look at a regression based approach and a classification based approach. We start simply and do a tiny bit of feature crafting. The end result is a basic but functional shot quality (expected points per shot and/or fg%) model based on 2018 and 2019 data. A good example of a well documented shot quality model is [KOBE, published at Nylon Calculus](https://fansided.com/2015/09/28/introducing-kobe-a-measure-of-shot-quality/) by [Krishna Narsu](https://twitter.com/knarsu3). The model we show here is in may ways a much much simpler version of that. There are obviously a million ways to make a shot quality model and what is shown here is just a way to get started.

### How to Make (Not Crappy) Shot Charts
WIP

### How to Make Shiny Apps
WIP

