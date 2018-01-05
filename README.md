# Rhythm Division
## Introduction
Rhythm division is part of my research on music rhythm. This type of graph could show the inner logic of rhythm, which lays the fundation of music structural research in the future.
## Method
Music rhythm has hierarchical structure. This structure shows the logic that human understand the rhythm of music.

The method will consider several points:
* The hierarchical structure based on prime numbers.
* Sub-division could break the large prime numbers into groups for structures easier to understand.
* Substitute could treat group of notes into one object.
### Notice
The division factor (denominators) can only contains one logic division line. For example:

In one group of division factors: `{x[0],x[1],...,x[i]}`, we will make sure all `x[i]/x[i-1]` are combination of prime numbers (not fraction). If meet large prime number, try to divided to break it down.
## Deficiency
This algorithm is currently doing mannuly. Automatic algrithm seems too difficult to search now.
## Demos
### middle east rhythm
`{2, 3, 1, 2, 4, 2, 1, 1}`
![Alt Text](https://github.com/RobertBoganKang/rhythm_division/blob/master/demo/middle-east.png)
### substitute
`{3, {2, 1}, {1, 1, 1}, 3}`
![Alt Text](https://github.com/RobertBoganKang/rhythm_division/blob/master/demo/substitute.png)
### division
`{{2, 1}, {2, 2}}`
![Alt Text](https://github.com/RobertBoganKang/rhythm_division/blob/master/demo/division.png)
