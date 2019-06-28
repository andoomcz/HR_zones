####Pulling in Potential HR rankings for all teams####

for(i in 1:30){
  foo = match_score(eval(parse(text = hr_bridge[i,1])),ev = 125)
  assign(hr_bridge[i,2],foo)}


