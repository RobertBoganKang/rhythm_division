(* ::Package:: *)

rhythmTree[data_]:=Module[{bhead,bQ,branchcolor,branches,branchtextcolor,containbranch,containbranchQ,denarr,denominators,duration,fathernodeposition,framecolor,graphics,groundcolor,head,i,ihead,index,indexheadoflist,innerindexoflist,iQ,length,linelength,margin,nearestposition,node,numerator,olddenarr,position,Q,recleft,recright,rectlength,simplify,sortlist,subposition,temp,text,textsize,tickbase,tickcolor,tickposition,tickpositiondict,ticks,ticktextsize,tpos,truelength,xbase,xpos,xpos2,ypos,ypos2,yposition,errorcolor,streamQ,errorQ,errcolor,secondarybranches,secondaryfathernodeposition,calculatecoordinate},
(*data manipulation*)
duration=Flatten[data];
tpos[x_]:=Accumulate[Prepend[x,0]]/Total[x];
position=tpos[duration];
simplify[x_]:=If[ListQ[x],Total[Flatten[x]],x];
(*all list?*)
containbranchQ[x_]:=And@@(ListQ/@x);
(*single logic stream line: all divisions are prime number?*)
streamQ[x_]:=!(And@@Table[If[Denominator[x[[i]]/x[[i-1]]]!=1,False,True],{i,2,Length[x]}]);
errcolor[x_]:=If[errorQ,errorcolor,x];

(*system parameters*)
framecolor=Gray;
tickcolor=LightGray;
groundcolor=Darker[Brown,.2];
branchcolor=Darker[Green,.6];
branchtextcolor=Orange;
errorcolor=Darker[Red,.3];
linelength=3;
margin=.1;
textsize=20;
ticktextsize=14;

(*graphics*)
graphics={};
branches={};
secondarybranches={};
text={};
xbase=0;

Do[
(*BFS Body*)
Q={data};
(*calculate index*)
iQ={0};
(*frame base line*)
bQ={0};
(*keep y position of nodes*)
yposition=Table[0,{Length[position]}];
(*keep denominator array for out side loop to check the boundray condition*)
denarr=Table[1000,{Length[position]}];
denarr[[1]]=denarr[[-1]]=1;
While[Length[Q]!=0,
(*take first element*)
head=Q[[1]];
Q=Drop[Q,1];
ihead=iQ[[1]];
iQ=Drop[iQ,1];
bhead=bQ[[1]];
bQ=Drop[bQ,1];
length=Length[head];
truelength=Length[Flatten[head]];
(*index head*)
indexheadoflist=0;
innerindexoflist=0;
(*prepare data*)
subposition=tpos[simplify/@head];
denominators=Denominator[subposition];
numerator=Numerator[subposition];
ticks=Union[denominators];
(*check stream*)
errorQ=streamQ[Union[Denominator[subposition]]];
(*push graphics element: frame*)
tickbase=bhead;
tickposition=bhead;
(*rectangle style*)
AppendTo[graphics,{EdgeForm[Directive[Dashed,errcolor[framecolor]]],White,Thin}];
(*build rectangle*)
rectlength=position[[1+ihead+Length[Flatten[head]]]]-position[[1+ihead]];
recleft=position[[1+ihead]];
recright=rectlength+position[[ihead+1]];
AppendTo[graphics,Rectangle[{recleft+xbase,tickbase},{recright+xbase,rectlength+tickbase}]];
(*rectangle text*)
AppendTo[text,Style[Text[InputForm[rectlength],{(recleft+recright)/2+xbase,tickbase+rectlength}],Background->White,ticktextsize,errcolor[framecolor]]];
(*build ticks*)
tickpositiondict={1->rectlength+tickbase};
containbranch=containbranchQ[head];
(*tick style*)
AppendTo[graphics,{Dashed,errcolor[tickcolor]}];
(*tick text*)
Do[If[!containbranch,
AppendTo[graphics,Line[{{recleft+xbase,rectlength/ticks[[i]]+tickposition},{recright+xbase,rectlength/ticks[[i]]+tickposition}}]];
AppendTo[text,Style[Text[InputForm[1/ticks[[i]]],{recleft+xbase,tickposition+rectlength/ticks[[i]]}],ticktextsize,Background->errcolor[branchcolor],White,Bold]];
AppendTo[tickpositiondict,ticks[[i]]->rectlength/ticks[[i]]+tickposition];,
(*else*)
AppendTo[tickpositiondict,ticks[[i]]->tickbase];
];
tickposition+=rectlength/ticks[[i]];,{i,2,Length[ticks]}];

(*styles for rhythm tree*)
AppendTo[graphics,{Black,Dashing[None]}];
For[i=1,i<=length,i++,
node=head[[i]];
index=indexheadoflist+ihead+innerindexoflist;
(*track y position*)
yposition[[index+1]]=((denominators[[i]])/.tickpositiondict);
(*track denominator array for the loop, the old is for last loop situation*)
olddenarr=denarr;
denarr[[index+1]]=denominators[[i]]/rectlength;
If[ListQ[node],
AppendTo[Q,node];
AppendTo[iQ,ihead+indexheadoflist+innerindexoflist];
AppendTo[bQ,If[!containbranch,(denominators[[i]])/.tickpositiondict,bhead]];
AppendTo[text,Style[Text[InputForm[numerator[[i]]/denominators[[i]]],{position[[index+1]]+xbase,If[!containbranch,(denominators[[i]])/.tickpositiondict,tickbase]}],White,Background->Red,textsize]];
indexheadoflist+=Length[Flatten[node]];,
(*else: reach the branch of tree*)
(*do something for the branch*)
If[numerator[[i]]!=0,AppendTo[text,Style[Text[InputForm[numerator[[i]]/denominators[[i]]],{position[[index+1]]+xbase,((denominators[[i]])/.tickpositiondict)}],textsize,White,Background->errcolor[branchtextcolor]]];];
innerindexoflist++;
];
If[i==1,
(*zero note line*)
AppendTo[branches,{Thickness[.02*rectlength],errcolor[branchcolor],Line[{{position[[index+1]]+xbase,tickbase},{position[[index+1]]+xbase,(denominators[[i]])/.tickpositiondict}}]}];,
(*else*)
sortlist=SortBy[Table[If[Denominator[subposition[[j]]]<Denominator[subposition[[i]]],{j,Denominator[subposition[[j]]],Abs[subposition[[j]]-subposition[[i]]]},{j,1,2}],{j,Length[subposition]}],Last];
nearestposition=Flatten[Position[sortlist[[;;,-1]],sortlist[[1,-1]]]];
fathernodeposition={sortlist[[1,1]]};
secondaryfathernodeposition={};
If[Length[nearestposition]>1,
Which[
Denominator[subposition[[sortlist[[1,1]]]]]>Denominator[subposition[[sortlist[[2,1]]]]],
fathernodeposition={sortlist[[2,1]]};secondaryfathernodeposition={sortlist[[1,1]]},
Denominator[subposition[[sortlist[[1,1]]]]]<Denominator[subposition[[sortlist[[2,1]]]]],
fathernodeposition={sortlist[[1,1]]};secondaryfathernodeposition={sortlist[[2,1]]},
True,(*if two edge connection: compare outside*)If[sortlist[[1,3]]+sortlist[[2,3]]!=1,fathernodeposition={sortlist[[1,1]],sortlist[[2,1]]},
Which[olddenarr[[ihead+1]]<olddenarr[[ihead+truelength+1]],fathernodeposition={sortlist[[1,1]]};secondaryfathernodeposition={sortlist[[2,1]]},
olddenarr[[ihead+1]]>olddenarr[[ihead+truelength+1]],fathernodeposition={sortlist[[2,1]]};secondaryfathernodeposition={sortlist[[1,1]]},
True,fathernodeposition={sortlist[[1,1]],sortlist[[2,1]]}]]
];
];
calculatecoordinate[fathernodeposition_,j_]:=(xpos=subposition[[fathernodeposition[[j]]]]*rectlength+xbase+position[[ihead+1]];
ypos=Which[subposition[[fathernodeposition[[j]]]]==0||subposition[[fathernodeposition[[j]]]]==1&&xpos-xbase==1,
ticks[[Position[ticks,denominators[[i]]][[1,1]]-1]]/.{1->tickbase}/.tickpositiondict,
subposition[[fathernodeposition[[j]]]]==1,yposition[[index+2]],
True,Denominator[subposition[[fathernodeposition[[j]]]]]/.tickpositiondict];
xpos2=position[[index+1]]+xbase;
ypos2=(denominators[[i]])/.tickpositiondict;);
(*create hidden branches*)
Do[AppendTo[secondarybranches,{Thickness[0.02*rectlength*1.1^-Denominator[position[[index+1]]]],If[errorQ,Lighter[Red,.8],GrayLevel[.9]]}];
calculatecoordinate[secondaryfathernodeposition,j];
If[(*delete ground leaf*)ypos!=ypos2||(ypos!=tickbase&&ypos==ypos2),
AppendTo[secondarybranches,Line[{{xpos,ypos},{xpos2,ypos2}}]];
];,{j,Length[secondaryfathernodeposition]}];
(*create branches*)
Do[AppendTo[branches,{errcolor[branchcolor],Thickness[0.02*rectlength*1.1^-Denominator[position[[index+1]]]]}];
calculatecoordinate[fathernodeposition,j];
If[(*delete ground leaf*)ypos!=ypos2||(ypos!=tickbase&&ypos==ypos2),
AppendTo[branches,Line[{{xpos,ypos},{xpos2,ypos2}}]];
];,{j,Length[fathernodeposition]}];
]
(*end For loop*)]
(*end while loop*)];
xbase++,{linelength}];
(*ground of tree*)
AppendTo[branches,{groundcolor,EdgeForm[Directive[groundcolor,Dashing[None]]],Rectangle[{0,0},{linelength,-.1}]}];
Framed@Show[Graphics[Flatten[{graphics,secondarybranches,branches,text}]],ImageSize->1200,PlotRange->{{1-margin,2+margin},{-0.07,All}}]]


(*export function to export the result:
a. use it outside the bowPath[] function;
b. results location: the same directory of notebook you are using and you can find folder 'exports' contains the files of results*)
export[x_]:=Module[{},Quiet[CreateDirectory[NotebookDirectory[]<>"exports"]];Column@{Export[NotebookDirectory[]<>"exports/"<>ToString[Round[AbsoluteTime[]*100]]<>".png",x],Export[NotebookDirectory[]<>"exports/"<>ToString[Round[AbsoluteTime[]*100]]<>".pdf",x]}];
