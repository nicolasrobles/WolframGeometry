(* ::Package:: *)

System`Private`NewContextPath[{"System`", "GeometrySceneDrawer`Common`"}];

(Unprotect[#]; Clear[#])& /@{
	System`FindGeometricConjectures
}


Begin["PlaneGeometry`GeometryConjectureNewDump`"]

(*
FindGeometricConjectures::usage="given region description input or its graph, return conjecture(s) in region language, also can return the corresponding graphs as option"
*)

coagulate[lists_,n_]:=FixedPoint[Function[bs,Union@@@Gather[bs,Length[Intersection[#1,#2]]>=n&]],lists];
A[twolines_]:=First@First@twolines;(*compare AB//XY*)
B[twolines_]:=Last@First@twolines;
X[twolines_]:=First@Last@twolines;
Y[twolines_]:=Last@Last@twolines;
line0[pts_]:=Det@Replace[pts,{a_,b_}:>{a,b,1},1];
combineColinear[lists_,coord_]:=SortBy[#,First@(#/.coord)&]&/@FixedPoint[Function[bs,Union@@@Gather[bs,Abs[line0[{First@#1,Last@#1,First@#2}/.coord]]<10^(-10)&]],lists];
equalClassToGraph[segs_]:=Graph[(#[[1]]<->#[[2]])&/@segs];
graphToPolygon[edges_]:=First/@edges;
sumIntersection[lists_]:=Sort[Intersection[#[[1]],#[[2]],#[[3]]]&@Total[lists,{3}]];
findSimilarTriangle[lists_]:=With[{sum=sumIntersection[lists]},If[Length[sum]>1,Transpose[Table[SortBy[Select[lists[[iii]],MemberQ[sum,Total[#]]&],Total][[All,2]],{iii,1,3}]],Missing[]]];
pickEqualEdge[list_]:=Select[GatherBy[list,Total],Length[#]==2&];
findIsosceles[list_]:=With[{ee=pickEqualEdge[list]},Transpose[{Total[ee[[All,1]],{2}]-ee[[All,1]][[All,2]]-ee[[All,2]][[All,2]],ee[[All,1]][[All,2]],ee[[All,2]][[All,2]]}]];
origin[onecircle_]:=onecircle[[2]];
extractLine[lists_,ith_,coord_]:=InfiniteLine[ReplaceAll[{lists[[ith,1]],lists[[ith,2]]},coord]];
cycleToEqual[edges_]:={First/@edges,Last/@edges}//Transpose;
cycleToEqualVerse[edges_]:={Last/@edges,First/@edges}//Transpose;
nearest[list_,point_]:=Join[If[#==Length[list],{},{list[[#+1]]}],If[#==1,{},{list[[#-1]]}]]&@Position[list,point][[1,1]];

existenceRate[phantomline_,lineList_]:=(Max[Length[Intersection[phantomline,#]]&/@lineList]-1)/(Length[phantomline]-1);



lineToString[onelist_]:=ToString/@onelist;
parallelClassToString[lines_]:=lineToString[First[lines]]<>StringJoin["//"<>lineToString[#]&/@Rest[lines]];
(*perpendicularToString[{class1_,class2_}]:=If[Length[class1]>1,"( "<>parallelClassToString[class1]<>" )",lineToString[Flatten[class1]]]<>" \[UpTee] "<>If[Length[class2]>1,"( "<>parallelClassToString[class2]<>" )",lineToString[Flatten[class2]]];*)
perpendicularToString[{class1_,class2_}]:=lineToString[class1]<>" \[UpTee] "<>lineToString[class2];
equalClassToString[lines_]:=lineToString[First[lines]]<>StringJoin["="<>lineToString[#]&/@Rest[lines]];
aPbEcToString[threeSegs_]:=lineToString[threeSegs[[3]]]<>"="<>lineToString[threeSegs[[1]]]<>"+"<>lineToString[threeSegs[[2]]];
equalAngleToString[angles_]:=If[Head[First[angles]]===List,"\[Angle]"<>lineToString[First[angles]]<>StringJoin["=\[Angle]"<>lineToString[#]&/@Rest[angles]],
							Row[{StringJoin["\[Angle]"<>lineToString[#]<>"="&/@Rest[angles]],First[angles]}]];
similarTriangleToString[tris_]:="\[EmptyUpTriangle]"<>lineToString[First[tris]]<>StringJoin["~\[EmptyUpTriangle]"<>lineToString[#]&/@Rest[tris]];
isomorphicTriangleToString[tris_]:="\[EmptyUpTriangle]"<>lineToString[First[tris]]<>StringJoin["\[TildeFullEqual]\[EmptyUpTriangle]"<>lineToString[#]&/@Rest[tris]];
isoscelesTriangleToString[list_]:="\[EmptyUpTriangle]"<>ToString/@list<>" is an isosceles triangle ("<>equalClassToString[{{list[[1]],list[[2]]},{list[[1]],list[[3]]}}]<>").";
equilateralToString[list_]:="Polygon "<>ToString/@list<>" is an equilateral polygon.";
regularToString[list_]:=Switch[Length[list],
	3, "\[EmptyUpTriangle]"<>ToString/@list<>" is a regular triangle.",
	4, "\[Square]"<>ToString/@list<>" is a square.",
	_, "Polygon "<>ToString/@list<>" is a right polygon."];
concurrentToString[lines_]:="Line "<>lineToString[First[lines]]<>StringJoin[", "<>lineToString[#]&/@Rest[lines]]<>" are concurrent.";
colinearToString[points_]:="Points "<>ToString[First@points]<>(", "<>ToString[#]&)/@Rest[points]<>" are on the same line.";
middleToString[twolines_]:=ToString@twolines[[1,2]]<>" is the midpoint of line "<>ToString@twolines[[1,1]]<>ToString@twolines[[2,2]]<>".";
typeset[{type_,data0_,priority_}]:=With[{data=ReplaceAll[data0,GeometricPoint[p_]:>p]},Switch[type,
	"parallel", parallelClassToString[data],
	"perpendicular", perpendicularToString[data],
	"equalClass", equalClassToString[data],
	"aPbEc", aPbEcToString[data],
	"equalAngle", equalAngleToString[data],
	"equalAngle90", equalAngleToString[data],
	"similarTriangle", similarTriangleToString[data],
	"isomorphicTriangle", isomorphicTriangleToString[data],
	"isoscelesTriangle", isoscelesTriangleToString[data],
	"equilateral", equilateralToString[data],
	"regular", regularToString[data],
	"concurrent", concurrentToString[data],
	"colinear", colinearToString[data],
	"middle", middleToString[data],
	_,"under construction"
	]];



smallTriangle[angle_]:=With[{m=1/5*Min[EuclideanDistance[angle[[1]],angle[[2]]],EuclideanDistance[angle[[3]],angle[[2]]]]},
		{angle[[2]],angle[[2]]+m Normalize[angle[[1]]-angle[[2]]],angle[[2]]+m Normalize[angle[[3]]-angle[[2]]]}];

smallRightAngle[angle_]:=With[{m=1/10*Min[EuclideanDistance[angle[[1]],angle[[2]]],EuclideanDistance[angle[[3]],angle[[2]]]]},
		Line[{{angle[[2]]+m Normalize[angle[[1]]-angle[[2]]],angle[[2]]+m Normalize[angle[[1]]-angle[[2]]]+m Normalize[angle[[3]]-angle[[2]]]},
			{angle[[2]]+m Normalize[angle[[3]]-angle[[2]]],angle[[2]]+m Normalize[angle[[1]]-angle[[2]]]+m Normalize[angle[[3]]-angle[[2]]]}}]];

rawToGraph[primitives_,csr_,raw_]:=With[{ss=Most@primitives,label=Last@primitives},Switch[#[[1]],
	"parallel", Graphics[Join[ss,{AbsoluteThickness[2],Hue[.58],Line[#[[2]]]},label]/.csr],
(*    "perpendicular", Graphics[Join[ss,{AbsoluteThickness[2],Hue[.58],Line[Last@#[[2]]],Line[First[#[[2]]]]},label]/.csr],*)
	"perpendicular", Graphics[Join[ss,{AbsoluteThickness[2],Hue[.58],Line[#[[2]]]},label]/.csr],
	"equalClass", Graphics[Join[ss,{AbsoluteThickness[2],Hue[.58],Line[#[[2]]]},label]/.csr],
	"aPbEc", Graphics[Join[ss,{AbsoluteThickness[2],Hue[.58],Line[{Last@#[[2]]}],AbsoluteThickness[1.5],Hue[.58,.7],Line[Most[#[[2]]]]},label]/.csr],
	"equalAngle", If[Head[First[#[[2]]]] === List,
					Graphics[ss~Join~{{Hue[.58],Opacity[0.5],Triangle[smallTriangle[#]]&/@#[[2]]}}~Join~label/.csr],
					Graphics[ss~Join~{{Hue[.58],Opacity[0.5],Triangle[smallTriangle[#]]&/@Rest[#[[2]]]}}~Join~label/.csr]
					],		
	"equalAngle90", Graphics[ss~Join~({Hue[.58],smallRightAngle/@Rest[#[[2]]]})~Join~label/.csr],
	"similarTriangle", Graphics[ss~Join~{{Hue[.58], Opacity[.25], EdgeForm[{Hue[.58], Thickness[Large]}],Polygon[#[[2]]]}}~Join~label/.csr],
	"isomorphicTriangle", Graphics[ss~Join~{{Hue[.58], Opacity[.5], EdgeForm[{Hue[.58], Thickness[Large]}],Polygon[#[[2]]]}}~Join~label/.csr],
	"isoscelesTriangle", Graphics[ss~Join~{AbsoluteThickness[1.5],Hue[.58,.7],Line[{{#[[2,2]],#[[2,3]]}}],AbsoluteThickness[2],Hue[.58],Line[{{#[[2,1]],#[[2,3]]},{#[[2,1]],#[[2,2]]}}]}~Join~label/.csr],
	"equilateral", Graphics[ss~Join~{{Transparent,EdgeForm[{Hue[.58], Thickness[Large]}],Polygon[#[[2]]]}}~Join~label/.csr],
	"regular", Graphics[ss~Join~{{Transparent,EdgeForm[{Hue[.58], Thickness[Large]}],Polygon[#[[2]]]}}~Join~label/.csr],
	"concurrent", Graphics[ss~Join~{AbsoluteThickness[2],Hue[.58],InfiniteLine[#[[1]],#[[1]]-#[[2]]]&/@#[[2]]}~Join~label/.csr],
	"colinear", Graphics[ss~Join~{AbsoluteThickness[2],Hue[.58],InfiniteLine[Last@#[[2]],#[[2,1]]-Last@#[[2]]]}~Join~label/.csr],
	"middle", Graphics[ss~Join~{AbsoluteThickness[2],Hue[.58],Line[#[[2]]]}~Join~label/.csr],
	_,"under construction"
]&@raw];


angleLanguage[data_]:=If[Head[data]===List,Angle@@data,data*180 Degree/Pi];
lineOrThrough[data_]:=If[Length[#]==2,Line[#],LineThrough[#]]&@data;

language[{type_,data_,priority_}]:=Switch[type,
	"parallel", Parallel@@lineOrThrough/@data,
(*	"perpendicular", And@@(Perpendicular@@lineOrThrough/@#&/@Tuples[data]),*)
	"perpendicular", Perpendicular@@lineOrThrough/@data,
	"equalClass", Equal@@EuclideanDistance@@@data,
	"aPbEc", Equal[#[[3]],Plus[#[[1]],#[[2]]]]&@(EuclideanDistance@@@data),
	"equalAngle", Equal@@(angleLanguage/@RotateLeft[data]),
	"equalAngle90", Equal@@(angleLanguage/@RotateLeft[data]),
	"similarTriangle", Similar@@Triangle/@data,
	"isomorphicTriangle", Congruent@@Triangle/@data,
	"isomorphicCircle", Congruent@@(CircleThrough[#[[1]],#[[2]]]&/@data),
(*	"isoscelesTriangle", isoscelesTriangleToString[data],*)
	"equilateral", equilateral@Polygon@data,
	"regular", If[Length[data]==3,Regular@Triangle@data,Regular@Polygon@data],
	"concurrent", Concurrent@@lineOrThrough/@data,
	"colinear", Collinear@@data,
	"middle", data[[2,1]]==Midpoint[Line[{data[[1,1]],data[[2,2]]}]],
	_,"Not Available"
	];	


candidateRawData[sceneAssoc_,csr_]:=Module[
{isoPts,lines,infiniteLines,circles,polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,regularPolygons,midpointTriples,
angleValueList,pointList,polygonList,lineList,lineSegments,
parallelLineClass,parallelLineClassAssoc,equalLineCouple,equalLineClass,
allLineClass,perpendicularClass,
equilateralPolygons,newRegular,segLengths,nnumber,aPbEc,intersectingLines,
angleList,angleAssoc,equalAngleClass,
similarTriangleClass,isoscelesTriangle,regularTriangle,isomorphicTriangleClass,radiusList,isomorphicCircleClass,
concurrentLineClass,equalLineClassClean,angleListClean,equalAngleClassClean,nonParallelLines,
vecs,dirs,phantomLineClass,phantomLineExistenceRate,segs,middlePoint,segsInv,
\[Epsilon]=10^(-5),result={},
vec,conPoint,cyc},

(*initialize*)
{isoPts,lines,infiniteLines,midpointTriples,circles,polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,regularPolygons}=
Lookup[sceneAssoc,{"isolatedPoints","lines","infiniteLines","midpointTriples","circles","polygons","nonintersectingPolygons","counterclockwisePolygons","convexPolygons","regularPolygons"},{}];

(*create lists*)
angleValueList=Union[(Pi/6)* Range[5],(Pi/4)*Range[3]];
pointList=Select[Keys@csr,Head[#]==GeometricPoint&];
lines=ReplaceAll[lines,{First@#,Last@#}->#&/@midpointTriples];
polygonList=Join[polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,regularPolygons];
lineList=SortBy[#,First@(#/.csr)&]&/@coagulate[Join[lines,infiniteLines,Flatten[Partition[#,2,1,{1,1}]&/@polygonList,1]],2];
lineList=Select[#,Head[#]==GeometricPoint&]&/@lineList;
(*lineSegments=Flatten[Subsets[#,{2}]&/@lineList,1];*)
(*If[Length[Join[polygonList,circles]]>0,\[Epsilon]=10^(-3)];*)

(*check parrallel, perpendicular lines*)
vecs=Select[AssociationMap[-Subtract@@(#/.csr)&,Tuples[pointList,{2}]],#[[1]]>0||(#[[1]]==0&&#[[2]]>0)&];
dirs=ArcTan@@#&/@vecs;
phantomLineClass=Map[SortBy[#,First@(#/.csr)&]&,coagulate[#,1]&/@Gather[Keys[dirs],Chop[dirs[#1]-dirs[#2],\[Epsilon]]==0&],{2}];
phantomLineExistenceRate=AssociationMap[existenceRate[#,coagulate[Join[lineList,midpointTriples],2]]&,Flatten[phantomLineClass,1]];
allLineClass=DeleteCases[Select[#,phantomLineExistenceRate[#]>0&]&/@phantomLineClass,{}];
parallelLineClass=Select[allLineClass,Length[#]>1&];
perpendicularClass=
Select[Subsets[allLineClass,{2}],Chop[Abs[dirs@Part[#[[1,1]],{1,2}]-dirs@Part[#[[2,1]],{1,2}]]-Pi/2,\[Epsilon]]==0&];
lineSegments=Flatten[Subsets[#,{2}]&/@Flatten[allLineClass,1],1];
AppendTo[result,{"parallel",#,1}]&/@parallelLineClass;
AppendTo[result,{"perpendicular",#,3}]&/@Flatten[Tuples/@perpendicularClass,1];
AppendTo[result,{"colinear",#,Floor[100-50*phantomLineExistenceRate[#]]}]&/@Select[Keys@Select[phantomLineExistenceRate,(0<#<1)&],Length[#]>=3&];

(*check equal angles*)
angleList=With[{int=Intersection[#[[1]],#[[2]]]},Join[Complement[#[[1]],int],int,Complement[#[[2]],int]]]&/@Select[Subsets[lineSegments,{2}],Intersection[#[[1]],#[[2]]]=!={}&];
angleAssoc=Select[AssociationMap[Function[{a,o,b},Min[#,2 Pi-#]&@First@Abs[Lookup[dirs,{{o,b}},dirs[{b,o}]-Pi]-Lookup[dirs,{{o,a}},dirs[{a,o}]-Pi]]]@@#&,angleList],0<Chop[#,\[Epsilon]]<Pi&];
angleAssoc=Join[AssociationMap[#&,angleValueList],angleAssoc];
equalAngleClass=Select[Gather[Keys@angleAssoc,Chop[angleAssoc[#1]-angleAssoc[#2],\[Epsilon]]==0&],Length[#]>1&];
intersectingLines=Select[Subsets[lineList,{2}],Intersection[#[[1]],#[[2]]]=!={}&];
angleListClean=Function[al,Join[al,Reverse/@al]]@Flatten[With[{intPoint=Intersection[#[[1]],#[[2]]][[1]]},Tuples[{nearest[#[[1]],intPoint],{intPoint},nearest[#[[2]],intPoint]}]]&/@intersectingLines,1]~Join~angleValueList;
equalAngleClassClean=Select[Intersection[#,angleListClean]&/@equalAngleClass,Length[#]>=2&];
If[First[#] === Pi/2,
	AppendTo[result,{"equalAngle90",#,2}],
	AppendTo[result,{"equalAngle",#,4}]
	]&/@equalAngleClassClean;

(*find similar triangles*)
similarTriangleClass=findSimilarTriangle/@Subsets[DeleteCases[equalAngleClass,_Times,2],{3}]//DeleteMissing;
AppendTo[result,{"similarTriangle",#,25}]&/@similarTriangleClass;

(*find Isoceles triangles and update similar triangles*)
(* Isoceles Triangle ABC => AB=AC\[NotEqual]BC *)
isoscelesTriangle=Select[findIsosceles/@equalAngleClass,Length[#]>0&];
AppendTo[result,{"similarTriangle",#,24}]&/@Select[isoscelesTriangle,Length[#]>=2&];
similarTriangleClass=Join[similarTriangleClass,Select[isoscelesTriangle,Length[#]>=2&]];
isoscelesTriangle=Flatten[isoscelesTriangle,1];
AppendTo[result,{"isoscelesTriangle",#,20}]&/@isoscelesTriangle;

(*check equal line segments*)
segs=AssociationMap[Norm@First@Lookup[vecs,{#},vecs[Reverse@#]]&,lineSegments];
equalLineClass=Select[Gather[lineSegments,Chop[segs[#1]-segs[#2],\[Epsilon]]==0&],Length[#]>=2&];
middlePoint=Select[#, (Length@Union@Flatten[#]==3 && Chop[dirs[#[[1]]]-dirs[#[[2]]],\[Epsilon]]==0) &]&@Flatten[Subsets[#,{2}]&/@equalLineClass,1];
AppendTo[result,{"middle",#,16}]&/@Select[middlePoint,!MemberQ[Union/@midpointTriples,Union@@#]&];

(*check lengths a+b=c*)
aPbEc=SortBy[#,segs[#]&]&/@Select[Subsets[lineSegments,{3}],Length[Union[Flatten[#]]]>3 && Chop[(Total[#]-2Max[#])&@(segs/@#),\[Epsilon]]==0&];
AppendTo[result,{"aPbEc",#,Length[Union[Flatten[#]]]}]&/@aPbEc;

(*clean equal relation, and find equilateral polygons*)
equilateralPolygons={};
equalLineClassClean=Select[If[(cyc=FindCycle[equalClassToGraph[#],Infinity,All])=!={},
		equilateralPolygons=Join[equilateralPolygons,graphToPolygon/@cyc];
		Join[Complement[#,Flatten[cycleToEqual/@cyc,1],Flatten[cycleToEqualVerse/@cyc,1]],cycleToEqual[First/@cyc]],#]&/@equalLineClass,Length[#]>1&];
equalLineClassClean=Complement[equalLineClassClean,middlePoint];
AppendTo[result,{"equalClass",#,8}]&/@equalLineClassClean;

(*find regular polygons*)
newRegular=Select[equilateralPolygons,Chop[First@Lookup[angleAssoc,{Part[#,{1,2,3}]},angleAssoc[Part[#,{3,2,1}]]]-(Length[#]-2)Pi/Length[#],\[Epsilon]]==0&];
AppendTo[result,{"equilateral",#,30}]&/@Complement[equilateralPolygons,newRegular];
newRegular=Complement[DeleteDuplicatesBy[Join[regularPolygons,newRegular],Total],regularPolygons];
regularPolygons=Join[regularPolygons,newRegular];
AppendTo[result,{"regular",#,100}]&/@newRegular;

(*use regular triangle to update similar triangles*)
regularTriangle=Select[regularPolygons,Length[#]==3&];
If[Length[regularTriangle]>=2,{AppendTo[similarTriangleClass,regularTriangle];
								AppendTo[result,{"similarTriangle",regularTriangle,5}]}];

(*find isomorphic triangle*)
isomorphicTriangleClass=Flatten[Select[Gather[#,Chop[First@Lookup[segs,{Part[#1,{1,2}]},segs[Part[#1,{2,1}]]]-First@Lookup[segs,{Part[#2,{1,2}]},segs[Part[#2,{2,1}]]],\[Epsilon]]==0&],Length[#]>1&]&/@similarTriangleClass,1];
AppendTo[result,{"isomorphicTriangle",#,50}]&/@isomorphicTriangleClass;

(*circles*)
radiusList=(EuclideanDistance[A[#],origin[#]]/.csr)&/@circles;
isomorphicCircleClass=Function[num,circles[[num]]]/@coagulate[Select[Subsets[Range[Length[circles]],{2}],Abs[radiusList[[#[[1]]]]-radiusList[[#[[2]]]]]<\[Epsilon]&],1];
AppendTo[result,{"isomorphicCircle",#,50}]&/@isomorphicCircleClass;

(*concurrent lines*)
nonParallelLines=Select[Flatten[Tuples[{#[[1]],#[[2]],#[[3]]}]&/@Subsets[allLineClass,{3}],1], 
					(DisjointQ[#[[1]],#[[2]]]) && (DisjointQ[#[[2]],#[[3]]]) && (DisjointQ[#[[1]],#[[3]]]) &];
concurrentLineClass=coagulate[#,2]&@Select[nonParallelLines,
				(conPoint=Flatten[Values[Solve[{ppp,qqq}\[Element]extractLine[#,1,csr]&&{ppp,qqq}\[Element]extractLine[#,2,csr],{ppp,qqq}]]])=!={}
				&& Length[conPoint]==2
				&& RegionDistance[extractLine[#,3,csr],conPoint]<\[Epsilon]&];
AppendTo[result,{"concurrent",#,90}]&/@concurrentLineClass;

result=Reverse[SortBy[result,Last]]

]


FindGeometricConjectures[graph_Annotation,num_:Infinity,type_:"Language"]:=Module[
{anno,sceneList,sceneAssociation,primitives,csr,rawdata,l,dup,kn},
anno=Replace[graph,Annotation[g_,c_Association]:>c];
{sceneList,sceneAssociation,primitives,csr}=Lookup[anno,{"sceneList","sceneAssociation","primitives","coordinates"},{}];

rawdata=candidateRawData[sceneAssociation,csr];

kn=Union@{Union@Flatten@#[[1]],Union@Flatten@#[[2]]}&/@(Cases[sceneList,Perpendicular[x_,y_]->{x,y}]/.{Line->List,LineThrough->List});
dup=Select[rawdata, #[[1]]=="perpendicular" && MemberQ[kn,Union@(Union@Flatten@#&/@#[[2]])]&];
rawdata=SortBy[Complement[rawdata,dup],-Last[#]&];

l=Min[Length[rawdata],num];
If[type=="Diagram",
TabView[Table[typeset[rawdata[[n]]]->rawToGraph[primitives,csr,rawdata[[n]]],{n,l}]/.g_Graphics:>Show[g,ImageSize->{{400},{400}}],ControlPlacement->Left],
Take[language/@rawdata,l]]
]

FindGeometricConjectures[geoDescription_List,num_,type_]:=With[
{graph=FindGeometricSceneGraphics@geoDescription},
(*Print[graph];*)
If[Head[graph]===Annotation,
FindGeometricConjectures[graph,num,type],
Defer@FindGeometricConjectures[geoDescription,num,type]]
]//Quiet

FindGeometricConjectures[geoDescription_List,num_]:=With[
{graph=FindGeometricSceneGraphics@geoDescription},
(*Print[graph];*)
If[Head[graph]===Annotation,
FindGeometricConjectures[graph,num],
Defer@FindGeometricConjectures[geoDescription,num]]
]//Quiet

FindGeometricConjectures[geoDescription_List]:=With[
{graph=FindGeometricSceneGraphics@geoDescription},
(*Print[graph];*)
If[Head[graph]===Annotation,
FindGeometricConjectures[graph],
Defer@FindGeometricConjectures[geoDescription]]
]//Quiet



End[]

System`Private`RestoreContextPath[];

{
	System`FindGeometricConjectures
}
