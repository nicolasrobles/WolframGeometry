(* ::Package:: *)

(* ::Section::RGBColor[0, 1, 1]::Closed:: *)
(*Begin package context change*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
(* Wolfram Language package *)

System`Private`NewContextPath[{"System`"}];

(Unprotect[#]; Clear[#])& /@{
	System`AffineHull,
	System`Angle,
	System`AngleBisector,
	System`CircleThrough,
	System`Collinear,
	System`Coplanar,
	System`Concurrent,
	System`Convex,
	System`Cyclic,
	System`EqualAngles,
	System`Equiangular,
	System`Equilateral,
	System`GeometricPoint,
	System`GeometricScene,
	System`InfiniteLineThrough,
	System`Inradius,
	System`LineThrough,
	System`Midpoint,
	System`Parallel,
	System`PerpendicularBisector,
	System`Radius,
	System`RegionCenter,
	System`Regular,
	System`Similar,
	System`Tangent,
	System`Unspecified,
	System`FindGeometricSceneGraphics,
	System`GeometricSceneToGraphicsPrimitives,
	System`FindGeometricCoordinates
}


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
Begin["GeometrySceneDrawer`Common`"]


(* ::Section::RGBColor[0, 1, 1]:: *)
(*Main code*)


(* ::Subsection::RGBColor[0, 1, 1]:: *)
(*Groebner basis code*)


(* ::Subsubsection::RGBColor[0, 1, 1]:: *)
(*Polynomial expressions representing geometric quantities*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
point[x_]:={x[1],x[2]}


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoVector[x_,y_]:={y[1]-x[1],y[2]-x[2]}
GeoDotProduct[{x_,y_},{z_,w_}]:=GeoVector[x,y].GeoVector[z,w]
Geo2DCross[{x_,y_},{z_,w_}]:=Last@(Cross@@(Append[0]/@{GeoVector[x,y],GeoVector[z,w]}))
GeoDistSquared[x_,y_]:=GeoDotProduct[{x,y},{x,y}]


(* ::Subsubsection::RGBColor[0, 1, 1]:: *)
(*Boolean operators on polynomials for Groebner basis techniques*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoEqualExpressions[exps_]:=Function[{exp},(First@exps)-exp]/@(Rest@exps)
GeoOr[pols_]:=Times@@Flatten[{pols}]
GeoNot[pols_]:=Times@@(Function[poly,1-notVar[Unique[]]poly]/@Flatten[{pols}])


(* ::Subsubsection::RGBColor[0, 1, 1]:: *)
(*Polynomials representing geometric statements when set equal to zero*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoCosAngle[Angle[x_,y_,z_]->\[Theta]_]:=GeoEqualExpressions[{GeoDotProduct[{y,x},{y,z}],distance[x,y]*distance[z,y]*N[Cos[\[Theta]]]}]
GeoCosSquaredAngle[Angle[x_,y_,z_]->\[Theta]_]:=GeoEqualExpressions[{GeoDotProduct[{y,x},{y,z}]^2,GeoDistSquared[x,y]*GeoDistSquared[z,y]*N[Cos[\[Theta]]^2]}]
GeoPerpendicular[{x_,y_},{z_,w_}]:=GeoDotProduct[{x,y},{z,w}]
GeoRightAngle[x_,y_,z_]:=GeoPerpendicular[{x,y},{y,z}]
GeoParallel[segs_]:=With[{x=(First@First@segs),y=(Last@First@segs)},Function[{z,w},GeoEqualExpressions[{(y[1]-x[1])(w[2]-z[2]),(w[1]-z[1])(y[2]-x[2])}]]@@@(Rest@segs)]
GeoCollinear[pointsOnLine_]:=Function[{x,y,z},GeoParallel[{{x,y},{y,z}}]]@@@Subsequences[pointsOnLine,{3}]
GeoGeneralPosition[pts_]:=(GeoNot@*GeoCollinear)/@Subsets[pts,{3}]
GeoIntersection[lineSegments_,w_]:=Function[{x,y},GeoCollinear[{x,w,y}]]@@@lineSegments


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
lineTriple[x_,y_,z_]:=GeoEqualExpressions[{point[y],point[x]+paramVar[Unique[]]*GeoVector[x,z]}]
GeoLine[pointsOnLine_]:=lineTriple@@@Subsequences[pointsOnLine,{3}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoMidpoint[x_,y_,z_]:={x[1]-2y[1]+z[1],x[2]-2y[2]+z[2]}
GeoSamePoint[pts_]:=GeoEqualExpressions/@{Through[pts[1]],Through[pts[2]]}
GeoDistinctPoints[pts_]:=(GeoNot@*GeoSamePoint)/@Subsets[pts,{2}]
GeoSameLength[segs_]:=GeoEqualExpressions[GeoDistSquared@@@segs]
GeoCircleCenter[pointsOnCircle_,w_]:=GeoSameLength[Thread[List[pointsOnCircle,w]]]
GeoOrthocenter[{x_,y_,z_},w_]:={GeoPerpendicular[{x,w},{y,z}],GeoPerpendicular[{y,w},{x,z}],GeoPerpendicular[{z,w},{x,y}]}


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
distancePolynomials[segs_]:=Function[{x,y},GeoEqualExpressions[{distance[x,y]^2,GeoDistSquared[x,y]}]]@@@segs


(* ::Subsubsection::RGBColor[0, 1, 1]:: *)
(*Functions for applying the Groebner basis technique of automated theorem proving*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
(*extractVars[var_,exprs_]:=var/@DeleteDuplicates[Function[index,Extract[exprs,ReplacePart[index,Length[index]->Last[index]+1]]]/@Position[exprs,var]]*)
extractVars[var_,exprs_]:=DeleteDuplicates[Cases[exprs,var[x__],All]]
extractNotVars[exprs_]:=extractVars[notVar,exprs]
extractParamVars[exprs_]:=extractVars[paramVar,exprs]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoConsistentQ[pols_,vars_]:=With[{polyList=Flatten[pols],varList=Flatten[vars]},GroebnerBasis[polyList,varList]=!={1}]
GeoTheoremQ[pts_,hypPols_,conPols_,segs_:{}]:=With[{theoremPols={hypPols,GeoNot[conPols],distancePolynomials[segs]}},\[Not]GeoConsistentQ[theoremPols,{(distance@@@segs),(point/@pts),extractNotVars[theoremPols]}]]


(* ::Subsection::RGBColor[0, 1, 1]::Closed:: *)
(*Nonpolynomial code*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoDist[x_,y_]:=Sqrt[GeoDistSquared[x,y]]
GeoCos[x_,y_,z_]:=GeoDotProduct[{y,x},{y,z}]/(GeoDist[x,y]*GeoDist[z,y])
GeoSin[x_,y_,z_]:=Geo2DCross[{y,x},{y,z}]/(GeoDist[x,y]*GeoDist[z,y])
GeoUnorientedAngle[x_,y_,z_]:=ArcCos[GeoCos[x,y,z]]
GeoCos[Angle[x_,y_,z_]-> \[Theta]_]:=GeoEqualExpressions[{GeoCos[x,y,z],Cos[\[Theta]]}]
GeoSin[Angle[x_,y_,z_]-> \[Theta]_]:=GeoEqualExpressions[{GeoSin[x,y,z],Sin[\[Theta]]}]
GeoOrientedAngle[Angle[x_,y_,z_]-> \[Theta]_]:={GeoCos[Angle[x,y,z]->\[Theta]],GeoSin[Angle[x,y,z]->\[Theta]]}


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoCosAngleQ[Angle[x_,y_,z_]->\[Theta]_]:=(GeoCos[x,y,z]==If[(Head@\[Theta])===Angle,GeoCos@@\[Theta],Cos[\[Theta]]])
GeoSinAngleQ[Angle[x_,y_,z_]->\[Theta]_]:=(GeoSin[x,y,z]==If[(Head@\[Theta])===Angle,GeoSin@@\[Theta],Sin[\[Theta]]])
GeoOrientedAngleQ[Angle[x_,y_,z_]->\[Theta]_]:=GeoCosAngleQ[Angle[x,y,z]->\[Theta]]\[And]GeoSinAngleQ[Angle[x,y,z]->\[Theta]]
GeoCosSquaredAngleQ[Angle[x_,y_,z_]->\[Theta]_]:=(GeoDotProduct[{y,x},{y,z}]^2==GeoDistSquared[x,y]*GeoDistSquared[z,y]*Cos[\[Theta]]^2)
GeoSinSquaredAngleQ[Angle[x_,y_,z_]->\[Theta]_]:=(Geo2DCross[{y,x},{y,z}]^2==GeoDistSquared[x,y]*GeoDistSquared[z,y]*Sin[\[Theta]]^2)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoPerpendicularQ[{x_,y_},{z_,w_}]:=(GeoDotProduct[{x,y},{z,w}]==0)
GeoRightAngleQ[x_,y_,z_]:=GeoPerpendicularQ[{x,y},{y,z}]
parallelQ[{x_,y_},{z_,w_}]:=(y[1]-x[1])(w[2]-z[2])==(w[1]-z[1])(y[2]-x[2])
GeoParallelQ[segs_]:=And@@(parallelQ@@@Subsequences[segs,{2}])
collinearQ[x_,y_,z_]:=parallelQ[{x,y},{y,z}]
GeoCollinearQ[pointsOnLine_]:=And@@(collinearQ@@@Subsequences[pointsOnLine,{3}])
intersectionQ[w_][x_,y_]:=collinearQ[x,w,y]
GeoIntersectionQ[lineSegments_,w_]:=And@@(intersectionQ[w]@@@lineSegments)
(*GeoParallelQ[segs_]:=With[{x=(First@First@segs),y=(Last@First@segs)},And@@(Function[{z,w},(y[1]-x[1])(w[2]-z[2])==(w[1]-z[1])(y[2]-x[2])]@@@(Rest@segs))]
GeoCollinearQ[pointsOnLine_]:=And@@(Function[{x,y,z},GeoParallelQ[{{x,y},{y,z}}]]@@@Subsequences[pointsOnLine,{3}])
GeoIntersectionQ[lineSegments_,w_]:=And@@(Function[{x,y},GeoCollinearQ[{x,w,y}]]@@@lineSegments)*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
lineTripleQ[x_,y_,z_]:=(point[y]==point[x]+paramVar[Unique[]]*GeoVector[x,z])
GeoLineQ[pointsOnLine_]:=And@@(lineTriple@@@Subsequences[pointsOnLine,{3}])


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoCentroid[pts_]:=Mean[point/@pts]
GeoCentroidQ[pts_,w_]:=point[w]==GeoCentroid[pts]
GeoMidpointQ[x_,y_,z_]:=GeoCentroidQ[{x,z},y]
GeoSamePointQ[pts_]:=And@@(Equal@@@{Through[pts[1]],Through[pts[2]]})
GeoDistinctPointsQ[pts_]:=And@@(Or[#1[1]<#2[1],#2[1]<#1[1],#1[2]<#2[2],#2[2]<#1[2]]&@@@Subsets[pts,{2}])
GeoSameLengthQ[segs_]:=Equal@@(GeoDistSquared@@@segs)
GeoCircleCenterQ[pointsOnCircle_,w_]:=GeoSameLengthQ[Thread[List[pointsOnCircle,w]]]
GeoOrthocenterQ[{x_,y_,z_},w_]:=And[GeoPerpendicularQ[{x,w},{y,z}],GeoPerpendicularQ[{y,w},{x,z}],GeoPerpendicularQ[{z,w},{x,y}]]


(* ::Text::RGBColor[0, 1, 1]:: *)
(*Negative orientation polynomial means counterclockwise triple.*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
(*
orientationPolynomial[x_,y_,z_]:=x[2]*y[1]-x[1]*y[2]-x[2]*z[1]+y[2]*z[1]+x[1]*z[2]-y[1]*z[2]
nonintersectingQ[{x_,y_},{z_,w_}]:=((orientationPolynomial[x,y,z]*orientationPolynomial[x,y,w]>0)\[Or](orientationPolynomial[z,w,x]*orientationPolynomial[z,w,y]>0))
GeoNonintersectingQ[segs_]:=And@@(nonintersectingQ@@@Subsets[segs,{2}])
GeoConvexQ[pts_]:=And@@(#<0&/@(orientationPolynomial@@@Subsets[pts,{3}]))
*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoAbs[expr_]:=Sqrt[expr^2]
GeoTriangleArea[x_,y_,z_]:=GeoAbs[Geo2DCross[{y,x},{y,z}]]/2
GeoSignedPolygonArea[pts_]:=Total[#2[1](#3[2]-#1[2])&@@@Subsequences[Join[{Last@pts},pts,{First@pts}],{3}]]/2
nonintersectingQ[{x_,y_},{z_,w_}]:=((GeoSignedPolygonArea[{x,y,z}]*GeoSignedPolygonArea[{x,y,w}]>0)\[Or](GeoSignedPolygonArea[{z,w,x}]*GeoSignedPolygonArea[{z,w,y}]>0))
GeoNonintersectingQ[segs_]:=And@@(nonintersectingQ@@@Subsets[segs,{2}])
counterclockwiseQ[pts_]:=GeoSignedPolygonArea[pts]>0
GeoCounterclockwiseQ[pts_]:=counterclockwiseQ[pts]\[And]GeoNonintersectingQ[Subsequences[Append[pts,First@pts],{2}]]
GeoConvexQ[pts_]:=And@@(counterclockwiseQ/@Subsets[pts,{3}])


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
(*GeoAntiparallelQ[{x_,y_},{z_,w_}]:=parallelQ[{x,y},{z,w}]\[And]nonintersectingQ[{x,w},{y,z}]*)
orientedParallelQ[{x_,y_},{z_,w_}]:=parallelQ[{x,y},{z,w}]\[And]nonintersectingQ[{x,z},{y,w}]
GeoOrientedParallelQ[segs_]:=And@@(orientedParallelQ@@@Subsequences[segs,{2}])
GeoAntiparallelQ[{x_,y_},{z_,w_}]:=orientedParallelQ[{x,y},{w,z}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
(*GeoAngleSplitQ[pts_,w_]:=With[{pairs=Subsets[pts,{2}]},Equal@@(GeoCos[#1,w,#2]&@@@pairs)\[And]And@@(GeoConvexQ[{#1,#2,w}]&@@@pairs)]*)
unitPoint[x_,y_]:=AssociationThread[{1,2}->GeoVector[x,y]/GeoDist[x,y]]
angleBisectorQ[{x_,y_,z_},w_]:=With[{a=unitPoint[w,x],b=unitPoint[w,y],c=unitPoint[w,z]},(GeoVector[a,c].GeoVector[w,y]==0)\[And]GeoConvexQ[{a,b,c}]]
GeoAngleSplitQ[pts_,w_]:=And@@(angleBisectorQ[#,w]&/@Subsequences[pts,{3}])


(* ::Subsection::RGBColor[0, 1, 1]::Closed:: *)
(*Angle solving code*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
findLinePositions[extendedLines_][pt_]:=With[{positionLists=Transpose[Position[extendedLines,pt]]},AssociationThread[(First@positionLists)->(Last@positionLists)]]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
createNeighborRules[extendedLines_,bool_][listIndex_,pointIndex_]:=extendedLines[[listIndex,pointIndex+If[bool,-1,1]]]->{listIndex,bool}


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
findNeighbors[pointToLinePositions_,extendedLines_][pt_]:=Association@@With[{incidentLinePositions=KeyValueMap[List,pointToLinePositions[pt]]},
(createNeighborRules[extendedLines,True]@@@incidentLinePositions)~Join~(createNeighborRules[extendedLines,False]@@@incidentLinePositions)]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
nextInLine[pointToNeighbors_,extendedLines_,pointToLinePositions_][x_,y_]:=With[{lineIndex=First@pointToNeighbors[y][x]},extendedLines[[lineIndex,2*pointToLinePositions[y][lineIndex]-pointToLinePositions[x][lineIndex]]]]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
consecutiveAnglePolynomials[pt_,neighbors_][index1_,index2_]:=Angle[neighbors[[index1]],pt,neighbors[[index2]]]-Total[Table[Angle[neighbors[[index]],pt,neighbors[[index+1]]],{index,index1,index2-1}]]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
createPointAnglePolynomials[pointToNeighbors_,pointToLinePositions_,extendedLines_][pt_]:=
With[{neighbors=Keys[pointToNeighbors[pt]],incidentLineCount=Length[pointToLinePositions[pt]],nil=nextInLine[pointToNeighbors,extendedLines,pointToLinePositions]},
{
(consecutiveAnglePolynomials[pt,neighbors]@@@Flatten[Table[{index1,index2},{index1,1,2*incidentLineCount-2},{index2,index1+2,2*incidentLineCount}],1]),
Table[Angle[neighbors[[index]],pt,(First@neighbors)]-Total[Table[Angle[neighbors[[index]],pt,neighbors[[i+1]]],{i,index,2*incidentLineCount-1}]]-Angle[(Last@neighbors),pt,(First@neighbors)],{index,2,2*incidentLineCount-1}],
360\[Degree]-Total[Thread[Angle[neighbors,pt,Append[Rest@neighbors,First@neighbors]]]],
Table[{Angle[neighbors[[i]],pt,neighbors[[incidentLineCount+i]]]-180\[Degree],Angle[neighbors[[i]],pt,neighbors[[i+1]]]-Angle[nil[neighbors[[i]],pt],pt,nil[neighbors[[i+1]],pt]]},{i,incidentLineCount}]
}
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
findIntersectionPoints[lines_,intersector_][line_]:=lines[[line]]\[Intersection]lines[[intersector]]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
plapAngles[extendedLines_,pointToLinePositions_,intersector_][line_,possiblePoint_]:=
If[possiblePoint=!={},With[{pt=First@possiblePoint},Angle[extendedLines[[line,pointToLinePositions[pt][line]+1]],pt,extendedLines[[intersector,pointToLinePositions[pt][intersector]+1]]]],{}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
plap[lines_,class_,extendedLines_,pointToLinePositions_][intersector_]:=
With[{intersectionPoints=findIntersectionPoints[lines,intersector]/@class},
With[{angles=Cases[MapThread[plapAngles[extendedLines,pointToLinePositions,intersector],{class,intersectionPoints}],_Angle]},
If[Length[angles]>1,(First@angles)-(Rest@angles),{}]
]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
createParallelLineAnglePolynomials[lineIndices_,lines_,extendedLines_,pointToLinePositions_][class_]:=
With[{intersectors=Complement[lineIndices,class]},
plap[lines,class,extendedLines,pointToLinePositions]/@intersectors
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
createPolygonAnglePolynomials[polygon_]:=Total[Angle@@@Partition[Join[polygon,polygon[[1;;2]]],3,1]]-(Length[polygon]-2)*180\[Degree]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
createAngles[pointToNeighbors_][pt_]:=Flatten[Function[{neighbor1,neighbor2},{Angle[neighbor1,pt,neighbor2],Angle[neighbor2,pt,neighbor1]}]@@@Subsets[Keys[pointToNeighbors[pt]],{2}]]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
anglePolsVars[pts_,lines_,parallelLineClasses_,polygons_,givenAnglePolynomials_]:=Module[
{pointCount=Length[pts],pointRank=First/@PositionIndex[pts],lineCount=Length[lines],
lineIndices,extendedLines,extendedPoints,pointToLinePositions,pointToNeighbors,nextInLine,pointAnglePolynomials,polygonAnglePolynomials,parallelLineAnglePolynomials,angleList},

lineIndices=Range[lineCount];
extendedLines={s[#],(Sequence@@lines[[#]]),t[#]}&/@lineIndices;
extendedPoints=pts~Join~Flatten[{s[#],t[#]}&/@lineIndices];
pointToLinePositions=AssociationMap[findLinePositions[extendedLines],extendedPoints];
(*point x associated to association whose keys are the neighbors of x, in counterclockwise order, 
and whose values are ordered pairs whose first element is the line they share with x, and whose second element is True if they are left of x and False otherwise*)
pointToNeighbors=AssociationMap[findNeighbors[pointToLinePositions,extendedLines],pts];

(*polynomials that equal zero when consecutive angle values sum to the value of their combined angle
polynomials that equal zero when supplementary non-reflex angles sum to 180
polynomials that equal zero when opposite angles created by intersecting lines are equal*)
pointAnglePolynomials=createPointAnglePolynomials[pointToNeighbors,pointToLinePositions,extendedLines]/@pts;

(*polynomials that equal zero when angles formed by a given line and a set of parallel lines are the same*)
parallelLineAnglePolynomials=createParallelLineAnglePolynomials[lineIndices,lines,extendedLines,pointToLinePositions]/@parallelLineClasses;

(*polynomials that equal zero when interior angles of n-gons sum to (n-2)180*)
polygonAnglePolynomials=createPolygonAnglePolynomials/@polygons;

angleList=Join@@(createAngles[pointToNeighbors]/@pts);

{Flatten[Join[pointAnglePolynomials,polygonAnglePolynomials,parallelLineAnglePolynomials,givenAnglePolynomials]],angleList}
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeoAngleSolver[args___]:=Module[{anglePolynomials,angleVariables},
{anglePolynomials,angleVariables}=anglePolsVars[args];
Select[(First@Quiet@Solve[And@@(Function[polynomial,Equal[polynomial,0]]/@anglePolynomials),angleVariables]),Function[rule,And@@Append[Through[{FreeQ[s],FreeQ[t]}[rule]],FreeQ[(Last@rule),Angle]]]]
]


(* ::Subsection::RGBColor[0, 1, 1]:: *)
(*Scene drawing code*)


(* ::Subsubsection::RGBColor[0, 1, 1]:: *)
(*Final method code*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
splitCoords[a_->{a1_,a2_}]:={{a[1],a1},{a[2],a2}}


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
assignSeeds[coordSeeds_,unseededVars_,notVars_,paramVars_]:=Join[
coordSeeds,
{#,RandomReal[3]}&/@unseededVars,
{#,0}&/@notVars,
Join@@((Transpose@{#,Array[Identity,2+Length@#,{0,1}][[2;;-2]]})&/@paramVars)
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
(*Join@@((Transpose@{#,Sort[RandomReal[{0,1},Length@#]]})&/@paramVars)*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
vrGenerator[objective_,constraints_,formattedVars_]:=Quiet@FindMinimum[Evaluate@{objective,And@@constraints},assignSeeds@@formattedVars,Method->"IPOPT"]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
vrGenerator1[objective_,constraints_,formattedVars_]:=FindMinimum1[Evaluate@{objective,And@@constraints},assignSeeds@@formattedVars,Method->"IPOPT"]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
vrGenerator2[objective_,constraints_,formattedVars_]:=Module[{value,rules},
Print[FindMinimum1[Evaluate@{objective,And@@constraints},assignSeeds@@formattedVars,Method->"IPOPT"]];
{value,rules}=Quiet[FindMinimum[Evaluate@{objective,And@@constraints},assignSeeds@@formattedVars,Method->"IPOPT"]];
Print[{value,rules}];
{value,rules}
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
newRules[objective_,unfixedRules_,fixedVarRules_,equalCons_,precision_]:=FindMinimum[
Evaluate[(objective+1000*Total@((#1-#2)^2&@@@Join@@(Subsequences[List@@#,{2}]&/@equalCons)))/.fixedVarRules],
List@@@unfixedRules,
WorkingPrecision->precision+15,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision,
Method->"Newton"
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
newRules1[objective_,unfixedRules_,fixedVarRules_,equalCons_,precision_]:=FindMinimum1[
Evaluate[(objective+1000*Total@((#1-#2)^2&@@@Join@@(Subsequences[List@@#,{2}]&/@equalCons)))/.fixedVarRules],
List@@@unfixedRules,
WorkingPrecision->precision+15,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision,
Method->"Newton"
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
newRulesNoIneq[objective_,rules_,equalCons_,precision_]:=FindMinimum[
Evaluate[(objective+1000*Total@((#1-#2)^2&@@@Join@@(Subsequences[List@@#,{2}]&/@equalCons)))],
List@@@rules,
WorkingPrecision->precision+15,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision,
Method->"Newton"
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
newRulesNoIneq1[objective_,rules_,equalCons_,precision_]:=FindMinimum1[
Evaluate[(objective+1000*Total@((#1-#2)^2&@@@Join@@(Subsequences[List@@#,{2}]&/@equalCons)))],
List@@@rules,
WorkingPrecision->precision+15,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision,
Method->"Newton"
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
intRules[objective_,constraints_,rules_,precision_]:=FindMinimum[
Evaluate@{objective,And@@constraints},
List@@@rules,
WorkingPrecision->precision+15,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision,
Method->"InteriorPoint"
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
intRules1[objective_,constraints_,rules_,precision_]:=FindMinimum1[
Evaluate@{objective,And@@constraints},
List@@@rules,
WorkingPrecision->precision+15,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision,
Method->"InteriorPoint"
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
ruleToConstraint[\[Epsilon]_][a_->b_]:=(b-\[Epsilon]<a<b+\[Epsilon])
intRulesConstrained[objective_,constraints_,rules_,precision_]:=FindMinimum[
Evaluate@{objective,And@@(constraints~Join~(ruleToConstraint[SetPrecision[.1,precision+15]]/@rules))},
List@@@rules,
WorkingPrecision->precision+15,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision,
Method->"InteriorPoint"
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
intRulesConstrained1[objective_,constraints_,rules_,precision_]:=FindMinimum1[
Evaluate@{objective,And@@(constraints~Join~(ruleToConstraint[SetPrecision[.1,precision+15]]/@rules))},
List@@@rules,
WorkingPrecision->precision+15,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision,
Method->"InteriorPoint"
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
squareSum[exprs_]:=Total[#^2&/@exprs]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
rootRules[objFuns_,unfixedRules_,fixedVarRules_,precision_]:=Module[{funCount=Length[objFuns],varCount=Length[unfixedRules],floor,ceiling,bigParts,smallParts,funList},
{floor,ceiling}=Through[{Floor,Ceiling}[funCount/varCount]];
{bigParts,smallParts}={Partition[#1,ceiling],Partition[#2,Max[floor,1]]}&@@TakeDrop[objFuns,ceiling*(funCount-floor*varCount)];
funList=Piecewise[{{PadRight[Join@@bigParts,varCount],ceiling==1},{(squareSum/@bigParts)~Join~(Join@@smallParts),floor==1}},squareSum/@(bigParts~Join~smallParts)];
FindRoot[
funList/.fixedVarRules,
List@@@unfixedRules,
WorkingPrecision->precision+10,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision
]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
rootRules1[objFuns_,unfixedRules_,fixedVarRules_,precision_]:=Module[{funCount=Length[objFuns],varCount=Length[unfixedRules],floor,ceiling,bigParts,smallParts,funList},
{floor,ceiling}=Through[{Floor,Ceiling}[funCount/varCount]];
{bigParts,smallParts}={Partition[#1,ceiling],Partition[#2,Max[floor,1]]}&@@TakeDrop[objFuns,ceiling*(funCount-floor*varCount)];
funList=Chop[Piecewise[{{PadRight[Join@@bigParts,varCount],ceiling==1},{(squareSum/@bigParts)~Join~(Join@@smallParts),floor==1}},squareSum/@(bigParts~Join~smallParts)],10^-6];
FindRoot1[
funList/.fixedVarRules,
List@@@unfixedRules,
WorkingPrecision->precision+10,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision
]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
rootRulesNoIneq[objFuns_,rules_,precision_]:=Module[{funCount=Length[objFuns],varCount=Length[rules],floor,ceiling,bigParts,smallParts,funList},
{floor,ceiling}=Through[{Floor,Ceiling}[funCount/varCount]];
{bigParts,smallParts}={Partition[#1,ceiling],Partition[#2,Max[floor,1]]}&@@TakeDrop[objFuns,ceiling*(funCount-floor*varCount)];
funList=Chop[Piecewise[{{PadRight[Join@@bigParts,varCount],ceiling==1},{(squareSum/@bigParts)~Join~(Join@@smallParts),floor==1}},squareSum/@(bigParts~Join~smallParts)],10^-6];
FindRoot[
funList,
List@@@rules,
WorkingPrecision->precision+10,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision
]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
rootRulesNoIneq1[objFuns_,rules_,precision_]:=Module[{funCount=Length[objFuns],varCount=Length[rules],floor,ceiling,bigParts,smallParts,funList},
{floor,ceiling}=Through[{Floor,Ceiling}[funCount/varCount]];
{bigParts,smallParts}={Partition[#1,ceiling],Partition[#2,Max[floor,1]]}&@@TakeDrop[objFuns,ceiling*(funCount-floor*varCount)];
funList=Chop[Piecewise[{{PadRight[Join@@bigParts,varCount],ceiling==1},{(squareSum/@bigParts)~Join~(Join@@smallParts),floor==1}},squareSum/@(bigParts~Join~smallParts)],10^-6];
FindRoot1[
funList,
List@@@rules,
WorkingPrecision->precision+10,
PrecisionGoal->precision+5,
AccuracyGoal->2*precision
]
]


(* ::Subsubsection::RGBColor[0, 1, 1]:: *)
(*Rules and graphics code*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
(*
before objectivePolynomials:
linesNotThroughCircles=Select[allLines,DisjointQ[#,Flatten[circles[[All,1]]]]&];
in objectivePolynomials:
GeoNot[GeoParallel[linesNotThroughCircles[[#,1;;2]]]]&/@Flatten[Cases[Position[linesNotThroughCircles,#][[All,1]]&/@DeleteDuplicates@Flatten@linesNotThroughCircles,(list_/;Length[list]>2):>Subsets[list,{2}]],1]
in constraints:
{(point/@pts[[{1,2}]])=={{0,0},{1,0}}}
in extraCons:
Not[GeoParallelQ[linesNotThroughCircles[[#,1;;2]]]]&/@Flatten[Cases[Position[linesNotThroughCircles,#][[All,1]]&/@DeleteDuplicates@Flatten@linesNotThroughCircles,(list_/;Length[list]>2):>Subsets[list,{2}]],1]

While[value>10^-3\[And]iterationCount\[LessEqual]150,{{value,rules},iterationCount}={vrGenerator[objective,constraints~Join~extraCon,formattedVars],iterationCount+1}]
*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
createRules[quantities_,pts_,cons_,lines_,infiniteLines_,circles_,disks_,angleRules_,nonintersectingSegs_,
polygons_,nonintersectingPolygons_,counterclockwisePolygons_,convexPolygons_,equiangularPolygons_,equilateralPolygons_,regularPolygons_,coordSeeds_,precision_,finalMethod_,analyzeQ_,partiallyAnalyzeQ_]:=

Module[{allLines,interiorPosAssoc,intersectionAssoc,paramVarAssoc,objectivePolynomials,objective,constraints,formattedVars,paramVar,varray,var,
firstVR,value,rules,iterationCount,equalCons,ineqCons,fixedVarRules,unfixedRules,objFuns,extraCons,conVals,failedCons},

allLines=lines~Join~infiniteLines;
interiorPosAssoc=Function[interior,(Map@({0,1}+#&))/@AssociationMap[Position[interior,#]&,DeleteDuplicates@Flatten@interior]]@(Take[#,{2,-2}]&/@allLines);
intersectionAssoc=(Function[pos,(point[allLines[[First@#,1]]]+(paramVar@@#)*(GeoVector@@Through[{First,Last}[allLines[[First@#]]]]))&/@pos]/@interiorPosAssoc);
paramVarAssoc=Mean/@intersectionAssoc;

objectivePolynomials=Flatten[{
GeoDistinctPoints/@Join[{circles[[All,2]]},allLines,polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons],
(GeoNot@*GeoCollinear)/@Join[polygons,nonintersectingPolygons,counterclockwisePolygons],
Function[poly,GeoCos[Angle[#1,#2,#3]->180\[Degree]*(Length[poly]-2)/Length[poly]]&@@@Subsequences[poly~Join~poly[[1;;2]],{3}]]/@(equiangularPolygons~Join~regularPolygons),
Flatten@(Function[depPoint,(paramVarAssoc[depPoint]-#)&/@intersectionAssoc[depPoint]]/@Keys[intersectionAssoc])
}];

objective=Total[#^2&/@objectivePolynomials];

constraints=Join[
cons,
Function[lineIndex,Less@@Join[{0},Table[paramVar[lineIndex,pointIndex],{pointIndex,2,Length[lines[[lineIndex]]]-1}],{1}]]/@Range[Length[lines]],
Flatten@(Thread[point[#]==paramVarAssoc[#]]&/@DeleteDuplicates[Flatten[Take[#,{2,-2}]&/@allLines]]),
Flatten@(Function[{pointsOnCircle,center},(GeoDistSquared[First@pointsOnCircle,center]==GeoDistSquared[#,center])&/@(Rest@pointsOnCircle)]@@@(circles)),
Function[poly,GeoSameLengthQ@Subsequences[Append[poly,First@poly],{2}]]/@(equilateralPolygons~Join~regularPolygons),
GeoConvexQ/@Join[convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons],
GeoCounterclockwiseQ/@counterclockwisePolygons,
GeoOrientedAngleQ/@angleRules,
GeoNonintersectingQ/@(nonintersectingSegs~Join~(Subsequences[Append[#,First@#],{2}]&/@nonintersectingPolygons))
];

formattedVars={
Join@@(splitCoords/@coordSeeds),
Join@@Append[(point/@Complement[pts,coordSeeds[[All,1]]]),quantities],
extractNotVars[objectivePolynomials],
Table[paramVar[i,#]&/@Range[2,(Length@allLines[[i]])-1],{i,Length@allLines}]
};

varray={#,Unique[var]}&/@Flatten[{formattedVars[[1,All,1]],Rest@formattedVars}];
{objective,constraints,formattedVars}={objective,constraints,formattedVars}/.(Rule@@@varray);

firstVR=vrGenerator[objective,constraints,formattedVars];
If[Head[firstVR]===FindMinimum,Throw["unevaluated"]];
{{value,rules},iterationCount}={firstVR,1};
While[value>10^-3\[And]iterationCount<=250,{{value,rules},iterationCount}={vrGenerator[objective,constraints,formattedVars],iterationCount+1}];
If[value>10^-3,Throw["unevaluated"]];

{value,rules}=If[finalMethod==="IPOPT",
{value,rules},
SetAttributes[#,NHoldAll]&/@Join[pts,quantities,{notVar,paramVar}];
{objective,constraints,rules}=SetPrecision[{objective,constraints,rules},precision+15];
SetPrecision[
Switch[finalMethod,
"InteriorPoint",intRules[objective,constraints,rules,precision],
"InteriorPointConstrained",intRulesConstrained[objective,constraints,rules,precision],
_,
equalCons=Select[constraints,Head[#]===Equal&];
If[finalMethod==="NewtonNoIneq",
newRulesNoIneq[objective,rules,equalCons,precision],
ineqCons=Complement[constraints,equalCons];
fixedVarRules=Select[rules,\[Not]FreeQ[ineqCons,First@#]&];
unfixedRules=Complement[rules,fixedVarRules];
{#1,#2~Join~fixedVarRules}&@@
If[finalMethod==="Newton",
newRules[objective,unfixedRules,fixedVarRules,equalCons,precision],
objFuns=SetPrecision[objectivePolynomials,precision+15]~Join~Flatten@(#1-#2&@@@(Join@@(Subsequences[List@@#,{2}]&/@equalCons)));
{0,If[finalMethod==="FindRoot",rootRules[objFuns,unfixedRules,fixedVarRules,precision],rootRulesNoIneq[objFuns,rules,precision]]}
]
]
],
precision]
];

If[analyzeQ\[Or]partiallyAnalyzeQ,

{objective,constraints,formattedVars,equalCons,ineqCons,fixedVarRules,unfixedRules,objFuns,rules}={objective,constraints,formattedVars,equalCons,ineqCons,fixedVarRules,unfixedRules,objFuns,rules}/.(Rule@@@(Reverse/@varray));

extraCons=List@@And@@Flatten[{
constraints,
GeoDistinctPointsQ/@Join[{circles[[All,2]]},allLines,polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons],
(Not@*GeoCollinearQ)/@Join[polygons,nonintersectingPolygons,counterclockwisePolygons],
Function[poly,GeoCosAngleQ[Angle[#1,#2,#3]->180\[Degree]*(Length[poly]-2)/Length[poly]]&@@@Subsequences[poly~Join~poly[[1;;2]],{3}]]/@(equiangularPolygons~Join~regularPolygons),
Flatten@(Function[depPoint,(paramVarAssoc[depPoint]==#)&/@intersectionAssoc[depPoint]]/@Keys[intersectionAssoc])
}];
conVals=extraCons/.rules;

Print["Final Method"];
Print[finalMethod];
If[analyzeQ,
Print["Final Input"];
Switch[finalMethod,
"IPOPT",Print[vrGenerator1[objective,constraints,formattedVars]],
"Newton",Print[newRules1[objective,unfixedRules,fixedVarRules,equalCons,precision]],
"NewtonNoIneq",Print[newRulesNoIneq1[objective,rules,equalCons,precision]],
"InteriorPoint",Print[intRules1[objective,constraints,rules,precision]],
"InteriorPointConstrained",Print[intRulesConstrained1[objective,constraints,rules,precision]],
"FindRoot",Print[rootRules1[objFuns,unfixedRules,fixedVarRules,precision]],
"FindRootNoIneq",Print[rootRulesNoIneq1[objFuns,rules,precision]]
];
Print["Objective Value"];
Print[value];
Print["Rules Returned"];
Print[rules];
];
Print["Failed Constraints (including constraint versions of nonzero addends in the objective function)"];
Print[failedCons=Pick[extraCons,conVals,False]];
Print["Failure Rates: {Head, List of Values, Difference}"];
Print[({Head@#,List@@#,(First@#)-(Last@#)}&/@failedCons)/.rules];

];

rules/.(Rule@@@(Reverse/@varray))

]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
CreateSceneRules[sceneAssoc_Association]:=Module[{isoPts,cons,lines,infiniteLines,circles,disks,angleRules,nonintersectingSegs,polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons,
coordSeeds,pts,quantities,conReplacements},

{isoPts,cons,lines,infiniteLines,circles,disks,angleRules,nonintersectingSegs,polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons,coordSeeds}=
Lookup[sceneAssoc/.l_List?Developer`PackedArrayQ:>Developer`FromPackedArray[l],
{"isolatedPoints","constraints","lines","infiniteLines","circles","disks","angleRules","nonintersectingSegments",
"polygons","nonintersectingPolygons","counterclockwisePolygons","convexPolygons","equiangularPolygons","equilateralPolygons","regularPolygons","coordinateSeeds"},
{}];
pts=DeleteDuplicates@Flatten@{lines,infiniteLines,nonintersectingSegs,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons,polygons,circles,disks[[All,1]],isoPts};
quantities=Lookup[sceneAssoc,"quantities",Complement[Union@Cases[cons,Except[__Symbol?(Context@#==="System`"&), _Symbol],{1, \[Infinity]}],pts]];
conReplacements={
(eq_[OrderlessPatternSequence[pow_[f_,r_Rational],g_]]/;And[eq===Equal,pow===Power,Denominator[r]==2]):>(f^(2r)==g^2\[And]g>=0),
(f_==g_/;\[Not]NumericQ[Denominator[f]]\[Or]\[Not]NumericQ[Denominator[g]]):>(Numerator[f]Denominator[g]==Numerator[g]Denominator[f]),
(eq_[OrderlessPatternSequence[arc_[f_],x_]]/;And[eq===Equal,MemberQ[{ArcCos,ArcSin},arc],NumericQ[x]]):>(f==Switch[arc,ArcCos,Cos,ArcSin,Sin][x]\[And]Numerator[f]^2<=Denominator[f]^2)
};

Normal[AssociationMap[point,pts]]/.
createRules[quantities,pts,cons//.conReplacements,lines,infiniteLines,circles,disks,angleRules,nonintersectingSegs,polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons,coordSeeds,
Lookup[sceneAssoc,"precision",15],Lookup[sceneAssoc,"finalMethod","IPOPT"],Lookup[sceneAssoc,"analyzeQ",False],Lookup[sceneAssoc,"partiallyAnalyzeQ",False]]

]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
SetScene[sceneAssoc_Association]:=Module[{invisPts,invisLines,isoPts,lines,infiniteLines,circles,disks,polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons,
polygonList,visPts,visLines},

{invisPts,invisLines,isoPts,lines,infiniteLines,circles,disks,polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons}=
Lookup[sceneAssoc,
{"invisiblePoints","invisibleLines","isolatedPoints","lines","infiniteLines","circles","disks","polygons","nonintersectingPolygons","counterclockwisePolygons","convexPolygons","equiangularPolygons","equilateralPolygons","regularPolygons"},
{}];
polygonList=Join[polygons,nonintersectingPolygons,counterclockwisePolygons,convexPolygons,equiangularPolygons,equilateralPolygons,regularPolygons];
visPts=Complement[DeleteDuplicates@Flatten@{isoPts,lines,infiniteLines,circles,disks[[All,1]],polygonList},invisPts];
visLines=Complement[lines,invisLines];

{AbsoluteThickness[2],Join[
({Opacity[0],EdgeForm[{GrayLevel[0.7],Thickness[Medium]}],Polygon[#]}&/@polygonList),
{GrayLevel[0.7],Line@#}&/@visLines,
({GrayLevel[0.7],InfiniteLine[(First@#),(Last@#)-(First@#)]}&/@infiniteLines[[All,1;;2]]),
(Function[{pointsOnCircle,center},{GrayLevel[0.7],Circle[center,Mean@(EuclideanDistance[center,#]&/@pointsOnCircle)]}]@@@circles),
({Opacity[0],EdgeForm[{GrayLevel[0.7],Thickness[Medium]}],Disk@@#}&/@disks)
],
Join@@({
Inset[Graphics[{Hue[.58,1,1,1],Disk[]},ImageSize->21],#], 
Inset[Style[ToString@(#/.GeometricPoint[x_]->x),LineColor->RGBColor[0, 0, 1],FrontFaceColor->RGBColor[0, 0, 1],System`BackFaceColor->RGBColor[0, 0, 1],GraphicsColor->RGBColor[0, 0, 1],FontSize->Medium, FontColor->White],#]
}&/@visPts)
}

]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
DrawScene[geoDescription_List,sceneAssoc_Association]:=With[{ss=SetScene[sceneAssoc],csr=CreateSceneRules[sceneAssoc]},
Quiet@Check[Annotation[Graphics[ss/.csr],<|"sceneList"->geoDescription,"sceneAssociation"->sceneAssoc,"primitives"->ss,"coordinates"->csr|>],Throw["unevaluated"]]
]


(* ::Subsection::RGBColor[0, 1, 1]:: *)
(*Translation Code*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
geoBasicConstructHeads={Circle,CircleThrough,GeometricPoint,InfiniteLine,InfiniteLineThrough,Line,LineThrough,Point,Polygon,Triangle};
geoAdvancedConstructHeads={Angle,AngleBisector,Midpoint,PerpendicularBisector,RegionCenter};
geoAdvanced1DRegionConstructHeads={Circumsphere,Insphere,Sphere};
geoAdvanced2DRegionConstructHeads={Ball,Disk};
geoConstructHeads={Angle,AngleBisector,Circle,CircleThrough,Circumsphere,GeometricPoint,InfiniteLine,InfiniteLineThrough,Insphere,Line,LineThrough,Midpoint,PerpendicularBisector,Point,Polygon,RegionCenter,Triangle};
geoConstructModifierHeads={Convex,Cyclic,Equiangular,Equilateral,Regular};
geoQuantityHeads={Area,EuclideanDistance,Inradius,Perimeter,Radius,Unspecified};
geoRelationHeads={Collinear,Concurrent,Congruent,Parallel,Perpendicular,Similar,Tangent};
geoHeadList=Join[geoConstructHeads,geoConstructModifierHeads,geoQuantityHeads,geoRelationHeads];


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
(* quantityQ[obj_]:=If[NumberQ@#,False,(Context@Evaluate@#=!="System`")\[And]FreeQ[geoHeadList,#]]&@If[Depth@obj==1,obj,Head@obj] *)
quantityQ[obj_]:=If[NumberQ@#,False,(Context@Evaluate@#=!="System`")]&@If[Depth@obj==1,obj,Head@obj]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
consecutivePairs[pts_]:=Subsequences[pts,{2}]


(* ::Code::Initialization:: *)
(*
headPositions=Position[headAssoc,Angle];
headPositions=Pick[headPositions,Equal=!=#&/@Extract[headAssoc,Drop[headPositions,None,{-2}]]];
headAssoc=ReplacePart[headAssoc,(Most@#)\[Rule]Unspecified["angle",#]&/@headPositions]~Join~(Extract[headAssoc,Most@#]\[Equal]Unspecified["angle",#]&/@headPositions);

headPositions=Position[headAssoc,PerpendicularBisector];
headArgs=List@@@(Extract[headAssoc,Most@#]&/@headPositions);
headPoints=If[(Head@#)===LineThrough,First@#,#]&/@headArgs;
headAssoc=ReplacePart[headAssoc,MapThread[(Most@#1)\[Rule]InfiniteLineThrough@{Unspecified["pbm",#2],Unspecified["pb",#2]}&,{headPositions,headPoints}]]
~Join~MapThread[PerpendicularBisector@@#1\[Rule]{Unspecified["pbm",#2],Unspecified["pb",#2]}&,{headArgs,headPoints}];

headPositions=Position[headAssoc,AngleBisector];
headArgs=List@@@(Extract[headAssoc,Most@#]&/@headPositions);
headAssoc=ReplacePart[headAssoc,MapThread[(Most@#1)\[Rule](InfiniteLineThrough@{#2[[2]],Unspecified["ab",#2]})&,{headPositions,headArgs}]~Join~((AngleBisector@@#)\[Rule]Unspecified["ab",#]&/@headArgs);

headPositions=Position[headAssoc,Midpoint];
headPositions=Pick[headPositions,Equal=!=#&/@Extract[headAssoc,Drop[headPositions,None,{-2}]]];
headArgs=List@@@(Extract[headAssoc,Most@#]&/@headPositions);
headAssoc=ReplacePart[headAssoc,(Most@#)\[Rule]Unspecified["midpt",#]&/@headPositions]
~Join~MapThread[Unspecified["midpt",#1]\[Equal](Midpoint@@#2)&,{headPositions,headArgs}];
headAssoc=FixedPoint[With[{pos=FirstCase[Position[#,Midpoint],]},
Append[ReplacePart[#,pos\[Rule]Unspecified["midpt",pos]],Unspecified["midpt",pos]\[Equal]Extract[#,pos]]
]&,headAssoc];

headPositions=Position[headAssoc,RegionCenter];
headPositions=Pick[headPositions,Equal=!=#&/@Extract[headAssoc,Drop[headPositions,None,{-2}]]];
headArgs=List@@@(Extract[headAssoc,Most@#]&/@headPositions);
headAssoc=ReplacePart[headAssoc,(Most@#)\[Rule]Unspecified["regionCenter",#]&/@headPositions]
~Join~MapThread[Unspecified["regionCenter",#1]\[Equal]RegionCenter@@#2&,{headPositions,headArgs}];

*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
normalizeConstructs[description_]:=Module[{headPositions,firstPos,construct,constructHead,constructString,placeholder},
headPositions=Position[description,head_?(MemberQ[{Angle,AngleBisector,Insphere,Midpoint,PerpendicularBisector,Radius,RegionCenter},#]&)];
firstPos=FirstCase[headPositions,pos_?(Rule=!=Extract[description,Drop[#,{-2}]]&):>Most@pos,0];
If[firstPos===0,description,
construct=Extract[description,firstPos];
{constructHead,constructString}=Through[{Head,ToString}[construct]];
placeholder=
Switch[constructHead,
AngleBisector,InfiniteLineThrough[{Unspecified[constructString],construct[[2]]}],
Insphere,CircleThrough[{Unspecified[constructString,1],Unspecified[constructString,2],Unspecified[constructString,3]},Unspecified[constructString,0]],
PerpendicularBisector,InfiniteLineThrough[{Unspecified[constructString,1],Unspecified[constructString,2]}],
_,If[Equal===Extract[description,Append[Most@firstPos,0]],
With[{same=Extract[description,Append[Most@firstPos,1+Mod[Last@firstPos,2]]]},If[Or[quantityQ[same],GeometricPoint===Head[same],NumericQ[same]],same,Unspecified[constructString]]],
Unspecified[constructString]]
];
Append[ReplacePart[description,firstPos->placeholder],placeholder->construct]
]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
createHeadAssoc[geoDescription_]:=Module[{description},
description=If[Head[#]===And,Sequence@@#,#]&/@geoDescription;
description=If[Head[#]===Equal,Sequence@@Subsequences[#,{2}],#]&/@description;

description=ReplacePart[description,#->Unspecified["unsp",#]&/@Position[geoDescription,Unspecified]]//.{
Ball[w_]:>Ball[w,1],
Ball[w_,rad_]:>Sequence@@(Disk[#,rad]&/@Flatten[{w}]),
Sphere[w_]:>Sphere[w,1],
Sphere[w_,rad_]:>Sequence@@(Circle[#,rad]&/@Flatten[{w}])
};
description=ReplacePart[description,Join[
(Most@#)->With[{args=Append[List@@Extract[description,Most@#],1][[{1,2}]]},CircleThrough@@{Unspecified@@@{{"ref",#,1},{"ref",#,2},{"ref",#,3}},First@args,If[Head[Last@args]===Unspecified,Nothing,Last@args]}]&/@Position[description,Circle],
With[{newPts=(Unspecified@@@{{"ref",#,1},{"ref",#,2},{"ref",#,3}})~Join~Extract[description,Append[Most@#,1]]},
If[Length@Extract[description,Most@#]==1,(Most@#)->CircleThrough[newPts,Unspecified["ref",#,0]],Append[Most@#,1]->newPts]]&/@Position[description,head_/;MemberQ[{CircleThrough,Circumsphere},head]],
(Most@#)->InfiniteLineThrough[Extract[description,Append[Most@#,2]]]&/@Position[description,InfiniteLine],
Append[Most@#,1]->(Unspecified@@@{{"ref",#,1},{"ref",#,2}})~Join~Extract[description,Append[Most@#,1]]&/@Position[description,InfiniteLineThrough]
]]/.{
Degree->Pi/180,
EuclideanDistance->GeoDist,
(Line[arr_]:>Sequence@@(LineThrough/@If[ListQ@First@arr,Join@@(consecutivePairs/@arr),consecutivePairs@arr])),
((shape_[arr_]/;MemberQ[{Polygon,Triangle},shape]):>Sequence@@(Polygon/@If[ListQ@First@arr,arr,{arr}])),
Disk[w_]:>Disk[w,1]
};

description=FixedPoint[normalizeConstructs,description];

AssociationMap[{}&,{"equal","element","other"}]~Join~GroupBy[description,Switch[Head@#,Equal,"equal",Element,"element",_,"other"]&]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
translateScene[geoDescription_List]:=Module[{headAssoc,eqQuantPos,elQuantPos,quantities,equalityRules,sceneAssoc},

headAssoc=createHeadAssoc[geoDescription];

Cases[headAssoc@"equal",(eq_[OrderlessPatternSequence[GeometricPoint[x_],Unspecified[y__]]]/;(eq===Equal)):>(Unspecified[y]->GeometricPoint[x])];

{eqQuantPos,elQuantPos}=Position[List@@@headAssoc@#,_?quantityQ,2]&/@{"equal","element"};
quantities=Union@@MapThread[Extract,{headAssoc/@{"equal","element"},{eqQuantPos,elQuantPos}}];
quantities=Union@@Extract[headAssoc@"element",elQuantPos];
equalityRules=DeleteDuplicatesBy[MapThread[#1[[#2]]->#1[[1+Mod[#2,2]]]&,{(headAssoc@"equal")[[eqQuantPos[[All,1]]]],eqQuantPos[[All,2]]}],Sort];

sceneAssoc=AssociationMap[{}&,{"invisibleLines","isolatedPoints","constraints","lines","infiniteLines","circles","disks","angleRules","nonintersectingSegments",
"polygons","nonintersectingPolygons","counterclockwisePolygons","convexPolygons","equiangularPolygons","equilateralPolygons","regularPolygons","quantities","midpointTriples"}];
sceneAssoc=DeleteDuplicates/@Fold[extendScene,sceneAssoc,Join[equalityRules[[All,2]],(headAssoc@"equal")[[Complement[Range@Length@headAssoc@"equal",eqQuantPos[[All,1]]]]],headAssoc@"element",headAssoc@"other"]//.equalityRules];

AssociateTo[sceneAssoc,{
"infiniteLines"->FixedPoint[Union@@@Gather[#,Composition[GreaterThan[1],Length,Intersection]]&,sceneAssoc["infiniteLines"]],
"circles"->FixedPoint[Function[circles,{Union@@#[[All,1]],#[[1,2]]}&/@Gather[circles,(Length@(Intersection@@{#1,#2}[[All,1]])>=3)\[And](SameQ@@{#1,#2}[[All,2]])&]],sceneAssoc["circles"]],
"invisiblePoints"->Cases[sceneAssoc,_Unspecified,All],
"quantities"->quantities\[Union]sceneAssoc["quantities"]
}];

sceneAssoc
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
joinAssocVals[sceneAssoc_,pairs_List]:=sceneAssoc~Join~(Association@@(#1->(sceneAssoc[#1]~Join~#2)&@@@pairs))
appendAssocVal[sceneAssoc_,key_,new_]:=joinAssocVals[sceneAssoc,{{key,{new}}}]
mergeAssocs[sceneAssocs_]:=Merge[sceneAssocs,Apply[Union]]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,a_==b_/;(\[Not]MemberQ[{GeometricPoint,Angle},Head@a])\[And]MemberQ[{GeometricPoint,Angle},Head@b]]:=extendScene[sceneAssoc,b==a]
extendScene[sceneAssoc_,GeometricPoint[a_]]:=appendAssocVal[sceneAssoc,"isolatedPoints",GeometricPoint[a]]
extendScene[sceneAssoc_,Point[GeometricPoint[a_]]]:=appendAssocVal[sceneAssoc,"isolatedPoints",GeometricPoint[a]]
extendScene[sceneAssoc_,Point[pts_]]:=joinAssocVals[sceneAssoc,{
{"isolatedPoints",pts}
}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,GeometricPoint[a_]==b_/;MemberQ[{GeometricPoint,Unspecified},Head@b]]:=With[{pts={GeometricPoint[a],b}},joinAssocVals[sceneAssoc,{
{"constraints",{GeoSamePointQ[pts]}},
{"isolatedPoints",pts}
}]]
extendScene[sceneAssoc_,GeometricPoint[a_]=={s_,t_}]:=joinAssocVals[sceneAssoc,{
{"constraints",{(point/@GeometricPoint[a])=={s,t}}},
{"isolatedPoints",{GeometricPoint[a]}},
{"quantities",Select[{s,t},quantityQ]}
}]
extendScene[sceneAssoc_,LineThrough[pts1_]==LineThrough[pts2_]]:=joinAssocVals[sceneAssoc,{
{"lines",{pts1,pts2}},
{"constraints",{(GeoSamePointQ[{First@pts1,First@pts2}]\[And]GeoSamePointQ[{Last@pts1,Last@pts2}])\[Or](GeoSamePointQ[{First@pts1,Last@pts2}]\[And]GeoSamePointQ[{Last@pts1,First@pts2}])}}
}]
extendScene[sceneAssoc_,InfiniteLineThrough[pts1_]==InfiniteLineThrough[pts2_]]:=appendAssocVal[sceneAssoc,"infiniteLines",pts1~Join~pts2]
extendScene[sceneAssoc_,CircleThrough[args1__]==CircleThrough[args2__]]:=With[{args={{args1},{args2}}},mergeAssocs@{
extendScene[sceneAssoc,CircleThrough[Union@@(args[[All,1]]),Sequence@@(Rest@First@MaximalBy[args,Length])]],
joinAssocVals[sceneAssoc,{
{"constraints",{GeoSamePointQ@(args[[All,2]]),If[Min[Length/@args]==3,Equal@@(args[[All,3]]),Nothing]}}
}]
}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,\[Theta]_->Angle[x_,y_,z_]]:=joinAssocVals[sceneAssoc,{
{"angleRules",{Angle[x,y,z]->\[Theta]}},
{"isolatedPoints",{x,y,z}},
{"quantities",{\[Theta]}}
}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,p_->Midpoint[LineThrough_[pts_List]]]:=joinAssocVals[sceneAssoc,{
{"lines",{pts}},
{"constraints",{GeoMidpointQ[First@pts,p,Last@pts]}},
{"isolatedPoints",{p}},
{"midpointTriples",{{First@pts,p,Last@pts}}}
}]
extendScene[sceneAssoc_,p_->Midpoint[y_,z_]]:=joinAssocVals[sceneAssoc,{
{"constraints",{GeoMidpointQ[y,p,z]}},
{"isolatedPoints",{p,y,z}},
{"midpointTriples",{{y,p,z}}}
}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,CircleThrough[{p_,q_,r_},s_]->Insphere[{x_,y_,z_}]]:=mergeAssocs@{
joinAssocVals[sceneAssoc,{
{"polygons",{{x,y,z}}},
{"invisibleLines",{{x,p,y},{x,q,z},{y,r,z}}}
}],
extendScene[sceneAssoc,Tangent[{CircleThrough[{p,q,r},s],LineThrough[{x,p,y}]},p]],
extendScene[sceneAssoc,Tangent[{CircleThrough[{p,q,r},s],LineThrough[{x,q,z}]},q]],
extendScene[sceneAssoc,Tangent[{CircleThrough[{p,q,r},s],LineThrough[{y,r,z}]},r]]
}


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,w_->RegionCenter[Polygon[{x_,y_,z_}],"Orthocenter"]]:=joinAssocVals[sceneAssoc,{
{"polygons",{{x,y,z}}},
{"constraints",{GeoOrthocenterQ[{x,y,z},w]}},
{"isolatedPoints",{w}}
}]
extendScene[sceneAssoc_,w_->RegionCenter[Polygon[{x_,y_,z_}],"Circumcenter"]]:=joinAssocVals[sceneAssoc,{
{"polygons",{{x,y,z}}},
{"circles",{{{x,y,z},w}}}
}]
extendScene[sceneAssoc_,w_->RegionCenter[Polygon[{x_,y_,z_}],"Incenter"]]:=extendScene[sceneAssoc,CircleThrough[Unspecified@@@{{"inc",{{x,y},z}},{"inc",{{x,z},y}},{"inc",{{y,z},x}}},w]->Insphere[{x,y,z}]]
extendScene[sceneAssoc_,w_->RegionCenter[Polygon[{x_,y_,z_}],"Centroid"]]:=With[{cenPts=Unspecified@@@{{"cen",{{x,y},z}},{"cen",{{x,z},y}},{"cen",{{y,z},x}}}},joinAssocVals[sceneAssoc,{
{"polygons",{{x,y,z}}},
{"lines",{{x,w,cenPts[[3]]},{y,w,cenPts[[2]]},{z,w,cenPts[[1]]}}},
{"invisibleLines",{{x,w,cenPts[[3]]},{y,w,cenPts[[2]]},{z,w,cenPts[[1]]}}},
{"constraints",{GeoMidpointQ[x,cenPts[[1]],y],GeoMidpointQ[x,cenPts[[2]],z],GeoMidpointQ[y,cenPts[[3]],z]}}
}]]
extendScene[sceneAssoc_,w_->RegionCenter[Point[pts_List],"Centroid"]]:=joinAssocVals[sceneAssoc,{
{"constraints",{GeoCentroidQ[pts,w]}},
{"isolatedPoints",Append[pts,w]}
}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,InfiniteLineThrough[{p_,y_}]->AngleBisector[x_,y_,z_]]:=appendAssocVal[sceneAssoc,"constraints",GeoAngleSplitQ[{x,p,z},y]]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,InfiniteLineThrough[{p_,q_}]->PerpendicularBisector[x_,y_]]:=joinAssocVals[sceneAssoc,{
{"constraints",{GeoMidpointQ[x,p,y],GeoRightAngleQ[x,p,q],GeoDistSquared[p,q]==1}}
}]
extendScene[sceneAssoc_,InfiniteLineThrough[{p_,q_}]->PerpendicularBisector[LineThrough[pts_]]]:=mergeAssocs@{
extendScene[sceneAssoc,InfiniteLineThrough[{p,q}]->(PerpendicularBisector@@pts[[{1,-1}]])],
appendAssocVal[sceneAssoc,"lines",pts]
}


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,rad_->Radius[CircleThrough[args__]]]:=If[Length@{args}==2,extendScene[sceneAssoc,CircleThrough[args,rad]],
mergeAssocs@{
extendScene[sceneAssoc,CircleThrough[args]],
joinAssocVals[sceneAssoc,{
{"constraints",{rad==(Last@{args})}},
{"quantities",{rad}}
}]
}
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,LineThrough[pts_List]]:=appendAssocVal[sceneAssoc,"lines",pts]
extendScene[sceneAssoc_,z_\[Element]LineThrough[pts_List]]:=joinAssocVals[sceneAssoc,{
{"lines",{If[Length@pts>2,pts,Nothing],{First@pts,z,Last@pts}}}
}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,z_\[Element]head_[args__]/;MemberQ[{InfiniteLineThrough,CircleThrough},head]]:=extendScene[sceneAssoc,head@@Prepend[Rest@{args},Append[First@{args},z]]]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,InfiniteLineThrough[pts_]]:=appendAssocVal[sceneAssoc,"infiniteLines",pts]
extendScene[sceneAssoc_,Collinear[pts__]]:=appendAssocVal[sceneAssoc,"infiniteLines",{pts}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,CircleThrough[pts_,w_]]:=appendAssocVal[sceneAssoc,"circles",{pts,w}]
extendScene[sceneAssoc_,CircleThrough[pts_,w_,rad_]]:=joinAssocVals[sceneAssoc,{
{"circles",{{pts,w}}},
{"constraints",{GeoDistSquared[First@pts,w]==rad^2}},
{"quantities",If[quantityQ@rad,{rad},{}]}
}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,Disk[w_,rad_]]:=joinAssocVals[sceneAssoc,{
{"disks",{{w,rad}}},
{"quantities",If[quantityQ@rad,{rad},{}]}
}]
extendScene[sceneAssoc_,x_\[Element]Disk[w_,rad_]]:=mergeAssocs@{
joinAssocVals[sceneAssoc,{
{"constraints",{GeoDistSquared[x,w]<=rad^2}},
{"isolatedPoints",{x}}
}],
extendScene[sceneAssoc,Disk[w,rad]]
}


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,Tangent[{OrderlessPatternSequence[CircleThrough[args1__],head_[args2__]]},p_]/;MemberQ[{LineThrough,InfiniteLineThrough},head]]:=mergeAssocs@{
extendScene[sceneAssoc,p\[Element]CircleThrough[args1]],
extendScene[sceneAssoc,p\[Element]head[args2]],
appendAssocVal[sceneAssoc,"constraints",GeoRightAngleQ[{args1}[[2]],p,First@Complement[Last@{args2},{p}]]]
}
extendScene[sceneAssoc_,Tangent[{CircleThrough[args1__],CircleThrough[args2__]},p_]]:=mergeAssocs@{
extendScene[sceneAssoc,p\[Element]CircleThrough[args1]],
extendScene[sceneAssoc,p\[Element]CircleThrough[args2]],
appendAssocVal[sceneAssoc,"constraints",GeoCollinearQ[{{args1}[[2]],{args2}[[2]],p}]]
}
extendScene[sceneAssoc_,Tangent[args__]]:=mergeAssocs@(extendScene[sceneAssoc,Tangent[#,Unspecified["tan",#]]]&/@Subsets[{args},{2}])


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,Perpendicular[args__]]:=mergeAssocs@Append[
extendScene[sceneAssoc,#]&/@{args},
joinAssocVals[sceneAssoc,{
{"constraints",GeoPerpendicularQ[#1[[1,{1,2}]],#2[[1,{1,2}]]]&@@@Subsets[{args},{2}]}
}]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,Parallel[args__]]:=mergeAssocs@Append[
extendScene[sceneAssoc,#]&/@{args},
joinAssocVals[sceneAssoc,{
{"constraints",{GeoParallelQ[{args}[[All,1,{1,2}]]]}}
}]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,Concurrent[args__]]:=mergeAssocs@(
extendScene[sceneAssoc,Unspecified["conc",ToString@{args}]\[Element]#]&/@{args}
)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,Polygon[pts_]]:=appendAssocVal[sceneAssoc,"convexPolygons",pts]
extendScene[sceneAssoc_,Convex[Polygon[pts_]]]:=appendAssocVal[sceneAssoc,"convexPolygons",pts]
extendScene[sceneAssoc_,Equiangular[Polygon[pts_]]]:=appendAssocVal[sceneAssoc,"equiangularPolygons",pts]
extendScene[sceneAssoc_,Equilateral[Polygon[pts_]]]:=appendAssocVal[sceneAssoc,"equilateralPolygons",pts]
extendScene[sceneAssoc_,Regular[Polygon[pts_]]]:=appendAssocVal[sceneAssoc,"regularPolygons",pts]
extendScene[sceneAssoc_,Cyclic[Polygon[pts_]]]:=joinAssocVals[sceneAssoc,{
{"convexPolygons",{pts}},
{"circles",{{pts,Unspecified["cir",pts]}}}
}]
extendScene[sceneAssoc_,x_\[Element]Polygon[pts_]]:=joinAssocVals[sceneAssoc,{
{"convexPolygons",{pts}},
{"constraints",GeoConvexQ/@(Append[x]/@Subsequences[Append[pts,First@pts],{2}])},
{"isolatedPoints",{x}}
}]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,EqualAngles[pts_,w_]]:=appendAssocVal[sceneAssoc,"constraints",GeoAngleSplitQ[pts,w]]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,Congruent[args__]/;{LineThrough}===DeleteDuplicates@(Head/@{args})]:=joinAssocVals[sceneAssoc,{
{"lines",Sequence@@@{args}},
{"constraints",{GeoSameLengthQ@({args}[[All,1,{1,-1}]])}}
}]
extendScene[sceneAssoc_,Congruent[args__]/;{CircleThrough}===DeleteDuplicates@(Head/@{args})]:=mergeAssocs@Append[
extendScene[sceneAssoc,#]&/@{args},
appendAssocVal[sceneAssoc,"constraints",GeoSameLengthQ@({First@#1,#2}&@@@{args})]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
extendScene[sceneAssoc_,x_]:=appendAssocVal[sceneAssoc,"constraints",x]


(* ::Subsection::RGBColor[0, 1, 1]::Closed:: *)
(*Public functions code*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
processScene[geoDescription_,emptyListResult_,callFunction_]:=
Quiet@If[geoDescription==={},
emptyListResult,
Catch@Check[callFunction@translateScene@geoDescription,"unevaluated"]
]


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
GeometricSceneToGraphicsPrimitives[geoDescription_List]:=With[{res=processScene[geoDescription,{},SetScene]},res/;res=!="unevaluated"]
FindGeometricCoordinates[geoDescription_List]:=With[{res=processScene[geoDescription,{},CreateSceneRules]},res/;res=!="unevaluated"]
FindGeometricSceneGraphics[geoDescription_List]:=With[{res=processScene[geoDescription,Annotation[Graphics[],<|"sceneList"->{}"sceneAssociation"-><||>,"primitives"->{},"coordinates"->{}|>],DrawScene[geoDescription,#]&]},res/;res=!="unevaluated"]


(* ::Section::RGBColor[0, 1, 1]:: *)
(*End context change*)


(* ::Code::Initialization::RGBColor[0, 1, 1]:: *)
End[]

System`Private`RestoreContextPath[];

{
	System`AffineHull,
	System`Angle,
	System`AngleBisector,
	System`CircleThrough,
	System`Collinear,
	System`Coplanar,
	System`Concurrent,
	System`Convex,
	System`Cyclic,
	System`EqualAngles,
	System`Equiangular,
	System`Equilateral,
	System`GeometricPoint,
	System`GeometricScene,
	System`InfiniteLineThrough,
	System`Inradius,
	System`LineThrough,
	System`Midpoint,
	System`Parallel,
	System`PerpendicularBisector,
	System`Radius,
	System`RegionCenter,
	System`Regular,
	System`Similar,
	System`Tangent,
	System`Unspecified,
	System`FindGeometricSceneGraphics,
	System`GeometricSceneToGraphicsPrimitives,
	System`FindGeometricCoordinates
}



