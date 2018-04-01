(* ::Package:: *)

(* Wolfram Language package *)


System`Private`NewContextPath[{"System`", "GeometrySceneDrawer`Common`"}];



Begin["GeometrySceneDrawer`"]

EntityFramework`$SceneExample := System`EntityStore["GeometryTheorem" -> <|

"Label" -> "geometry theorem",

"Entities" -> <|

"PythagoreanTheorem" -> <|
	"Label" ->
		"Pythagorean theorem",
	"Statement" ->
		"For a triangle \[EmptyUpTriangle]ABC, if \[Angle]BAC is a right angle, then BC^2 = AB^2 + AC^2.",
	"Hypotheses" -> {
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		Angle[GeometricPoint["B"], GeometricPoint["A"], GeometricPoint["C"]] == 90\[Degree]
	},
	"Conclusions" -> {
		EuclideanDistance[GeometricPoint["B"], GeometricPoint["C"]]^2 == EuclideanDistance[GeometricPoint["A"], GeometricPoint["B"]]^2 + EuclideanDistance[GeometricPoint["A"], GeometricPoint["C"]]^2
	},
	"EponymousPeople" -> {
		"Pythagoras::ksw9v"
	},
	"Formulators" -> {
		"Pythagoras::ksw9v"
	},
	"Provers" -> {
		"Pythagoras::ksw9v"
	},
	"FormulationSources" -> {
		"FamousMathProblem"
	},
	"ProofSources" -> {
		"FamousMathProblem"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/PythagoreanTheoremFigure_1000.gif"
|>,

"PythagoreanTheoremConverse" -> <|
	"Label" ->
		"Pythagorean theorem converse",
	"Statement" ->
		"For a triangle \[EmptyUpTriangle]ABC, if BC^2 = AB^2 + AC^2, then \[Angle]BAC is a right angle.",
	"Hypotheses" -> {
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		EuclideanDistance[GeometricPoint["B"], GeometricPoint["C"]]^2 == EuclideanDistance[GeometricPoint["A"], GeometricPoint["B"]]^2 + EuclideanDistance[GeometricPoint["A"], GeometricPoint["C"]]^2
	},
	"Conclusions" -> {
		Angle[GeometricPoint["B"], GeometricPoint["A"], GeometricPoint["C"]] == 90\[Degree]
	},
	"EponymousPeople" -> {
		"Pythagoras::ksw9v"
	},
	"Formulators" -> {
		"Pythagoras::ksw9v"
	},
	"Provers" -> {
		"Pythagoras::ksw9v"
	},
	"FormulationSources" -> {
		"FamousMathProblem"
	},
	"ProofSources" -> {
		"FamousMathProblem"
	},
	"MathWorld" ->
		"PythagoreanTheorem",
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/PythagoreanTheoremFigure_1000.gif"
|>,

"ThalesTheorem" -> <|
	"Label" ->
		"Thales' theorem",
	"Statement" ->
		"If A, B and C are points on a circle where the line [AC] is a diameter of the circle, then the angle \[Angle]ABC is a right angle.",
	"Hypotheses" -> {
		CircleThrough[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}, GeometricPoint["O"]],
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		GeometricPoint["O"] == Midpoint[Line[{GeometricPoint["A"], GeometricPoint["C"]}]]
	},
	"Conclusions" -> {
		Angle[GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]] == 90\[Degree]
	},
	"EponymousPeople" -> {
		"Thales::535t4"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/ThalesTheorem_1000.gif"
|>,

"ArchimedesMidpointTheorem" -> <|
	"Label" ->
		"Archimedes' midpoint theorem",
	"Statement" ->
		"Let M be the midpoint of the arc AMB. Pick C at random and pick D such that (MD) \[UpTee] (AC). Then |AD| = |DC| + |BC|.",
	"Hypotheses" -> {
		CircleThrough[GeometricPoint/@{"A","B","C","M"}] ,
		EuclideanDistance[GeometricPoint["A"], GeometricPoint["M"]] == EuclideanDistance[GeometricPoint["B"], GeometricPoint["M"]],
		LineThrough[GeometricPoint/@{"B","C"}],
		LineThrough[GeometricPoint/@{"M","D"}] \[Perpendicular] LineThrough[GeometricPoint/@{"A","D","C"}]
	},
	"Conclusions" -> {
		EuclideanDistance[GeometricPoint["A"], GeometricPoint["D"]] == EuclideanDistance[GeometricPoint["B"], GeometricPoint["C"]] + EuclideanDistance[GeometricPoint["D"], GeometricPoint["C"]]
	},
	"EponymousPeople" -> {
		"Archimedes::29f2h"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/ArchimedesMidpointTheorem_1000.gif"
|>,

"DesarguesTheorem" -> <|
	"Label" ->
		"Desargues' theorem",
	"Statement" ->
		"If the three straight lines joining the corresponding vertices of two triangles ABC and A'B'C' all meet in a point (the perspector), then the three intersections of pairs of corresponding sides lie on a straight line (the perspectrix). Equivalently, if two triangles are perspective from a point, they are perspective from a line.",
	"AlternateStatements" -> {
		"Two triangles are in perspective axially if and only if they are in perspective centrally."
	},
	"Hypotheses" -> {
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		Triangle[{GeometricPoint["A'"], GeometricPoint["B'"], GeometricPoint["C'"]}],
		InfiniteLineThrough[{GeometricPoint["O"], GeometricPoint["A"], GeometricPoint["A'"]}],
		InfiniteLineThrough[{GeometricPoint["O"], GeometricPoint["B"], GeometricPoint["B'"]}],
		InfiniteLineThrough[{GeometricPoint["O"], GeometricPoint["C"], GeometricPoint["C'"]}],
		InfiniteLineThrough[{GeometricPoint["R"], GeometricPoint["A"], GeometricPoint["B"]}],
		InfiniteLineThrough[{GeometricPoint["R"], GeometricPoint["A'"], GeometricPoint["B'"]}],
		InfiniteLineThrough[{GeometricPoint["S"], GeometricPoint["A"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["S"], GeometricPoint["A'"], GeometricPoint["C'"]}],
		InfiniteLineThrough[{GeometricPoint["T"], GeometricPoint["B"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["T"], GeometricPoint["B'"], GeometricPoint["C'"]}]
	},
	"Conclusions" -> {
		Collinear[GeometricPoint["R"], GeometricPoint["S"], GeometricPoint["T"]]
	},
	"EponymousPeople" -> {
		"GirardDesargues::y296j"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/DesarguesTheorem_700.gif"
|>,

"ButterflyTheorem" -> <|
	"Label" ->
		"butterfly theorem",
	"Statement" ->
		"Let M be the midpoint of a chord PQ of a circle, through which two other chords AB and CD are drawn; (AD) and (BC) intersect chord PQ at X and Y correspondingly. Then M is the midpoint of [XY].",
	"Hypotheses" -> {
		CircleThrough[GeometricPoint/@{"A","P","D","B","Q","C"}],
		GeometricPoint["M"] == Midpoint[Line[GeometricPoint/@{"P","Q"}]],
		LineThrough[GeometricPoint/@{"A","M","B"}],
		LineThrough[GeometricPoint/@{"C","M","D"}],
		LineThrough[GeometricPoint/@{"P","X","Q"}],
		LineThrough[GeometricPoint/@{"A","X","D"}],
		LineThrough[GeometricPoint/@{"P","Y","Q"}],
		LineThrough[GeometricPoint/@{"B","Y","C"}]
	},
	"Conclusions" -> {
		GeometricPoint["M"] == Midpoint[Line[{GeometricPoint["X"], GeometricPoint["Y"]}]]
	},
	"Formulators" -> {
		"WilliamWallace::yj78q",
		"SirFrederickWilliamHerschel::s72gs"
	},
	"FormulationDates" -> {
		{1803},
		{1805,4,7}
	},
	"ProofDates" -> {
		{1804}
	},
	"FormulationSources" -> {
		"http://www.cut-the-knot.org/pythagoras/WallaceButterfly.shtml"
	},
	"ProofSources" -> {
		"http://www.cut-the-knot.org/pythagoras/WallaceButterfly.shtml"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/ButterflyTheorem_900.gif"
|>,

"CevasTheorem" -> <|
	"Label" ->
		"Ceva's theorem",
	"Statement" ->
		"Given a triangle with polygon vertices A, B, and C and points along the sides D, E, and F, a sufficient condition for the cevians [AD], [BE], and [CF] to be concurrent (intersect in a single point) is that |BD| x |CE| x |AF| = |DC| x |EA| x |FB|.",
	"Hypotheses" -> {
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["F"], GeometricPoint["B"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["E"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["B"], GeometricPoint["D"], GeometricPoint["C"]}],
		(EuclideanDistance[GeometricPoint["A"], GeometricPoint["F"]]*EuclideanDistance[GeometricPoint["B"], GeometricPoint["D"]]*EuclideanDistance[GeometricPoint["C"], GeometricPoint["E"]])/(EuclideanDistance[GeometricPoint["D"], GeometricPoint["C"]]*EuclideanDistance[GeometricPoint["E"], GeometricPoint["A"]]*EuclideanDistance[GeometricPoint["F"], GeometricPoint["B"]]) == 1
	},
	"Conclusions" -> {
		Concurrent[
			InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["D"]}],
			InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["E"]}],
			InfiniteLineThrough[{GeometricPoint["C"], GeometricPoint["F"]}]
		]
	},
	"EponymousPeople" -> {
		"GiovanniCeva::cf96n"
	},
	"Provers" -> {
		"YusufAl-MutamanIbnHud::k6tc5",
		"GiovanniCeva::cf96n"
	},
	"ProofDates" -> {
		DateObject[{1000},"Century"],
		{1678}
	},
	"Formulators" -> {
		"YusufAl-MutamanIbnHud::k6tc5",
		"GiovanniCeva::cf96n"
	},
	"FormulationDates" -> {
		DateObject[{1000},"Century"],
		{1678}
	},
	"FormulationSources" -> {
		"http://mathworld.wolfram.com/CevasTheorem.html",
		"https://en.wikipedia.org/wiki/Ceva's_theorem",
		"https://en.wikipedia.org/wiki/Yusuf_al-Mu'taman_ibn _Hud",
		"Giovanni Ceva, \" De lineis rectis \" 1678",
		"Yusuf al-Mu'taman ibn Hud, \" Kitab al-Istikm\[ABar]l \""
	},
	"ProofSources" -> {
		"http://mathworld.wolfram.com/CevasTheorem.html",
		"https://en.wikipedia.org/wiki/Ceva's_theorem",
		"https://en.wikipedia.org/wiki/Yusuf_al-Mu'taman_ibn _Hud",
		"Giovanni Ceva, \" De lineis rectis \" 1678",
		"Yusuf al-Mu'taman ibn Hud, \" Kitab al-Istikm\[ABar]l \""
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/CevasTheorem_1000.gif"
|>,

"CevasTheoremConverse" -> <|
	"Label" ->
		"Ceva's theorem converse",
	"Statement" ->
		"Given a triangle with polygon vertices A, B, and C and points along the sides D, E, and F, a necessary condition for the cevians [AD], [BE], and [CF] to be concurrent (intersect in a single point) is that |BD| x |CE| x |AF| = |DC| x |EA| x |FB|.",
	"Hypotheses" -> {
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["F"], GeometricPoint["B"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["E"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["B"], GeometricPoint["D"], GeometricPoint["C"]}],
		Concurrent[
			InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["D"]}],
			InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["E"]}],
			InfiniteLineThrough[{GeometricPoint["C"], GeometricPoint["F"]}]
		]
	},
	"Conclusions" -> {
		(EuclideanDistance[GeometricPoint["A"], GeometricPoint["F"]]*EuclideanDistance[GeometricPoint["B"], GeometricPoint["D"]]*EuclideanDistance[GeometricPoint["C"], GeometricPoint["E"]])/(EuclideanDistance[GeometricPoint["D"], GeometricPoint["C"]]*EuclideanDistance[GeometricPoint["E"], GeometricPoint["A"]]*EuclideanDistance[GeometricPoint["F"], GeometricPoint["B"]]) == 1
	},
	"EponymousPeople" -> {
		"GiovanniCeva::cf96n"
	},
	"Provers" -> {
		"YusufAl-MutamanIbnHud::k6tc5",
		"GiovanniCeva::cf96n"
	},
	"ProofDates" -> {
		DateObject[{1000},"Century"],
		{1678}
	},
	"Formulators" -> {
		"YusufAl-MutamanIbnHud::k6tc5",
		"GiovanniCeva::cf96n"
	},
	"FormulationDates" -> {
		DateObject[{1000},"Century"],
		{1678}
	},
	"FormulationSources" -> {
		"http://mathworld.wolfram.com/CevasTheorem.html",
		"https://en.wikipedia.org/wiki/Ceva's_theorem",
		"https://en.wikipedia.org/wiki/Yusuf_al-Mu'taman_ibn _Hud",
		"Giovanni Ceva, \" De lineis rectis \" 1678",
		"Yusuf al-Mu'taman ibn Hud, \" Kitab al-Istikm\[ABar]l \""
	},
	"ProofSources" -> {
		"http://mathworld.wolfram.com/CevasTheorem.html",
		"https://en.wikipedia.org/wiki/Ceva's_theorem",
		"https://en.wikipedia.org/wiki/Yusuf_al-Mu'taman_ibn _Hud",
		"Giovanni Ceva, \" De lineis rectis \" 1678",
		"Yusuf al-Mu'taman ibn Hud, \" Kitab al-Istikm\[ABar]l \""
	},
	"MathWorld" ->
		"CevasTheorem",
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/CevasTheorem_1000.gif"
|>,

"CrossedLaddersTheorem" -> <|
	"Label" ->
		"crossed ladders theorem",
	"Statement" ->
		"Let (AB) and (CD) be two parallel lines. Let E be the intersection of (AD) and (BC) and let F be such that (EF) is parallel to (AB). Then: 1 / |AB| + 1 / |CD| = 1 / |EF|.",
	"Hypotheses" -> {
		Parallel[
			InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["B"]}],
			InfiniteLineThrough[{GeometricPoint["C"], GeometricPoint["D"]}],
			InfiniteLineThrough[{GeometricPoint["F"], GeometricPoint["E"]}]
		],
		InfiniteLineThrough[{GeometricPoint["E"], GeometricPoint["A"], GeometricPoint["D"]}],
		InfiniteLineThrough[{GeometricPoint["E"], GeometricPoint["B"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["F"], GeometricPoint["C"]}]
		},
	"Conclusions" -> {
		1/EuclideanDistance[GeometricPoint["A"], GeometricPoint["B"]] + 1/EuclideanDistance[GeometricPoint["C"], GeometricPoint["D"]] == 1/EuclideanDistance[GeometricPoint["E"], GeometricPoint["F"]]
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/CrossedLaddersTheorem_1000.gif"
|>,

"StengelsCrossedLaddersTheorem" -> <|
	"Label" ->
		"Stengel's crossed ladders theorem",
	"Statement" ->
		"Let A, B and C be three arbitrary points. Let E be in the segment [AB] and D in the segment [BC]. Let F be the intersection of (EC) and (AD). Place points G, H, I and J on the line (AC) such that (EI), (DH), (FJ) and (BG) are all parallel to each other. Then 1 / |EI| + 1 / |DH| = 1 / |FJ| + 1 / |BG|.",
	"Hypotheses" -> {
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["E"], GeometricPoint["B"]}],
		LineThrough[{GeometricPoint["B"], GeometricPoint["D"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["F"], GeometricPoint["D"]}],
		LineThrough[{GeometricPoint["E"], GeometricPoint["F"], GeometricPoint["C"]}],
		Collinear[
			GeometricPoint["A"],
			GeometricPoint["I"],
			GeometricPoint["G"],
			GeometricPoint["J"],
			GeometricPoint["H"],
			GeometricPoint["C"]
		],
		Parallel[
			InfiniteLineThrough[{GeometricPoint["E"], GeometricPoint["I"]}],
			InfiniteLineThrough[{GeometricPoint["D"], GeometricPoint["H"]}],
			InfiniteLineThrough[{GeometricPoint["F"], GeometricPoint["J"]}],
			InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["G"]}]
		]
	},
	"Conclusions" -> {
		1/EuclideanDistance[GeometricPoint["D"], GeometricPoint["H"]] + 1/EuclideanDistance[GeometricPoint["E"], GeometricPoint["I"]] == 1/EuclideanDistance[GeometricPoint["B"], GeometricPoint["G"]] + 1/EuclideanDistance[GeometricPoint["F"], GeometricPoint["J"]]
	},
	"MathWorld" ->
		"CrossedLaddersTheorem",
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/CrossedLaddersTheoremExt_1000.gif"
|>,

"PtolemysTheorem" -> <|
	"Label" ->
		"Ptolemy's theorem",
	"Statement" ->
		"For a cyclic quadrilateral ABCD, the sum of the products of the two pairs of opposite sides equals the product of the diagonals: |AB| x |CD| + |BC| x |DA| = |AC| x |BD|",
	"Hypotheses" -> {
		Cyclic[Polygon[{GeometricPoint["A"], GeometricPoint["D"], GeometricPoint["C"], GeometricPoint["B"]}]]
	},
	"Conclusions" -> {
		EuclideanDistance[GeometricPoint["A"], GeometricPoint["B"]]*EuclideanDistance[GeometricPoint["C"], GeometricPoint["D"]] + EuclideanDistance[GeometricPoint["B"], GeometricPoint["C"]]*EuclideanDistance[GeometricPoint["D"], GeometricPoint["A"]] == EuclideanDistance[GeometricPoint["A"], GeometricPoint["C"]]*EuclideanDistance[GeometricPoint["B"], GeometricPoint["D"]]
	},
	"EponymousPeople" -> {
		"Ptolemy::y9827"
	},
	"Formulators" -> {
		"Ptolemy::y9827"
	},
	"Provers" -> {
		"Ptolemy::y9827"
	},
	"FormulationDates" -> {
		DateObject[{150},"Century"]
	},
	"ProofDates" -> {
		DateObject[{150},"Century"]
	},
	"FormulationSources" -> {
		"http://www.cut-the-knot.org/proofs/ptolemy.shtml",
		"https://en.wikipedia.org/wiki/Almagest"
	},
	"ProofSources" -> {
		"http://www.cut-the-knot.org/proofs/ptolemy.shtml",
		"https://en.wikipedia.org/wiki/Almagest"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/PtolemysTheorem_1000.gif"
|>,

(*"PtolemyInequality" -> <|
	"Label" ->
		"Ptolemy inequality",
	"Statement" ->
		"For a quadrilateral ABCD which is not cyclic, Ptolemy's theorem becomes an inequality: |AB| * |CD| + |BC| * |DA| > |AC| * |BD|.",
	"Hypotheses" -> {
		Polygon[{GeometricPoint["A"], GeometricPoint["D"], GeometricPoint["C"], GeometricPoint["B"]}],
		GeometricPoint["C"] \[NotElement] CircleThrough[{GeometricPoint["A"], GeometricPoint["D"], GeometricPoint["B"]}]
	},
	"Conclusions" -> {
		EuclideanDistance[GeometricPoint["A"], GeometricPoint["B"]]*EuclideanDistance[GeometricPoint["C"], GeometricPoint["D"]] + EuclideanDistance[GeometricPoint["B"], GeometricPoint["C"]]*EuclideanDistance[GeometricPoint["D"], GeometricPoint["A"]] > EuclideanDistance[GeometricPoint["A"], GeometricPoint["C"]]*EuclideanDistance[GeometricPoint["B"], GeometricPoint["D"]]
	},
	"EponymousPeople" -> {
		"Ptolemy::y9827"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/PtolemysInequality_1000.gif"
|>,*)

"Finsler-HadwigerTheorem" -> <|
	"Label" ->
		"Finsler-Hadwiger theorem",
	"Statement" ->
		"Let the squares \[Square] ABCD and square \[Square] AB'C'D' share a common polygon vertex A. The midpoints Q and S of the segmentsB'D and BD' together with the centers of the original squares R and T then form another square \[Square] QRST.",
	"Hypotheses" -> {
		Regular[Polygon[GeometricPoint/@{"D","C","B","A"}]],
		Regular[Polygon[GeometricPoint/@{"D'","C'","B'","A"}]],
		Polygon[GeometricPoint/@{"Q","R","S","T"}],
		GeometricPoint["Q"]==Midpoint[LineThrough[GeometricPoint/@{"B'","D"}]],
		GeometricPoint["S"]==Midpoint[LineThrough[GeometricPoint/@{"B","D'"}]],
		GeometricPoint["R"]==Midpoint[LineThrough[GeometricPoint/@{"A","C"}]],
		GeometricPoint["T"]==Midpoint[LineThrough[GeometricPoint/@{"A","C'"}]]
	},
	"Conclusions" -> {
		Regular[Polygon[{GeometricPoint["R"], GeometricPoint["S"], GeometricPoint["T"], GeometricPoint["Q"]}]]
	},
	"EponymousPeople" -> {
		"HugoHadwiger::864x9",
		"PaulFinsler::34ppj"
	},
	"Formulators" -> {
		"HugoHadwiger::864x9",
		"PaulFinsler::34ppj"
	},
	"Provers" -> {
		"HugoHadwiger::864x9",
		"PaulFinsler::34ppj"
	},
	"FormulationDates" -> {
		{1938,6,22}
	},
	"ProofDates" -> {
		{1938,6,22}
	},
	"FormulationSources" -> {
		"Finsler, P.; Hadwiger, H. (1937), \" Einige Relationen im Dreieck \", Commentarii Mathematici Helvetici (in German), 10 (1): 316\[Dash]326, doi:10.1007/BF01214300, MR 1509584. See in particular p. 324."
	},
	"ProofSources" -> {
		"Finsler, P.; Hadwiger, H. (1937), \" Einige Relationen im Dreieck \", Commentarii Mathematici Helvetici (in German), 10 (1): 316\[Dash]326, doi:10.1007/BF01214300, MR 1509584. See in particular p. 324."
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/FinslerHadwingerTheorem_1000.gif"
|>,

"KosnitaTheorem" -> <|
	"Label" ->
		"Kosnita theorem",
	"Statement" ->
		"The lines joining the vertices A, B, and C of a given triangle ABC with the circumcenters of the triangles BCO, CAO, and ABO (where O is the circumcenter of ABC), respectively, are concurrent at point K. The point K of concurrence is known as the Kosnita point.",
	"Hypotheses" -> {
		GeometricPoint["O"]==RegionCenter[Triangle[GeometricPoint/@{"A","B","C"}],"Circumcenter"],
		GeometricPoint["Obco"]==RegionCenter[Triangle[GeometricPoint/@{"O","B","C"}],"Circumcenter"],
		GeometricPoint["Ocao"]==RegionCenter[Triangle[GeometricPoint/@{"A","O","C"}],"Circumcenter"],
		GeometricPoint["Oabo"]==RegionCenter[Triangle[GeometricPoint/@{"A","B","O"}],"Circumcenter"],
		InfiniteLineThrough[GeometricPoint/@{"A","Obco"}],
		InfiniteLineThrough[GeometricPoint/@{"B","Ocao"}],
		InfiniteLineThrough[GeometricPoint/@{"C","Oabo"}]
	},
	"Conclusions" -> {
		Concurrent[
			Line[{GeometricPoint["A"], GeometricPoint["Obco"]}],
			Line[{GeometricPoint["B"], GeometricPoint["Ocao"]}],
			Line[{GeometricPoint["C"], GeometricPoint["Oabo"]}]
		]
	},
	"EponymousPeople" -> {
		"CezarCosnita::24f59"
	},
	"Formulators" -> {
		"CezarCosnita::24f59"
	},
	"Provers" -> {
		"CezarCosnita::24f59"
	},
	"FormulationDates" -> {
		{1941}
	},
	"ProofDates" -> {
		{1941}
	},
	"FormulationSources" -> {
		"http://recreatiimatematice.ro/arhiva/processed/22010/14_ 22010_RM22010.pdf",
		" C. Co\:015fni\:0163\[ACup] - \" Coordonn\[EAcute]es barycentriques \", Bucarest, Paris, Librairie Vuibert, 1941"
	},
	"ProofSources" -> {
		"http://recreatiimatematice.ro/arhiva/processed/22010/14_ 22010_RM22010.pdf",
		" C. Co\:015fni\:0163\[ACup] - \" Coordonn\[EAcute]es barycentriques \", Bucarest, Paris, Librairie Vuibert, 1941"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/KosnitaTheorem_1000.gif"
|>,

"Droz-FarnyTheorem" -> <|
	"Label" ->
		"Droz-Farny theorem",
	"Statement" ->
		"If two perpendicular lines are drawn through the orthocenter H of any triangle A1A2A3, these lines intercept each side (or its extension) in two points (labeled P(12), P'(12), P(13), P'(13), P(23), P'(23)). Then the midpoints M(12), M(13), and M(23) of these three segments are collinear.",
	"Hypotheses" -> {
		GeometricPoint["H"] == RegionCenter[Triangle[{GeometricPoint["A1"], GeometricPoint["A2"], GeometricPoint["A3"]}],"Orthocenter"],
		InfiniteLineThrough[{GeometricPoint["P12"], GeometricPoint["A1"], GeometricPoint["A2"]}],
		InfiniteLineThrough[{GeometricPoint["P'12"], GeometricPoint["A1"], GeometricPoint["A2"]}],
		InfiniteLineThrough[{GeometricPoint["P23"], GeometricPoint["A2"], GeometricPoint["A3"]}],
		InfiniteLineThrough[{GeometricPoint["P'23"], GeometricPoint["A2"], GeometricPoint["A3"]}],
		InfiniteLineThrough[{GeometricPoint["P13"], GeometricPoint["A1"], GeometricPoint["A3"]}],
		InfiniteLineThrough[{GeometricPoint["P'13"], GeometricPoint["A1"], GeometricPoint["A3"]}],
		Collinear[GeometricPoint["P12"], GeometricPoint["P23"], GeometricPoint["P13"], GeometricPoint["H"]],
		Collinear[GeometricPoint["P'12"], GeometricPoint["P'23"], GeometricPoint["P'13"], GeometricPoint["H"]],
		InfiniteLineThrough[{GeometricPoint["P12"], GeometricPoint["H"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P'12"], GeometricPoint["H"]}],
		GeometricPoint["M12"] == Midpoint[Line[{GeometricPoint["P12"], GeometricPoint["P'12"]}]],
		GeometricPoint["M23"] == Midpoint[Line[{GeometricPoint["P23"], GeometricPoint["P'23"]}]],
		GeometricPoint["M13"] == Midpoint[Line[{GeometricPoint["P13"], GeometricPoint["P'13"]}]]
	},
	"Conclusions" -> {
		Collinear[GeometricPoint["M12"], GeometricPoint["M23"], GeometricPoint["M13"]]
	},
	"EponymousPeople" -> {
		"ArnoldDroz-Farny::7345p"
	},
	"Formulators" -> {
		"ArnoldDroz-Farny::7345p"
	},
	"Provers" -> {
		"ReneGoormaghtigh::94jxb"
	},
	"FormulationDates" -> {
		{1899}
	},
	"ProofDates" -> {
		{1930}
	},
	"FormulationSources" -> {
		"https://en.wikipedia.org/wiki/Droz-Farny_line _theorem",
		"A. Droz-Farny (1899), \" Question 14111\". The Educational Times, volume 71, pages 89-90"
	},
	"ProofSources" -> {
		"https://en.wikipedia.org/wiki/Droz-Farny_line _theorem",
		"Ren\[EAcute] Goormaghtigh (1930), \" Sur une g\[EAcute]n\[EAcute]ralisation du th\[EAcute]oreme de Noyer, Droz-Farny et Neuberg \". Mathesis, volume 44, page 25"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/DrozFarnyTheorem_1000.gif"
|>,

"JohnsonsTheorem" -> <|
	"Label" ->
		"Johnson's theorem",
	"Statement" ->
		"Let three equal circles with centers J_a, J_b, and J_c intersect in a single point H and intersect pairwise in the points A, B, and C. The radius of the circumcircle O of the triangle ABC is equal to the radius of the original three circles.",
	"Hypotheses" -> {
		CircleThrough[GeometricPoint/@{"B","C","H"},GeometricPoint["J_a"]] == C1,
		CircleThrough[GeometricPoint/@{"A","C","H"},GeometricPoint["J_b"]] == C2,
		CircleThrough[GeometricPoint/@{"A","B","H"},GeometricPoint["J_c"]] == C3,
		Congruent[C1,C2,C3],
		Polygon[GeometricPoint/@{"A","B","C"}]
	},
	"Conclusions" -> {
		Congruent[
			CircleThrough[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
			CircleThrough[{GeometricPoint["H"], GeometricPoint["B"], GeometricPoint["C"]}, GeometricPoint["J_a"]]
		]
	},
	"EponymousPeople" -> {
		"RogerAJohnson::v9756"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/JohnsonsTheorem_1000.gif"
|>,


"vanAubelsTheorem" -> <|
	"Label" ->
		"van Aubel's theorem",
	"Statement" ->
		"Given an arbitrary planar quadrilateral, place a square outwardly on each side, and connect the centers of opposite squares. Then van Aubel's theorem states that the two lines are of equal length and cross at a right angle.",
	"Hypotheses" -> {
		Polygon[GeometricPoint/@{"D","C","B","A"}],
		Regular[Polygon[GeometricPoint/@{"A","B","Pba","Pab"}]],
		Regular[Polygon[GeometricPoint/@{"B","C","Pcb","Pbc"}]],
		Regular[Polygon[GeometricPoint/@{"C","D","Pdc","Pcd"}]],
		Regular[Polygon[GeometricPoint/@{"D","A","Pad","Pda"}]],
		GeometricPoint["Oab"]==Midpoint[Line[GeometricPoint/@{"A","Pba"}]],
		GeometricPoint["Obc"]==Midpoint[Line[GeometricPoint/@{"B","Pcb"}]],
		GeometricPoint["Ocd"]==Midpoint[Line[GeometricPoint/@{"C","Pdc"}]],
		GeometricPoint["Oad"]==Midpoint[Line[GeometricPoint/@{"D","Pad"}]],
		LineThrough[GeometricPoint/@{"Oab","Ocd"}],
		LineThrough[GeometricPoint/@{"Oad","Obc"}]
	},
	"Conclusions" -> {
		EuclideanDistance[GeometricPoint["Oab"], GeometricPoint["Ocd"]] == EuclideanDistance[GeometricPoint["Oad"], GeometricPoint["Obc"]],
		Line[{GeometricPoint["Oab"], GeometricPoint["Ocd"]}] \[Perpendicular] Line[{GeometricPoint["Oad"], GeometricPoint["Obc"]}]
	},
	"EponymousPeople" -> {
		"HHVanAubel::ft8fd"
	},
	"Formulators" -> {
		"HHVanAubel::ft8fd"
	},
	"Provers" -> {
		"HHVanAubel::ft8fd"
	},
	"FormulationDates" -> {
		{1878}
	},
	"ProofDates" -> {
		{1878}
	},
	"FormulationSources" -> {
		"https://en.wikipedia.org/wiki/Van_Aubel's_theorem",
		"van Aubel, H. H. (1878), \" Note concernant les centres de carr\[EAcute]s construits sur les c\[OHat]t\[EAcute]s d'un polygon quelconque \", Nouvelle Correspondance Math\[EAcute]matique (In French), 4: 40\[Dash]44"
	},
	"ProofSources" -> {
		"https://en.wikipedia.org/wiki/Van_Aubel's_theorem",
		"van Aubel, H. H. (1878), \" Note concernant les centres de carr\[EAcute]s construits sur les c\[OHat]t\[EAcute]s d'un polygon quelconque \", Nouvelle Correspondance Math\[EAcute]matique (In French), 4: 40\[Dash]44"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/AubelsTheorem_1000.gif"
|>,

"NapoleonsTheorem" -> <|
	"Label" ->
		"Napoleon's theorem",
	"Statement" ->
		"If equilateral triangles are constructed on the sides of any triangle, either all outward or all inward, the centers of those equilateral triangles themselves form an equilateral triangle.",
	"Hypotheses" -> {
		Triangle[GeometricPoint/@{"C","B","A"}],
		TC==Triangle[GeometricPoint/@{"A","B","Eab"}],
		TB==Triangle[GeometricPoint/@{"C","A","Eac"}],
		TA==Triangle[GeometricPoint/@{"B","C","Ebc"}],
		Regular[TC],
		Regular[TB],
		Regular[TA],
		GeometricPoint["Nab"]==RegionCenter[TC,"Centroid"],
		GeometricPoint["Nac"]==RegionCenter[TB,"Centroid"],
		GeometricPoint["Nbc"]==RegionCenter[TA,"Centroid"],
		Triangle[GeometricPoint/@{"Nab","Nac","Nbc"}]
	},
	"Conclusions" -> {
		Regular @ Triangle[{GeometricPoint["Nab"], GeometricPoint["Nac"], GeometricPoint["Nbc"]}]
	},
	"EponymousPeople" -> {
		"NapoleonBonaparte::8md95"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/OuterNapoleonsTheorem_1000.gif"
|>,

"PappussHexagonTheorem" -> <|
	"Label" ->
		"Pappus's hexagon theorem",
	"Statement" ->
		"If A, B, and C are three points on one line; D, E, and F are three points on another line, and (AE) meets (BD) at X, (AF) meets (CD) at Y, and (BF) meets (CE) at Z, then the three points X, Y, and Z are collinear.",
	"Hypotheses" -> {
		LineThrough[GeometricPoint/@{"A","B","C"}],
		LineThrough[GeometricPoint/@{"D","E","F"}],
		LineThrough[GeometricPoint/@{"A","X","E"}],
		LineThrough[GeometricPoint/@{"B","X","D"}],
		LineThrough[GeometricPoint/@{"A","Y","F"}],
		LineThrough[GeometricPoint/@{"C","Y","D"}],
		LineThrough[GeometricPoint/@{"B","Z","F"}],
		LineThrough[GeometricPoint/@{"C","Z","E"}],
		InfiniteLineThrough[GeometricPoint/@{"X","Z"}]
	},
	"Conclusions" -> {
		Collinear[GeometricPoint["X"], GeometricPoint["Y"], GeometricPoint["Z"]]
	},
	"EponymousPeople" -> {
		"Pappus::zbtf6"
	},
	"Formulators" -> {
		"Pappus::zbtf6"
	},
	"Provers" -> {
		"Pappus::zbtf6"
	},
	"FormulationDates" -> {
		DateObject[{340},"Century"]
	},
	"EponymousPeople" -> {
		DateObject[{340},"Century"]
	},
	"FormulationSources" -> {
		"https://en.wikipedia.org/wiki/Pappus's_hexagon _theorem",
		"https://en.wikipedia.org/wiki/Pappus_of_Alexandria"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/PappusTheorem_1000.gif"
|>,

"MorleysTheorem" -> <|
	"Label" ->
		"Morley's theorem",
	"Statement" ->
		"The points of intersection of the adjacent angle trisectors of the angles of any triangle ABC are the polygon vertices of an equilateral triangle PQR known as the first Morley triangle.",
	"Hypotheses" -> {
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["F"]}],
		Triangle[{GeometricPoint["B"], GeometricPoint["C"], GeometricPoint["D"]}],
		Triangle[{GeometricPoint["C"], GeometricPoint["A"], GeometricPoint["E"]}],
		Triangle[{GeometricPoint["A"], GeometricPoint["F"], GeometricPoint["E"]}],
		Triangle[{GeometricPoint["B"], GeometricPoint["D"], GeometricPoint["F"]}],
		Triangle[{GeometricPoint["C"], GeometricPoint["E"], GeometricPoint["D"]}],
		EqualAngles[
			{GeometricPoint["C"], GeometricPoint["D"], GeometricPoint["F"], GeometricPoint["A"]},
			GeometricPoint["B"]
		],
		EqualAngles[
			{GeometricPoint["B"], GeometricPoint["F"], GeometricPoint["E"], GeometricPoint["C"]},
			GeometricPoint["A"]
		],
		EqualAngles[
			{GeometricPoint["A"], GeometricPoint["E"], GeometricPoint["D"], GeometricPoint["B"]},
			GeometricPoint["C"]
		]
	},
	"Conclusions" -> {
		Regular @ Triangle[{GeometricPoint["D"], GeometricPoint["E"], GeometricPoint["F"]}]
	},
	"EponymousPeople" -> {
		"FrankMorley::6f47j"
	},
	"Formulators" -> {
		"FrankMorley::6f47j"
	},
	"FormulationDates" -> {
		{1899}
	},
	"FormulationSources" -> {
		"http://www.cut-the-knot.org/triangle/Morley/"
	},
	"Diagram" -> Hold @ Import @ "http://mathworld.wolfram.com/images/eps-gif/MorleysTheorem_1000.gif"
|>,

"Example1" -> <|
	"Label" ->
		"example 1",
	"Statement" ->
		"In triangle ABC, let F the midpoint of the side [BC], D and E the feet of the altitudes on [AB] and [AC], respectively. [FG] is perpendicular to [DE] at G. Show that G is the midpoint of [DE].",
	"Hypotheses" -> {
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		GeometricPoint["F"] == Midpoint[Line[{GeometricPoint["B"], GeometricPoint["C"]}]],
		InfiniteLineThrough[{GeometricPoint["D"], GeometricPoint["A"], GeometricPoint["B"]}],
		InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["B"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["C"], GeometricPoint["D"]}],
		InfiniteLineThrough[{GeometricPoint["E"], GeometricPoint["A"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["C"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["E"]}],
		InfiniteLineThrough[{GeometricPoint["G"], GeometricPoint["D"], GeometricPoint["E"]}],
		InfiniteLineThrough[{GeometricPoint["F"], GeometricPoint["G"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["D"], GeometricPoint["E"]}]
	},
	"Conclusions" -> {
		GeometricPoint["G"] == Midpoint[Line[{GeometricPoint["D"], GeometricPoint["E"]}]]
	}
|>,

"Example2" -> <|
	"Label" ->
		"example 2",
	"Statement" ->
		"The circumcenter of a triangle is the orthocenter of its medial triangle.",
	"Hypotheses" -> {
		GeometricPoint["O"] == RegionCenter[Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],"Circumcenter"],
		GeometricPoint["A1"] == Midpoint[Line[{GeometricPoint["B"], GeometricPoint["C"]}]],
		GeometricPoint["B1"] == Midpoint[Line[{GeometricPoint["A"], GeometricPoint["C"]}]],
		GeometricPoint["C1"] == Midpoint[Line[{GeometricPoint["A"], GeometricPoint["B"]}]]
	},
	"Conclusions" -> {
		GeometricPoint["O"] == RegionCenter[Triangle[{GeometricPoint["A1"], GeometricPoint["B1"], GeometricPoint["C1"]}],"Orthocenter"]
	}
|>,

"Example38" -> <|
	"Label" ->
		"example 38",
	"Statement" ->
		"If D1 is the second point of intersection of the altitude ADD1 of the triangle T(ABC) with the circumcircle, center O, and P is the trace on (BC) of the perpendicular from D1 to AC, then the lines AP, AO make equal angles with the bisector of the angle DAC.",
	"Hypotheses" -> {
		Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		CircleThrough[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["D1"], GeometricPoint["C"]}, GeometricPoint["O"]],
		InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["D1"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["D"], GeometricPoint["A"], GeometricPoint["D1"]}],
		InfiniteLineThrough[{GeometricPoint["D"], GeometricPoint["B"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["B"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["D1"], GeometricPoint["P"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["C"]}]
	},
	"Conclusions" -> {
		Angle[GeometricPoint["O"], GeometricPoint["A"], GeometricPoint["D"]] == Angle[GeometricPoint["C"], GeometricPoint["A"], GeometricPoint["P"]]
	}
|>,

"Example45" -> <|
	"Label" ->
		"example 45",
	"Statement"->
		"The perpendicular from the point of intersection of two opposite sides, produced, of a cyclic quadrilateral upon the line joining the midpoints of the two sides considered passes through the anticenter of the quadrilateral.",
	"Hypotheses" -> {
		Cyclic[Polygon[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"], GeometricPoint["D"]}]],
		GeometricPoint["S"] == Midpoint[Line[{GeometricPoint["A"], GeometricPoint["D"]}]],
		GeometricPoint["Q"] == Midpoint[Line[{GeometricPoint["B"], GeometricPoint["C"]}]],
		InfiniteLineThrough[{GeometricPoint["I"], GeometricPoint["A"], GeometricPoint["D"]}],
		InfiniteLineThrough[{GeometricPoint["I"], GeometricPoint["B"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["I"], GeometricPoint["Z"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["S"], GeometricPoint["Z"], GeometricPoint["Q"]}]
	},
	"Conclusions" -> {
		RegionCenter[Polygon[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"], GeometricPoint["D"]}], "Anticenter"] \[Element] InfiniteLineThrough[{GeometricPoint["I"], GeometricPoint["Z"]}]
	}
|>,

"Example48" -> <|
	"Label" ->
		"example 48",
	"Statement" ->
		"Let E be the intersection of the two diagonals (AC) and (BD) of cyclic quadrilateral Q (ABCD). Let I be the center of circumcircle of T (ABE). One has that (IE) \[UpTee] (DC).",
	"Hypotheses" -> {
		Cyclic[Polygon[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"], GeometricPoint["D"]}]],
		InfiniteLineThrough[{GeometricPoint["E"], GeometricPoint["A"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["E"], GeometricPoint["B"], GeometricPoint["D"]}],
		GeometricPoint["I"] == RegionCenter[Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["E"]}],"Circumcenter"]
	},
	"Conclusions" -> {
		InfiniteLineThrough[{GeometricPoint["I"], GeometricPoint["E"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["D"], GeometricPoint["C"]}]
	}
|>,

"Example107" -> <|
	"Label" ->
		"example 107",
	"Statement"->
		"A line (AD) through the vertex A meets the circumcircle of the triangle T(ABC) in D. If U, V are the orthocenters of the triangle T(ABD), T(ACD), respectively, prove that (UV) is parallel to (BC) and |UV| = |BC|.",
	"Hypotheses" -> {
		GeometricPoint["D"] \[Element] CircleThrough[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		GeometricPoint["U"] == RegionCenter[Triangle[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["D"]}],"Orthocenter"],
		GeometricPoint["V"] == RegionCenter[Triangle[{GeometricPoint["A"], GeometricPoint["C"], GeometricPoint["D"]}],"Orthocenter"]
	},
	"Conclusions" -> {
		Parallel[InfiniteLineThrough[{GeometricPoint["U"], GeometricPoint["V"]}], InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["C"]}]],
		Congruent[Line[{GeometricPoint["U"], GeometricPoint["V"]}], Line[{GeometricPoint["B"], GeometricPoint["C"]}]]
	}
|>,

(*"CrossTheorem" -> <|
	"Label" ->
		"Cross Theorem Setup",
	"Statement" ->
		"On edges of a triangle ABC, build 3 squares, then draw edges between neighboring corners. The three new triangles have area equal to ABC.",
	"Hypotheses" -> {
		GeometricPoint["A"],
		GeometricPoint["B"],
		GeometricPoint["C"],
		GeometricPoint["R"],
		GeometricPoint["S"],
		GeometricPoint["T"],		
		GeometricPoint["U"],		
		GeometricPoint["V"],
		GeometricPoint["W"],
		(*ABSR, CBTU, ACVW are squares*)
		Regular @ Polygon[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["S"], GeometricPoint["R"]}],
		Regular @ Polygon[{GeometricPoint["C"], GeometricPoint["B"], GeometricPoint["T"], GeometricPoint["U"]}],
		Regular @ Polygon[{GeometricPoint["A"], GeometricPoint["C"], GeometricPoint["V"], GeometricPoint["W"]}]
	},
	"Conclusions" -> {
		Area[Polygon[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}]] == 
		Area[Polygon[{GeometricPoint["A"], GeometricPoint["W"], GeometricPoint["R"]}]] ==
		Area[Polygon[{GeometricPoint["B"], GeometricPoint["S"], GeometricPoint["T"]}]] ==
		Area[Polygon[{GeometricPoint["C"], GeometricPoint["U"], GeometricPoint["V"]}]]
	},
	"Diagram" -> Hold @ CloudImport @ "https://www.wolframcloud.com/objects/user-ff215380-8f0e-483f-a2dd-3f3dffa8aaf7/Geometry/CrossTheorem"
|>,*)

"AConcurrencyFromAPointAndATrianglesExcenters" -> <|
	"Label" ->
		"A Concurrency from a Point and a Triangle's Excenters",
	"Statement" ->
		"Let ABC be a triangle and P a point. Let A', B', and C' be the excenters opposite A, B, and C, respectively. Let PA', PB', and PC' intersect BC, AC, and AB at A'', B'', and C'', respectively. Then AA'', BB'', and CC'' are concurrent.",
	"Hypotheses" -> {
		LineThrough[{GeometricPoint["A"], GeometricPoint["ab"], GeometricPoint["B"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["ba"], GeometricPoint["B"]}],
		LineThrough[{GeometricPoint["B"], GeometricPoint["bc"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["B"], GeometricPoint["cb"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["ac"], GeometricPoint["C"]}],
		LineThrough[{GeometricPoint["A"], GeometricPoint["ca"], GeometricPoint["C"]}],
		Angle[GeometricPoint["A'"], GeometricPoint["B"], GeometricPoint["C"]] == Angle[GeometricPoint["A'"], GeometricPoint["B"], GeometricPoint["ba"]],
		Angle[GeometricPoint["A'"], GeometricPoint["C"], GeometricPoint["B"]] == Angle[GeometricPoint["A'"], GeometricPoint["C"], GeometricPoint["cb"]],
		Angle[GeometricPoint["B'"], GeometricPoint["A"], GeometricPoint["C"]] == Angle[GeometricPoint["B'"], GeometricPoint["A"], GeometricPoint["ab"]],
		Angle[GeometricPoint["B'"], GeometricPoint["C"], GeometricPoint["A"]] == Angle[GeometricPoint["B'"], GeometricPoint["C"], GeometricPoint["ca"]],		
		Angle[GeometricPoint["C'"], GeometricPoint["A"], GeometricPoint["B"]] == Angle[GeometricPoint["C'"], GeometricPoint["A"], GeometricPoint["ac"]],
		Angle[GeometricPoint["C'"], GeometricPoint["B"], GeometricPoint["A"]] == Angle[GeometricPoint["C'"], GeometricPoint["B"], GeometricPoint["bc"]],
		Collinear[GeometricPoint["A'"], GeometricPoint["A''"], GeometricPoint["P"]],
		Collinear[GeometricPoint["B"], GeometricPoint["A''"], GeometricPoint["C"]],
		Collinear[GeometricPoint["B'"], GeometricPoint["B''"], GeometricPoint["P"]],
		Collinear[GeometricPoint["A"], GeometricPoint["B''"], GeometricPoint["C"]],
		Collinear[GeometricPoint["C'"], GeometricPoint["C''"], GeometricPoint["P"]],
		Collinear[GeometricPoint["A"], GeometricPoint["C''"], GeometricPoint["B"]]
		},
	"Conclusions" -> {
		Concurrent[
			InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["A''"]}],
			InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["B''"]}],
			InfiniteLineThrough[{GeometricPoint["C"], GeometricPoint["C''"]}]
		]
	},
	"Diagram" -> Hold @ CloudImport @ "https://www.wolframcloud.com/objects/user-ff215380-8f0e-483f-a2dd-3f3dffa8aaf7/Geometry/AConcurrencyFromAPointAndATrianglesExcenters"
|>,

"AConcurrencyFromCircumcirclesOfSubtriangles" -> <|
	"Label" ->
		"A Concurrency from Circumcircles of Subtriangles",
	"Statement" ->
		"Let ABC be a triangle and let the incircle intersect BC, CA, and AB at A', B', and C', respectively. Let the circumcircles of AB'C', A'BC', and A'B'C intersect the circumcircle of ABC (apart from A, B, and C) at A'', B'', and C'', respectively. Then A'A'', B'B'', and C'C'' are concurrent.",
	"Hypotheses" -> {
		CI == Insphere[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		CC == CircleThrough[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		GeometricPoint["A'"] \[Element] CI,
		InfiniteLineThrough[{GeometricPoint["A'"], GeometricPoint["B"], GeometricPoint["C"]}],
		GeometricPoint["B'"] \[Element] CI,
		InfiniteLineThrough[{GeometricPoint["B'"], GeometricPoint["A"], GeometricPoint["C"]}],
		GeometricPoint["C'"] \[Element] CI,
		InfiniteLineThrough[{GeometricPoint["C'"], GeometricPoint["A"], GeometricPoint["B"]}],
		C1 == CircleThrough[{GeometricPoint["A"], GeometricPoint["B'"], GeometricPoint["C'"]}],
		C2 == CircleThrough[{GeometricPoint["A'"], GeometricPoint["B"], GeometricPoint["C'"]}],
		C3 == CircleThrough[{GeometricPoint["A'"], GeometricPoint["B'"], GeometricPoint["C"]}],
		GeometricPoint["A''"] \[Element] CC,
		GeometricPoint["A''"] \[Element] C1,
		GeometricPoint["B''"] \[Element] CC,
		GeometricPoint["B''"] \[Element] C2,
		GeometricPoint["C''"] \[Element] CC,
		GeometricPoint["C''"] \[Element] C3
	},
	"Conclusions" -> {
		Concurrent[
			InfiniteLineThrough[{GeometricPoint["A'"], GeometricPoint["A''"]}],
			InfiniteLineThrough[{GeometricPoint["B'"], GeometricPoint["B''"]}],
			InfiniteLineThrough[{GeometricPoint["C'"], GeometricPoint["C''"]}]
		]
	},
	"Diagram" -> Hold @ CloudImport @ "https://www.wolframcloud.com/objects/user-ff215380-8f0e-483f-a2dd-3f3dffa8aaf7/Geometry/AConcurrencyFromCircumcirclesOfSubtriangles"
|>,

"AConcurrencyFromSixPedalPoints" -> <|
	"Label" ->
		"A Concurrency from Six Pedal Points",
	"Statement" ->
		"Let ABC be a triangle and P and Q be two other points. Drop perpendiculars from P to the three sides of ABC. Let X, Y, and Z be the feet of the perpendiculars (pedals) of Q on the three perpendiculars. Let X', Y', and Z' be the pedals of P on AQ, BQ, and CQ. Then XX', YY', and ZZ' are concurrent.",
	"Hypotheses" -> {
		Collinear[GeometricPoint["A"], GeometricPoint["X'"], GeometricPoint["Q"]],
		Collinear[GeometricPoint["B"], GeometricPoint["Y'"], GeometricPoint["Q"]],
		Collinear[GeometricPoint["C"], GeometricPoint["Z'"], GeometricPoint["Q"]],
		InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["C"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["X"]}],
		InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["C"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["Y"]}],
		InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["B"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["Z"]}],
		InfiniteLineThrough[{GeometricPoint["Q"], GeometricPoint["X"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["X"]}],
		InfiniteLineThrough[{GeometricPoint["Q"], GeometricPoint["Y"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["Y"]}],
		InfiniteLineThrough[{GeometricPoint["Q"], GeometricPoint["Z"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["Z"]}],
		InfiniteLineThrough[{GeometricPoint["Q"], GeometricPoint["X'"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["X'"]}],
		InfiniteLineThrough[{GeometricPoint["Q"], GeometricPoint["Y'"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["Y'"]}],
		InfiniteLineThrough[{GeometricPoint["Q"], GeometricPoint["Z'"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["P"], GeometricPoint["Z'"]}]
	},
	"Conclusions" -> {
		Concurrent[
			InfiniteLineThrough[{GeometricPoint["X"], GeometricPoint["X'"]}],
			InfiniteLineThrough[{GeometricPoint["Y"], GeometricPoint["Y'"]}],
			InfiniteLineThrough[{GeometricPoint["Z"], GeometricPoint["Z'"]}]
		]
	},
	"Diagram" -> Hold @ CloudImport @ "https://www.wolframcloud.com/objects/user-ff215380-8f0e-483f-a2dd-3f3dffa8aaf7/Geometry/AConcurrencyFromSixPedalPoints"
|>,

"AConcurrencyFromTheMidpointsOfLineSegmentsThroughTheCircumcenter" -> <|
	"Label" ->
		"A Concurrency from the Midpoints of Line Segments through the Circumcenter",
	"Statement" ->
		"Let ABC be a triangle with circumcenter O. Let A', B', and C' be the perpendicular projections of A, B, and C onto BC, CA, and AB, respectively. Let A'', B'', and C'' be the midpoints of AD, BE, and CF, respectively. Then A'A'', B'B'', and C'C'' are concurrent.",
	"Hypotheses" -> {
		CC == CircleThrough[{GeometricPoint["A"], GeometricPoint["B"], GeometricPoint["C"]}],
		Circle[GeometricPoint["O"], r] == CC,
		Collinear[GeometricPoint["B"], GeometricPoint["A'"], GeometricPoint["C"]],
		Collinear[GeometricPoint["A"], GeometricPoint["B'"], GeometricPoint["C"]],
		Collinear[GeometricPoint["A"], GeometricPoint["C'"], GeometricPoint["B"]],
		InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["A'"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["B"], GeometricPoint["B'"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["C"]}],
		InfiniteLineThrough[{GeometricPoint["C"], GeometricPoint["C'"]}] \[Perpendicular] InfiniteLineThrough[{GeometricPoint["A"], GeometricPoint["B"]}],
		Collinear[GeometricPoint["A"], GeometricPoint["D"], GeometricPoint["O"]],
		Collinear[GeometricPoint["B"], GeometricPoint["D"], GeometricPoint["C"]],
		Collinear[GeometricPoint["B"], GeometricPoint["E"], GeometricPoint["O"]],
		Collinear[GeometricPoint["A"], GeometricPoint["E"], GeometricPoint["C"]],
		Collinear[GeometricPoint["C"], GeometricPoint["F"], GeometricPoint["O"]],
		Collinear[GeometricPoint["A"], GeometricPoint["F"], GeometricPoint["B"]],
		(*Midpoints*)
		GeometricPoint["A''"] == Midpoint[{GeometricPoint["A"],GeometricPoint["D"]}],
		GeometricPoint["B''"] == Midpoint[{GeometricPoint["B"],GeometricPoint["E"]}],
		GeometricPoint["C''"] == Midpoint[{GeometricPoint["C"],GeometricPoint["F"]}]
	},
	"Conclusions" -> {
		Concurrent[
			InfiniteLineThrough[{GeometricPoint["A'"], GeometricPoint["A''"]}],
			InfiniteLineThrough[{GeometricPoint["B'"], GeometricPoint["B''"]}],
			InfiniteLineThrough[{GeometricPoint["C'"], GeometricPoint["C''"]}]
		]
	},
	"Diagram" -> Hold @ CloudImport @ "https://www.wolframcloud.com/objects/user-ff215380-8f0e-483f-a2dd-3f3dffa8aaf7/Geometry/AConcurrencyFromTheMidpointsOfLineSegmentsThroughTheCircumcenter"
|>

|>,

"Properties" -> <|

"AdditionalPeople" -> <|
	"Label" ->"additional people involved",
	"FormattingFunction" ->
		Map[Entity["Person", #]&]
|>,

"AlternateStatements" -> <|
	"Label" ->"alternate statements"
|>,

"Assertions" -> <|
	"Label" ->"assertions",
	"DefaultFunction" ->
		Function[ent, Cases[ent["Hypotheses"], _Collinear | _Concurrent | _Cyclic | _Element | _Equal | _Greater | _Less | _Not | _NotElement | _Parallel | _Perpendicular | _RegionDisjoint | _Unequal, {1}] // Union]
|>,

"AssociatedEntities" -> <|
	"Label" ->"associated entities",
	"DefaultFunction" ->
		Function[ent, Replace[Union @ Flatten @ DeleteMissing @ ent[{"AssociatedPeople","MathWorld"}], {} -> Missing["NotAvailable"]]]
|>,

"AssociatedPeople" -> <|
	"Label" ->"associated people",
	"DefaultFunction" ->
		Function[ent, Replace[Union @@ DeleteMissing @ ent[{"EponymousPeople","AdditionalPeople","Formulators","Provers"}], {} -> Missing["NotAvailable"]]]
|>,

"Conclusions" -> <|
	"Label" ->"conclusions"
|>,

"Constructs" -> <|
	"Label" ->"constructs",
	"DefaultFunction" ->
		Function[ent, Cases[ent[{"Hypotheses","Conclusions"}], _Circle | _GeometricPoint | _InfiniteLineThrough | _Line | _Point | _Polygon, Infinity] // Union]
|>,

"Diagram" -> <|
	"Label" ->"diagram",
	"FormattingFunction" ->
		ReleaseHold
|>,

"EponymousPeople" -> <|
	"Label" ->"eponymous people",
	"FormattingFunction" ->
		Map[Entity["Person", #]&]
|>,

"FormulationDates" -> <|
	"Label" ->"formulation dates",
	"FormattingFunction" ->
		Map[DateObject]
|>,

"FormulationSources" -> <|
	"Label" ->"formulation sources"
|>,

"Formulators" -> <|
	"Label" ->"formulators",
	"FormattingFunction" ->
		Map[Entity["Person", #]&]
|>,

"Hypotheses" -> <|
	"Label" ->"hypotheses"
|>,

"Label" -> <|
	"Label" ->"label"
|>,

"MathWorld" -> <|
	"Label" ->"MathWorld entry",
	"DefaultFunction" ->
		Function[ent, Replace[EntityValue @ Entity["MathWorld", CanonicalName @ ent], _Missing :> Missing["NotAvailable"]]],
	"FormattingFunction" ->
		Function[entry, Switch[entry, _String, Entity["MathWorld", entry], _, entry]]
|>,

"ProofDates" -> <|
	"Label" ->"proof dates",
	"FormattingFunction" ->
		Map[DateObject]
|>,

"ProofSources" -> <|
	"Label" ->"proof sources"
|>,

"Provers" -> <|
	"Label" ->"provers",
	"FormattingFunction" ->
		Map[Entity["Person", #]&]
|>,

"References" -> <|
	"Label" ->"references"
|>,

"Statement" -> <|
	"Label" ->"statement",
	"FormattingFunction" -> Text
|>,

"Variables" -> <|
	"Label" ->"variables",
	"DefaultFunction" ->
		Function[ent, Cases[ent[{"Hypotheses","Conclusions"}], _?AtomQ, Infinity, Heads -> False] // DeleteCases[_?NumericQ | Reals | Unspecified |"NotAvailable"] // Union]
|>

|>

|>]

End[]

System`Private`RestoreContextPath[];

{
	"EntityFramework`$SceneExample"
	
}



