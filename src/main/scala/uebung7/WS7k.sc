val Paradigmen = List(("erlang", "funktional"), ("erlang", "logisch"), ("prolog", "logisch"),
  ("scala", "funktional"), ("scala", "objektorientiert"), ("scala", "logisch"),
  ("java", "objektorientiert"))

//a
Paradigmen.filter(_._2 == "objektorientiert").flatMap(x => List(x._1))

//b
Paradigmen.filter(x => x._1 == "erlang" || x._1 == "java").flatMap(x => List(x._2))

//c
Paradigmen.map(x => (x._1, 1)).groupBy(_._1).
  map{ case (s, l) => (s, l.length)}.filter(_._2>1).map(_._1)

//d
Paradigmen.map(_._1)::Paradigmen.map(_._2)
Paradigmen map (x=> (x._1,Paradigmen filter(y=> x._1==y._1) map (_._2)))