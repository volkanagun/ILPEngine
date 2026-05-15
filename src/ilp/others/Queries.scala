package ilp.others

object Queries {

  def centipedeSparql(): String =
    """
      |PREFIX ex: <http://example.org/>
      |
      |SELECT DISTINCT ?V0 ?V1 ?V2
      |WHERE {
      |
      |  {
      |    # Rule 1
      |    ?tbp ex:predicate ex:true_blackPayoff ;
      |         ex:arg1 ?V0 ;
      |         ex:arg2 ?V2 .
      |
      |    ?i15 ex:predicate ex:int_15 ;
      |         ex:arg1 ?V2 .
      |
      |    ?ab ex:predicate ex:agent_black ;
      |        ex:arg1 ?V1 .
      |
      |    ?tc ex:predicate ex:true_control ;
      |        ex:arg1 ?V0 ;
      |        ex:arg2 ?V3 .
      |
      |    ?aw ex:predicate ex:agent_white ;
      |        ex:arg1 ?V3 .
      |  }
      |
      |  UNION
      |
      |  {
      |    # Rule 2
      |    ?ab ex:predicate ex:agent_black ;
      |        ex:arg1 ?V1 .
      |
      |    ?tc ex:predicate ex:true_control ;
      |        ex:arg1 ?V0 ;
      |        ex:arg2 ?V1 .
      |
      |    ?tbp ex:predicate ex:true_blackPayoff ;
      |         ex:arg1 ?V0 ;
      |         ex:arg2 ?V2 .
      |
      |    ?succ ex:predicate ex:succ ;
      |          ex:arg1 ?V2 ;
      |          ex:arg2 ?V3 .
      |
      |    ?twp ex:predicate ex:true_whitePayoff ;
      |         ex:arg1 ?V0 ;
      |         ex:arg2 ?V3 .
      |  }
      |
      |  UNION
      |
      |  {
      |    # Rule 3
      |    ?i0 ex:predicate ex:int_0 ;
      |        ex:arg1 ?V2 .
      |
      |    ?tc ex:predicate ex:true_control ;
      |        ex:arg1 ?V0 ;
      |        ex:arg2 ?V1 .
      |
      |    ?tbp ex:predicate ex:true_blackPayoff ;
      |         ex:arg1 ?V0 ;
      |         ex:arg2 ?V4 .
      |
      |    ?s1 ex:predicate ex:succ ;
      |        ex:arg1 ?V6 ;
      |        ex:arg2 ?V4 .
      |
      |    ?s2 ex:predicate ex:succ ;
      |        ex:arg1 ?V3 ;
      |        ex:arg2 ?V6 .
      |
      |    ?s3 ex:predicate ex:succ ;
      |        ex:arg1 ?V5 ;
      |        ex:arg2 ?V3 .
      |  }
      |
      |  UNION
      |
      |  {
      |    # Rule 4
      |    ?i0 ex:predicate ex:int_0 ;
      |        ex:arg1 ?V2 .
      |
      |    ?role ex:predicate ex:role ;
      |          ex:arg1 ?V1 .
      |
      |    ?tc ex:predicate ex:true_control ;
      |        ex:arg1 ?V0 ;
      |        ex:arg2 ?V3 .
      |
      |    ?aw ex:predicate ex:agent_white ;
      |        ex:arg1 ?V3 .
      |
      |    ?twp ex:predicate ex:true_whitePayoff ;
      |         ex:arg1 ?V0 ;
      |         ex:arg2 ?V4 .
      |
      |    ?succ ex:predicate ex:succ ;
      |          ex:arg1 ?V5 ;
      |          ex:arg2 ?V4 .
      |  }
      |
      |  UNION
      |
      |  {
      |    # Rule 5
      |    ?i0 ex:predicate ex:int_0 ;
      |        ex:arg1 ?V2 .
      |
      |    ?aw ex:predicate ex:agent_white ;
      |        ex:arg1 ?V1 .
      |
      |    ?tbp ex:predicate ex:true_blackPayoff ;
      |         ex:arg1 ?V0 ;
      |         ex:arg2 ?V4 .
      |
      |    ?s1 ex:predicate ex:succ ;
      |        ex:arg1 ?V6 ;
      |        ex:arg2 ?V4 .
      |
      |    ?s2 ex:predicate ex:succ ;
      |        ex:arg1 ?V3 ;
      |        ex:arg2 ?V6 .
      |
      |    ?s3 ex:predicate ex:succ ;
      |        ex:arg1 ?V5 ;
      |        ex:arg2 ?V3 .
      |  }
      |
      |  UNION
      |
      |  {
      |    # Rule 6
      |    ?aw ex:predicate ex:agent_white ;
      |        ex:arg1 ?V1 .
      |
      |    ?twp ex:predicate ex:true_whitePayoff ;
      |         ex:arg1 ?V0 ;
      |         ex:arg2 ?V2 .
      |
      |    ?succ ex:predicate ex:succ ;
      |          ex:arg1 ?V3 ;
      |          ex:arg2 ?V2 .
      |
      |    ?tbp ex:predicate ex:true_blackPayoff ;
      |         ex:arg1 ?V0 ;
      |         ex:arg2 ?V3 .
      |
      |    ?ab ex:predicate ex:agent_black ;
      |        ex:arg1 ?V4 .
      |
      |    ?tc ex:predicate ex:true_control ;
      |        ex:arg1 ?V0 ;
      |        ex:arg2 ?V4 .
      |  }
      |}
      |""".stripMargin



  def pteSparql(): String = {
    "PREFIX ex: <http://example.org/>\n\nSELECT DISTINCT ?V0\nWHERE {\n\n  {\n    # pte_active(V0):-\n    #   pte_atm(V0,V1,V2,V4,V3),\n    #   pte_phenol(V5,V1),\n    #   pte_ketone(V5,V1).\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V1 ;\n         ex:arg3 ?V2 ;\n         ex:arg4 ?V4 ;\n         ex:arg5 ?V3 .\n\n    ?phenol ex:predicate ex:pte_phenol ;\n             ex:arg1 ?V5 ;\n             ex:arg2 ?V1 .\n\n    ?ketone ex:predicate ex:pte_ketone ;\n            ex:arg1 ?V5 ;\n            ex:arg2 ?V1 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_atm(V0,V1,V2,V4,V3),\n    #   pte_nitro(V5,V1),\n    #   pte_non_ar_hetero_5_ring(V5,V1).\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V1 ;\n         ex:arg3 ?V2 ;\n         ex:arg4 ?V4 ;\n         ex:arg5 ?V3 .\n\n    ?nitro ex:predicate ex:pte_nitro ;\n           ex:arg1 ?V5 ;\n           ex:arg2 ?V1 .\n\n    ?ring5 ex:predicate ex:pte_non_ar_hetero_5_ring ;\n           ex:arg1 ?V5 ;\n           ex:arg2 ?V1 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_alkyl_halide(V5,V1),\n    #   pte_methyl(V5,V1),\n    #   pte_atm(V0,V1,V2,V4,V3).\n\n    ?alkyl_halide ex:predicate ex:pte_alkyl_halide ;\n                  ex:arg1 ?V5 ;\n                  ex:arg2 ?V1 .\n\n    ?methyl ex:predicate ex:pte_methyl ;\n            ex:arg1 ?V5 ;\n            ex:arg2 ?V1 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V1 ;\n         ex:arg3 ?V2 ;\n         ex:arg4 ?V4 ;\n         ex:arg5 ?V3 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_alcohol(V5,V1),\n    #   pte_ester(V5,V1),\n    #   pte_atm(V0,V1,V2,V4,V3).\n\n    ?alcohol ex:predicate ex:pte_alcohol ;\n             ex:arg1 ?V5 ;\n             ex:arg2 ?V1 .\n\n    ?ester ex:predicate ex:pte_ester ;\n           ex:arg1 ?V5 ;\n           ex:arg2 ?V1 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V1 ;\n         ex:arg3 ?V2 ;\n         ex:arg4 ?V4 ;\n         ex:arg5 ?V3 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_atm(V0,V1,V2,V4,V3),\n    #   pte_imine(V5,V1),\n    #   pte_ames(V5).\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V1 ;\n         ex:arg3 ?V2 ;\n         ex:arg4 ?V4 ;\n         ex:arg5 ?V3 .\n\n    ?imine ex:predicate ex:pte_imine ;\n           ex:arg1 ?V5 ;\n           ex:arg2 ?V1 .\n\n    ?ames ex:predicate ex:pte_ames ;\n          ex:arg1 ?V5 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_sulfide(V5,V1),\n    #   pte_alkyl_halide(V5,V1),\n    #   pte_atm(V0,V1,V2,V4,V3).\n\n    ?sulfide ex:predicate ex:pte_sulfide ;\n             ex:arg1 ?V5 ;\n             ex:arg2 ?V1 .\n\n    ?alkyl_halide ex:predicate ex:pte_alkyl_halide ;\n                  ex:arg1 ?V5 ;\n                  ex:arg2 ?V1 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V1 ;\n         ex:arg3 ?V2 ;\n         ex:arg4 ?V4 ;\n         ex:arg5 ?V3 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_methyl(V1,V3),\n    #   pte_five_ring(V1,V3),\n    #   pte_ames(V1),\n    #   pte_atm(V0,V3,V4,V2,V5).\n\n    ?methyl ex:predicate ex:pte_methyl ;\n            ex:arg1 ?V1 ;\n            ex:arg2 ?V3 .\n\n    ?five_ring ex:predicate ex:pte_five_ring ;\n               ex:arg1 ?V1 ;\n               ex:arg2 ?V3 .\n\n    ?ames ex:predicate ex:pte_ames ;\n          ex:arg1 ?V1 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V3 ;\n         ex:arg3 ?V4 ;\n         ex:arg4 ?V2 ;\n         ex:arg5 ?V5 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_sulfo(V1,V3),\n    #   pte_ames(V1),\n    #   pte_mutagenic(V1),\n    #   pte_atm(V0,V3,V4,V2,V5).\n\n    ?sulfo ex:predicate ex:pte_sulfo ;\n           ex:arg1 ?V1 ;\n           ex:arg2 ?V3 .\n\n    ?ames ex:predicate ex:pte_ames ;\n          ex:arg1 ?V1 .\n\n    ?mutagenic ex:predicate ex:pte_mutagenic ;\n               ex:arg1 ?V1 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V3 ;\n         ex:arg3 ?V4 ;\n         ex:arg4 ?V2 ;\n         ex:arg5 ?V5 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_six_ring(V1,V3),\n    #   pte_ames(V1),\n    #   pte_ester(V1,V3),\n    #   pte_atm(V0,V3,V4,V2,V5).\n\n    ?six_ring ex:predicate ex:pte_six_ring ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V3 .\n\n    ?ames ex:predicate ex:pte_ames ;\n          ex:arg1 ?V1 .\n\n    ?ester ex:predicate ex:pte_ester ;\n           ex:arg1 ?V1 ;\n           ex:arg2 ?V3 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V3 ;\n         ex:arg3 ?V4 ;\n         ex:arg4 ?V2 ;\n         ex:arg5 ?V5 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_ether(V1,V3),\n    #   pte_phenol(V1,V3),\n    #   pte_ames(V1),\n    #   pte_atm(V0,V3,V4,V2,V5).\n\n    ?ether ex:predicate ex:pte_ether ;\n           ex:arg1 ?V1 ;\n           ex:arg2 ?V3 .\n\n    ?phenol ex:predicate ex:pte_phenol ;\n             ex:arg1 ?V1 ;\n             ex:arg2 ?V3 .\n\n    ?ames ex:predicate ex:pte_ames ;\n          ex:arg1 ?V1 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V3 ;\n         ex:arg3 ?V4 ;\n         ex:arg4 ?V2 ;\n         ex:arg5 ?V5 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_non_ar_hetero_6_ring(V1,V3),\n    #   pte_ames(V1),\n    #   pte_amine(V1,V3),\n    #   pte_atm(V0,V3,V4,V2,V5).\n\n    ?ring6 ex:predicate ex:pte_non_ar_hetero_6_ring ;\n           ex:arg1 ?V1 ;\n           ex:arg2 ?V3 .\n\n    ?ames ex:predicate ex:pte_ames ;\n          ex:arg1 ?V1 .\n\n    ?amine ex:predicate ex:pte_amine ;\n           ex:arg1 ?V1 ;\n           ex:arg2 ?V3 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V3 ;\n         ex:arg3 ?V4 ;\n         ex:arg4 ?V2 ;\n         ex:arg5 ?V5 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_ketone(V1,V3),\n    #   pte_mutagenic(V1),\n    #   pte_methoxy(V1,V3),\n    #   pte_atm(V0,V3,V4,V2,V5).\n\n    ?ketone ex:predicate ex:pte_ketone ;\n            ex:arg1 ?V1 ;\n            ex:arg2 ?V3 .\n\n    ?mutagenic ex:predicate ex:pte_mutagenic ;\n               ex:arg1 ?V1 .\n\n    ?methoxy ex:predicate ex:pte_methoxy ;\n             ex:arg1 ?V1 ;\n             ex:arg2 ?V3 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V3 ;\n         ex:arg3 ?V4 ;\n         ex:arg4 ?V2 ;\n         ex:arg5 ?V5 .\n  }\n\n  UNION\n\n  {\n    # pte_active(V0):-\n    #   pte_ames(V1),\n    #   pte_amine(V1,V3),\n    #   pte_atm(V0,V3,V4,V2,V5),\n    #   pte_methyl(V1,V3),\n    #   pte_mutagenic(V1).\n\n    ?ames ex:predicate ex:pte_ames ;\n          ex:arg1 ?V1 .\n\n    ?amine ex:predicate ex:pte_amine ;\n           ex:arg1 ?V1 ;\n           ex:arg2 ?V3 .\n\n    ?atm ex:predicate ex:pte_atm ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V3 ;\n         ex:arg3 ?V4 ;\n         ex:arg4 ?V2 ;\n         ex:arg5 ?V5 .\n\n    ?methyl ex:predicate ex:pte_methyl ;\n            ex:arg1 ?V1 ;\n            ex:arg2 ?V3 .\n\n    ?mutagenic ex:predicate ex:pte_mutagenic ;\n               ex:arg1 ?V1 .\n  }\n}\nORDER BY ?V0"
  }

  def yeastSparql(): String = {
    "PREFIX ex: <http://example.org/>\n\nSELECT DISTINCT ?V0\nWHERE {\n\n  {\n    # proteins(V0):- path(V0,V2), location(V0,V1).\n    ?path ex:predicate ex:path ;\n          ex:arg1 ?V0 ;\n          ex:arg2 ?V2 .\n\n    ?loc ex:predicate ex:location ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V1 .\n  }\n\n  UNION\n\n  {\n    # proteins(V0):- enzyme(V0,V1), renzyme(V0,V1).\n    ?enz ex:predicate ex:enzyme ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V1 .\n\n    ?renz ex:predicate ex:renzyme ;\n          ex:arg1 ?V0 ;\n          ex:arg2 ?V1 .\n  }\n\n  UNION\n\n  {\n    # proteins(V0):- path(V2,V3), interaction(V3,V0,V1).\n    ?path ex:predicate ex:path ;\n          ex:arg1 ?V2 ;\n          ex:arg2 ?V3 .\n\n    ?inter ex:predicate ex:interaction ;\n           ex:arg1 ?V3 ;\n           ex:arg2 ?V0 ;\n           ex:arg3 ?V1 .\n  }\n\n  UNION\n\n  {\n    # proteins(V0):- protein_class(V0,V1), rprotein_class(V0,V1).\n    ?pc ex:predicate ex:protein_class ;\n        ex:arg1 ?V0 ;\n        ex:arg2 ?V1 .\n\n    ?rpc ex:predicate ex:rprotein_class ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V1 .\n  }\n\n  UNION\n\n  {\n    # proteins(V0):- protein_class(V0,V4), interaction(V2,V0,V1), rprotein_class(V3,V4).\n    ?pc ex:predicate ex:protein_class ;\n        ex:arg1 ?V0 ;\n        ex:arg2 ?V4 .\n\n    ?inter ex:predicate ex:interaction ;\n           ex:arg1 ?V2 ;\n           ex:arg2 ?V0 ;\n           ex:arg3 ?V1 .\n\n    ?rpc ex:predicate ex:rprotein_class ;\n         ex:arg1 ?V3 ;\n         ex:arg2 ?V4 .\n  }\n\n  UNION\n\n  {\n    # proteins(V0):- phenotype(V0,V3), renzyme(V0,V2), rphenotype(V1,V3).\n    ?phen ex:predicate ex:phenotype ;\n          ex:arg1 ?V0 ;\n          ex:arg2 ?V3 .\n\n    ?renz ex:predicate ex:renzyme ;\n          ex:arg1 ?V0 ;\n          ex:arg2 ?V2 .\n\n    ?rphen ex:predicate ex:rphenotype ;\n           ex:arg1 ?V1 ;\n           ex:arg2 ?V3 .\n  }\n\n  UNION\n\n  {\n    # proteins(V0):- protein_class(V0,V3), rprotein_class(V2,V3), enzyme(V2,V1).\n    ?pc ex:predicate ex:protein_class ;\n        ex:arg1 ?V0 ;\n        ex:arg2 ?V3 .\n\n    ?rpc ex:predicate ex:rprotein_class ;\n         ex:arg1 ?V2 ;\n         ex:arg2 ?V3 .\n\n    ?enz ex:predicate ex:enzyme ;\n         ex:arg1 ?V2 ;\n         ex:arg2 ?V1 .\n  }\n\n  UNION\n\n  {\n    # proteins(V0):- interaction(V3,V0,V1), protein_class(V3,V2), rprotein_class(V3,V2).\n    ?inter ex:predicate ex:interaction ;\n           ex:arg1 ?V3 ;\n           ex:arg2 ?V0 ;\n           ex:arg3 ?V1 .\n\n    ?pc ex:predicate ex:protein_class ;\n        ex:arg1 ?V3 ;\n        ex:arg2 ?V2 .\n\n    ?rpc ex:predicate ex:rprotein_class ;\n         ex:arg1 ?V3 ;\n         ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    # proteins(V0):- path(V2,V1), interaction(V2,V0,V3), rprotein_class(V0,V4).\n    ?path ex:predicate ex:path ;\n          ex:arg1 ?V2 ;\n          ex:arg2 ?V1 .\n\n    ?inter ex:predicate ex:interaction ;\n           ex:arg1 ?V2 ;\n           ex:arg2 ?V0 ;\n           ex:arg3 ?V3 .\n\n    ?rpc ex:predicate ex:rprotein_class ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V4 .\n  }\n}\nORDER BY ?V0"
  }

  def zendoSparql(): String = {
    "PREFIX ex: <http://example.org/>\n\nSELECT DISTINCT ?V0\nWHERE {\n  {\n    ?f_medium ex:predicate ex:medium ;\n              ex:arg1 ?V2 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V1 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V1 .\n\n    ?f_upright ex:predicate ex:upright ;\n               ex:arg1 ?V1 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_lhs ex:predicate ex:lhs ;\n           ex:arg1 ?V1 .\n\n    ?f_blue ex:predicate ex:blue ;\n            ex:arg1 ?V1 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V1 ;\n            ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_medium ex:predicate ex:medium ;\n              ex:arg1 ?V2 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_rhs ex:predicate ex:rhs ;\n           ex:arg1 ?V1 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V1 ;\n            ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_medium ex:predicate ex:medium ;\n              ex:arg1 ?V2 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_lhs ex:predicate ex:lhs ;\n           ex:arg1 ?V1 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?f_small ex:predicate ex:small ;\n             ex:arg1 ?V2 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_size1 ex:predicate ex:size ;\n             ex:arg1 ?V1 ;\n             ex:arg2 ?V2 .\n\n    ?f_contact ex:predicate ex:contact ;\n               ex:arg1 ?V1 ;\n               ex:arg2 ?V3 .\n\n    ?f_size2 ex:predicate ex:size ;\n             ex:arg1 ?V3 ;\n             ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_piece1 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V3 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V3 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V3 ;\n            ex:arg2 ?V2 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V3 ;\n              ex:arg2 ?V2 .\n\n    ?f_piece2 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_medium ex:predicate ex:medium ;\n              ex:arg1 ?V1 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V2 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V2 ;\n            ex:arg2 ?V1 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V2 ;\n              ex:arg2 ?V1 .\n\n    ?f_red ex:predicate ex:red ;\n           ex:arg1 ?V2 .\n\n    ?f_strange ex:predicate ex:strange ;\n               ex:arg1 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_small ex:predicate ex:small ;\n             ex:arg1 ?V2 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_strange ex:predicate ex:strange ;\n               ex:arg1 ?V1 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V1 ;\n            ex:arg2 ?V2 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?f_small ex:predicate ex:small ;\n             ex:arg1 ?V2 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_red ex:predicate ex:red ;\n           ex:arg1 ?V1 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V1 ;\n            ex:arg2 ?V2 .\n\n    ?f_upright ex:predicate ex:upright ;\n               ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?f_large ex:predicate ex:large ;\n             ex:arg1 ?V2 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V3 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V3 ;\n              ex:arg2 ?V2 .\n\n    ?f_strange ex:predicate ex:strange ;\n               ex:arg1 ?V3 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V3 ;\n            ex:arg2 ?V1 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V3 ;\n              ex:arg2 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?f_large ex:predicate ex:large ;\n             ex:arg1 ?V4 .\n\n    ?f_piece1 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V3 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V3 ;\n            ex:arg2 ?V4 .\n\n    ?f_piece2 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V1 .\n\n    ?f_contact ex:predicate ex:contact ;\n               ex:arg1 ?V1 ;\n               ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_small ex:predicate ex:small ;\n             ex:arg1 ?V1 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V3 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V3 ;\n              ex:arg2 ?V1 .\n\n    ?f_lhs ex:predicate ex:lhs ;\n           ex:arg1 ?V3 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V3 ;\n            ex:arg2 ?V2 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V3 ;\n              ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_large ex:predicate ex:large ;\n             ex:arg1 ?V2 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_lhs ex:predicate ex:lhs ;\n           ex:arg1 ?V1 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_red ex:predicate ex:red ;\n           ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?f_piece1 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V3 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V3 ;\n            ex:arg2 ?V2 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V3 ;\n              ex:arg2 ?V2 .\n\n    ?f_piece2 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n    ?f_rhs ex:predicate ex:rhs ;\n           ex:arg1 ?V1 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_piece1 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V2 .\n\n    ?f_green1 ex:predicate ex:green ;\n              ex:arg1 ?V2 .\n\n    ?f_lhs ex:predicate ex:lhs ;\n           ex:arg1 ?V2 .\n\n    ?f_piece2 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n    ?f_rhs ex:predicate ex:rhs ;\n           ex:arg1 ?V1 .\n\n    ?f_green2 ex:predicate ex:green ;\n              ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?f_medium ex:predicate ex:medium ;\n              ex:arg1 ?V1 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V2 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V2 ;\n            ex:arg2 ?V1 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V2 ;\n              ex:arg2 ?V1 .\n\n    ?f_blue ex:predicate ex:blue ;\n            ex:arg1 ?V2 .\n\n    ?f_strange ex:predicate ex:strange ;\n               ex:arg1 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_piece1 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V2 .\n\n    ?f_green1 ex:predicate ex:green ;\n              ex:arg1 ?V2 .\n\n    ?f_lhs ex:predicate ex:lhs ;\n           ex:arg1 ?V2 .\n\n    ?f_piece2 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n    ?f_green2 ex:predicate ex:green ;\n              ex:arg1 ?V1 .\n\n    ?f_upright ex:predicate ex:upright ;\n               ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?f_piece1 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V3 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V3 .\n\n    ?f_lhs ex:predicate ex:lhs ;\n           ex:arg1 ?V3 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V3 ;\n              ex:arg2 ?V2 .\n\n    ?f_piece2 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V1 ;\n            ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_medium ex:predicate ex:medium ;\n              ex:arg1 ?V1 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V2 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V2 ;\n            ex:arg2 ?V1 .\n\n    ?f_coord2 ex:predicate ex:coord2 ;\n              ex:arg1 ?V2 ;\n              ex:arg2 ?V1 .\n\n    ?f_red ex:predicate ex:red ;\n           ex:arg1 ?V2 .\n\n    ?f_upright ex:predicate ex:upright ;\n               ex:arg1 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_large ex:predicate ex:large ;\n             ex:arg1 ?V2 .\n\n    ?f_piece ex:predicate ex:piece ;\n             ex:arg1 ?V0 ;\n             ex:arg2 ?V1 .\n\n    ?f_coord1 ex:predicate ex:coord1 ;\n              ex:arg1 ?V1 ;\n              ex:arg2 ?V2 .\n\n    ?f_strange ex:predicate ex:strange ;\n               ex:arg1 ?V1 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V1 .\n\n    ?f_size ex:predicate ex:size ;\n            ex:arg1 ?V1 ;\n            ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?f_piece1 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V2 .\n\n    ?f_red ex:predicate ex:red ;\n           ex:arg1 ?V2 .\n\n    ?f_lhs1 ex:predicate ex:lhs ;\n            ex:arg1 ?V2 .\n\n    ?f_piece2 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n    ?f_lhs2 ex:predicate ex:lhs ;\n            ex:arg1 ?V1 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?f_piece1 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V2 .\n\n    ?f_piece2 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V2 .\n\n    ?f_coord1a ex:predicate ex:coord1 ;\n               ex:arg1 ?V1 ;\n               ex:arg2 ?V3 .\n\n    ?f_coord1b ex:predicate ex:coord1 ;\n               ex:arg1 ?V2 ;\n               ex:arg2 ?V3 .\n\n    ?f_lhs ex:predicate ex:lhs ;\n           ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?f_piece1 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V2 .\n\n    ?f_red ex:predicate ex:red ;\n           ex:arg1 ?V2 .\n\n    ?f_piece2 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V3 .\n\n    ?f_green ex:predicate ex:green ;\n             ex:arg1 ?V3 .\n\n    ?f_piece3 ex:predicate ex:piece ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n    ?f_blue ex:predicate ex:blue ;\n            ex:arg1 ?V1 .\n  }\n}\nORDER BY ?V0".stripMargin
  }


  def webkbSparql(): String = {
    """
      |PREFIX ex: <http://example.org/>
      |
      |SELECT DISTINCT ?V0
      |WHERE {
      |
      |  {
      |    # faculty(V0):-
      |    #   courseprof(V1,V0),
      |    #   project(V4,V0),
      |    #   project(V4,V3),
      |    #   courseta(V2,V3).
      |
      |    ?courseprof1 ex:predicate ex:courseprof ;
      |                 ex:arg1 ?V1 ;
      |                 ex:arg2 ?V0 .
      |
      |    ?project1 ex:predicate ex:project ;
      |              ex:arg1 ?V4 ;
      |              ex:arg2 ?V0 .
      |
      |    ?project2 ex:predicate ex:project ;
      |              ex:arg1 ?V4 ;
      |              ex:arg2 ?V3 .
      |
      |    ?courseta1 ex:predicate ex:courseta ;
      |               ex:arg1 ?V2 ;
      |               ex:arg2 ?V3 .
      |  }
      |
      |  UNION
      |
      |  {
      |    # faculty(V0):-
      |    #   courseprof(V5,V0),
      |    #   courseta(V5,V3),
      |    #   courseta(V4,V3),
      |    #   courseprof(V4,V2),
      |    #   project(V1,V2).
      |
      |    ?courseprof1 ex:predicate ex:courseprof ;
      |                 ex:arg1 ?V5 ;
      |                 ex:arg2 ?V0 .
      |
      |    ?courseta1 ex:predicate ex:courseta ;
      |               ex:arg1 ?V5 ;
      |               ex:arg2 ?V3 .
      |
      |    ?courseta2 ex:predicate ex:courseta ;
      |               ex:arg1 ?V4 ;
      |               ex:arg2 ?V3 .
      |
      |    ?courseprof2 ex:predicate ex:courseprof ;
      |                 ex:arg1 ?V4 ;
      |                 ex:arg2 ?V2 .
      |
      |    ?project1 ex:predicate ex:project ;
      |              ex:arg1 ?V1 ;
      |              ex:arg2 ?V2 .
      |  }
      |}
      |""".stripMargin
  }

  def ptcSparql(): String =
    """
      |PREFIX ex: <http://example.org/>
      |
      |SELECT DISTINCT ?V0
      |WHERE {
      |
      |  {
      |    # label(V0):- zn(V2), atom(V1,V0,V2).
      |    ?znFact ex:predicate ex:zn ;
      |            ex:arg1 ?V2 .
      |
      |    ?atomFact ex:predicate ex:atom ;
      |              ex:arg1 ?V1 ;
      |              ex:arg2 ?V0 ;
      |              ex:arg3 ?V2 .
      |  }
      |
      |  UNION
      |
      |  {
      |    # label(V0):- cu(V2), atom(V1,V0,V2).
      |    ?cuFact ex:predicate ex:cu ;
      |            ex:arg1 ?V2 .
      |
      |    ?atomFact ex:predicate ex:atom ;
      |              ex:arg1 ?V1 ;
      |              ex:arg2 ?V0 ;
      |              ex:arg3 ?V2 .
      |  }
      |
      |  UNION
      |
      |  {
      |    # label(V0):-
      |    #   c(V4),
      |    #   connected(V3,V5,V2),
      |    #   atom(V5,V0,V4),
      |    #   p(V1),
      |    #   atom(V3,V0,V1).
      |
      |    ?cFact ex:predicate ex:c ;
      |           ex:arg1 ?V4 .
      |
      |    ?connFact ex:predicate ex:connected ;
      |              ex:arg1 ?V3 ;
      |              ex:arg2 ?V5 ;
      |              ex:arg3 ?V2 .
      |
      |    ?atomFact1 ex:predicate ex:atom ;
      |               ex:arg1 ?V5 ;
      |               ex:arg2 ?V0 ;
      |               ex:arg3 ?V4 .
      |
      |    ?pFact ex:predicate ex:p ;
      |           ex:arg1 ?V1 .
      |
      |    ?atomFact2 ex:predicate ex:atom ;
      |               ex:arg1 ?V3 ;
      |               ex:arg2 ?V0 ;
      |               ex:arg3 ?V1 .
      |  }
      |
      |  UNION
      |
      |  {
      |    # label(V0):-
      |    #   connected(V3,V5,V2),
      |    #   atom(V5,V0,V4),
      |    #   p(V1),
      |    #   h(V4),
      |    #   atom(V3,V0,V1).
      |
      |    ?connFact ex:predicate ex:connected ;
      |              ex:arg1 ?V3 ;
      |              ex:arg2 ?V5 ;
      |              ex:arg3 ?V2 .
      |
      |    ?atomFact1 ex:predicate ex:atom ;
      |               ex:arg1 ?V5 ;
      |               ex:arg2 ?V0 ;
      |               ex:arg3 ?V4 .
      |
      |    ?pFact ex:predicate ex:p ;
      |           ex:arg1 ?V1 .
      |
      |    ?hFact ex:predicate ex:h ;
      |           ex:arg1 ?V4 .
      |
      |    ?atomFact2 ex:predicate ex:atom ;
      |               ex:arg1 ?V3 ;
      |               ex:arg2 ?V0 ;
      |               ex:arg3 ?V1 .
      |  }
      |}
      |""".stripMargin

  def ptcVirtuoso(graphUri: String): String = {
    s"SPARQL\nPREFIX ex: <http://example.org/>\n\nSELECT DISTINCT ?V0\nFROM <${graphUri}>\nWHERE {\n\n  {\n    # label(V0) :- zn(V2), atom(V1,V0,V2).\n\n    ?znFact ex:predicate ex:zn ;\n            ex:argument ?zn_a1 .\n\n    ?zn_a1 ex:index 1 ;\n           ex:value ?V2 .\n\n    ?atomFact ex:predicate ex:atom ;\n              ex:argument ?atom_a1 ;\n              ex:argument ?atom_a2 ;\n              ex:argument ?atom_a3 .\n\n    ?atom_a1 ex:index 1 ;\n             ex:value ?V1 .\n\n    ?atom_a2 ex:index 2 ;\n             ex:value ?V0 .\n\n    ?atom_a3 ex:index 3 ;\n             ex:value ?V2 .\n  }\n\n  UNION\n\n  {\n    # label(V0) :- cu(V2), atom(V1,V0,V2).\n\n    ?cuFact ex:predicate ex:cu ;\n            ex:argument ?cu_a1 .\n\n    ?cu_a1 ex:index 1 ;\n           ex:value ?V2 .\n\n    ?atomFact ex:predicate ex:atom ;\n              ex:argument ?atom_a1 ;\n              ex:argument ?atom_a2 ;\n              ex:argument ?atom_a3 .\n\n    ?atom_a1 ex:index 1 ;\n             ex:value ?V1 .\n\n    ?atom_a2 ex:index 2 ;\n             ex:value ?V0 .\n\n    ?atom_a3 ex:index 3 ;\n             ex:value ?V2 .\n  }\n\n  UNION\n\n  {\n    # label(V0) :-\n    #   c(V4),\n    #   connected(V3,V5,V2),\n    #   atom(V5,V0,V4),\n    #   p(V1),\n    #   atom(V3,V0,V1).\n\n    ?cFact ex:predicate ex:c ;\n           ex:argument ?c_a1 .\n\n    ?c_a1 ex:index 1 ;\n          ex:value ?V4 .\n\n\n    ?connFact ex:predicate ex:connected ;\n              ex:argument ?conn_a1 ;\n              ex:argument ?conn_a2 ;\n              ex:argument ?conn_a3 .\n\n    ?conn_a1 ex:index 1 ;\n             ex:value ?V3 .\n\n    ?conn_a2 ex:index 2 ;\n             ex:value ?V5 .\n\n    ?conn_a3 ex:index 3 ;\n             ex:value ?V2 .\n\n\n    ?atomFact1 ex:predicate ex:atom ;\n               ex:argument ?atom1_a1 ;\n               ex:argument ?atom1_a2 ;\n               ex:argument ?atom1_a3 .\n\n    ?atom1_a1 ex:index 1 ;\n              ex:value ?V5 .\n\n    ?atom1_a2 ex:index 2 ;\n              ex:value ?V0 .\n\n    ?atom1_a3 ex:index 3 ;\n              ex:value ?V4 .\n\n\n    ?pFact ex:predicate ex:p ;\n           ex:argument ?p_a1 .\n\n    ?p_a1 ex:index 1 ;\n          ex:value ?V1 .\n\n\n    ?atomFact2 ex:predicate ex:atom ;\n               ex:argument ?atom2_a1 ;\n               ex:argument ?atom2_a2 ;\n               ex:argument ?atom2_a3 .\n\n    ?atom2_a1 ex:index 1 ;\n              ex:value ?V3 .\n\n    ?atom2_a2 ex:index 2 ;\n              ex:value ?V0 .\n\n    ?atom2_a3 ex:index 3 ;\n              ex:value ?V1 .\n  }\n\n  UNION\n\n  {\n    # label(V0) :-\n    #   connected(V3,V5,V2),\n    #   atom(V5,V0,V4),\n    #   p(V1),\n    #   h(V4),\n    #   atom(V3,V0,V1).\n\n    ?connFact ex:predicate ex:connected ;\n              ex:argument ?conn_a1 ;\n              ex:argument ?conn_a2 ;\n              ex:argument ?conn_a3 .\n\n    ?conn_a1 ex:index 1 ;\n             ex:value ?V3 .\n\n    ?conn_a2 ex:index 2 ;\n             ex:value ?V5 .\n\n    ?conn_a3 ex:index 3 ;\n             ex:value ?V2 .\n\n\n    ?atomFact1 ex:predicate ex:atom ;\n               ex:argument ?atom1_a1 ;\n               ex:argument ?atom1_a2 ;\n               ex:argument ?atom1_a3 .\n\n    ?atom1_a1 ex:index 1 ;\n              ex:value ?V5 .\n\n    ?atom1_a2 ex:index 2 ;\n              ex:value ?V0 .\n\n    ?atom1_a3 ex:index 3 ;\n              ex:value ?V4 .\n\n\n    ?pFact ex:predicate ex:p ;\n           ex:argument ?p_a1 .\n\n    ?p_a1 ex:index 1 ;\n          ex:value ?V1 .\n\n\n    ?hFact ex:predicate ex:h ;\n           ex:argument ?h_a1 .\n\n    ?h_a1 ex:index 1 ;\n          ex:value ?V4 .\n\n\n    ?atomFact2 ex:predicate ex:atom ;\n               ex:argument ?atom2_a1 ;\n               ex:argument ?atom2_a2 ;\n               ex:argument ?atom2_a3 .\n\n    ?atom2_a1 ex:index 1 ;\n              ex:value ?V3 .\n\n    ?atom2_a2 ex:index 2 ;\n              ex:value ?V0 .\n\n    ?atom2_a3 ex:index 3 ;\n              ex:value ?V1 .\n  }\n}\nORDER BY ?V0"
  }

  def centipenteVirtuoso(graphUri: String): String = {
    s"SPARQL\nPREFIX ex: <http://example.org/>\n\nSELECT DISTINCT ?V0 ?V1 ?V2\nFROM <${graphUri}>\nWHERE {\n\n  {\n    # goal(V0,V1,V2):-\n    #   true_blackPayoff(V0,V2),\n    #   int_15(V2),\n    #   agent_black(V1),\n    #   true_control(V0,V3),\n    #   agent_white(V3).\n\n    ?tbp ex:predicate ex:true_blackPayoff ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V2 .\n\n    ?i15 ex:predicate ex:int_15 ;\n         ex:arg1 ?V2 .\n\n    ?ab ex:predicate ex:agent_black ;\n        ex:arg1 ?V1 .\n\n    ?tc ex:predicate ex:true_control ;\n        ex:arg1 ?V0 ;\n        ex:arg2 ?V3 .\n\n    ?aw ex:predicate ex:agent_white ;\n        ex:arg1 ?V3 .\n  }\n\n  UNION\n\n  {\n    # goal(V0,V1,V2):-\n    #   agent_black(V1),\n    #   true_control(V0,V1),\n    #   true_blackPayoff(V0,V2),\n    #   succ(V2,V3),\n    #   true_whitePayoff(V0,V3).\n\n    ?ab ex:predicate ex:agent_black ;\n        ex:arg1 ?V1 .\n\n    ?tc ex:predicate ex:true_control ;\n        ex:arg1 ?V0 ;\n        ex:arg2 ?V1 .\n\n    ?tbp ex:predicate ex:true_blackPayoff ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V2 .\n\n    ?s ex:predicate ex:succ ;\n       ex:arg1 ?V2 ;\n       ex:arg2 ?V3 .\n\n    ?twp ex:predicate ex:true_whitePayoff ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V3 .\n  }\n\n  UNION\n\n  {\n    # goal(V0,V1,V2):-\n    #   int_0(V2),\n    #   true_control(V0,V1),\n    #   true_blackPayoff(V0,V4),\n    #   succ(V6,V4),\n    #   succ(V3,V6),\n    #   succ(V5,V3).\n\n    ?i0 ex:predicate ex:int_0 ;\n        ex:arg1 ?V2 .\n\n    ?tc ex:predicate ex:true_control ;\n        ex:arg1 ?V0 ;\n        ex:arg2 ?V1 .\n\n    ?tbp ex:predicate ex:true_blackPayoff ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V4 .\n\n    ?s1 ex:predicate ex:succ ;\n        ex:arg1 ?V6 ;\n        ex:arg2 ?V4 .\n\n    ?s2 ex:predicate ex:succ ;\n        ex:arg1 ?V3 ;\n        ex:arg2 ?V6 .\n\n    ?s3 ex:predicate ex:succ ;\n        ex:arg1 ?V5 ;\n        ex:arg2 ?V3 .\n  }\n\n  UNION\n\n  {\n    # goal(V0,V1,V2):-\n    #   int_0(V2),\n    #   role(V1),\n    #   true_control(V0,V3),\n    #   agent_white(V3),\n    #   true_whitePayoff(V0,V4),\n    #   succ(V5,V4).\n\n    ?i0 ex:predicate ex:int_0 ;\n        ex:arg1 ?V2 .\n\n    ?role ex:predicate ex:role ;\n          ex:arg1 ?V1 .\n\n    ?tc ex:predicate ex:true_control ;\n        ex:arg1 ?V0 ;\n        ex:arg2 ?V3 .\n\n    ?aw ex:predicate ex:agent_white ;\n        ex:arg1 ?V3 .\n\n    ?twp ex:predicate ex:true_whitePayoff ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V4 .\n\n    ?s ex:predicate ex:succ ;\n       ex:arg1 ?V5 ;\n       ex:arg2 ?V4 .\n  }\n\n  UNION\n\n  {\n    # goal(V0,V1,V2):-\n    #   int_0(V2),\n    #   agent_white(V1),\n    #   true_blackPayoff(V0,V4),\n    #   succ(V6,V4),\n    #   succ(V3,V6),\n    #   succ(V5,V3).\n\n    ?i0 ex:predicate ex:int_0 ;\n        ex:arg1 ?V2 .\n\n    ?aw ex:predicate ex:agent_white ;\n        ex:arg1 ?V1 .\n\n    ?tbp ex:predicate ex:true_blackPayoff ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V4 .\n\n    ?s1 ex:predicate ex:succ ;\n        ex:arg1 ?V6 ;\n        ex:arg2 ?V4 .\n\n    ?s2 ex:predicate ex:succ ;\n        ex:arg1 ?V3 ;\n        ex:arg2 ?V6 .\n\n    ?s3 ex:predicate ex:succ ;\n        ex:arg1 ?V5 ;\n        ex:arg2 ?V3 .\n  }\n\n  UNION\n\n  {\n    # goal(V0,V1,V2):-\n    #   agent_white(V1),\n    #   true_whitePayoff(V0,V2),\n    #   succ(V3,V2),\n    #   true_blackPayoff(V0,V3),\n    #   agent_black(V4),\n    #   true_control(V0,V4).\n\n    ?aw ex:predicate ex:agent_white ;\n        ex:arg1 ?V1 .\n\n    ?twp ex:predicate ex:true_whitePayoff ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V2 .\n\n    ?s ex:predicate ex:succ ;\n       ex:arg1 ?V3 ;\n       ex:arg2 ?V2 .\n\n    ?tbp ex:predicate ex:true_blackPayoff ;\n         ex:arg1 ?V0 ;\n         ex:arg2 ?V3 .\n\n    ?ab ex:predicate ex:agent_black ;\n        ex:arg1 ?V4 .\n\n    ?tc ex:predicate ex:true_control ;\n        ex:arg1 ?V0 ;\n        ex:arg2 ?V4 .\n  }\n}\nORDER BY ?V0 ?V1 ?V2"
  }

  def zendoVirtuoso(graphUri: String): String = {
    s"SPARQL\nPREFIX ex: <http://example.org/>\n\nSELECT DISTINCT ?V0\nFROM <${graphUri}>\nWHERE {\n\n  {\n    ?m ex:predicate ex:medium ; ex:arg1 ?V2 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?g ex:predicate ex:green ; ex:arg1 ?V1 .\n    ?c2 ex:predicate ex:coord2 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?g ex:predicate ex:green ; ex:arg1 ?V1 .\n    ?u ex:predicate ex:upright ; ex:arg1 ?V1 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?c2 ex:predicate ex:coord2 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?lhs ex:predicate ex:lhs ; ex:arg1 ?V1 .\n    ?b ex:predicate ex:blue ; ex:arg1 ?V1 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?m ex:predicate ex:medium ; ex:arg1 ?V2 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?rhs ex:predicate ex:rhs ; ex:arg1 ?V1 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?m ex:predicate ex:medium ; ex:arg1 ?V2 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?lhs ex:predicate ex:lhs ; ex:arg1 ?V1 .\n    ?g ex:predicate ex:green ; ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?sm ex:predicate ex:small ; ex:arg1 ?V2 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?s1 ex:predicate ex:size ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?ct ex:predicate ex:contact ; ex:arg1 ?V1 ; ex:arg2 ?V3 .\n    ?s2 ex:predicate ex:size ; ex:arg1 ?V3 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?p3 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V3 .\n    ?g3 ex:predicate ex:green ; ex:arg1 ?V3 .\n    ?s3 ex:predicate ex:size ; ex:arg1 ?V3 ; ex:arg2 ?V2 .\n    ?c13 ex:predicate ex:coord1 ; ex:arg1 ?V3 ; ex:arg2 ?V2 .\n    ?p1 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?c21 ex:predicate ex:coord2 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?m ex:predicate ex:medium ; ex:arg1 ?V1 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V2 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V2 ; ex:arg2 ?V1 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V2 ; ex:arg2 ?V1 .\n    ?r ex:predicate ex:red ; ex:arg1 ?V2 .\n    ?st ex:predicate ex:strange ; ex:arg1 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?sm ex:predicate ex:small ; ex:arg1 ?V2 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?st ex:predicate ex:strange ; ex:arg1 ?V1 .\n    ?c2 ex:predicate ex:coord2 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?g ex:predicate ex:green ; ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?sm ex:predicate ex:small ; ex:arg1 ?V2 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?r ex:predicate ex:red ; ex:arg1 ?V1 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?u ex:predicate ex:upright ; ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?lg ex:predicate ex:large ; ex:arg1 ?V2 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V3 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V3 ; ex:arg2 ?V2 .\n    ?st ex:predicate ex:strange ; ex:arg1 ?V3 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V3 ; ex:arg2 ?V1 .\n    ?c2 ex:predicate ex:coord2 ; ex:arg1 ?V3 ; ex:arg2 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?lg ex:predicate ex:large ; ex:arg1 ?V4 .\n    ?p3 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V3 .\n    ?s3 ex:predicate ex:size ; ex:arg1 ?V3 ; ex:arg2 ?V4 .\n    ?p1 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?g1 ex:predicate ex:green ; ex:arg1 ?V1 .\n    ?ct ex:predicate ex:contact ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?sm ex:predicate ex:small ; ex:arg1 ?V1 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V3 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V3 ; ex:arg2 ?V1 .\n    ?lhs ex:predicate ex:lhs ; ex:arg1 ?V3 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V3 ; ex:arg2 ?V2 .\n    ?c2 ex:predicate ex:coord2 ; ex:arg1 ?V3 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?lg ex:predicate ex:large ; ex:arg1 ?V2 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?lhs ex:predicate ex:lhs ; ex:arg1 ?V1 .\n    ?c2 ex:predicate ex:coord2 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?r ex:predicate ex:red ; ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?p3 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V3 .\n    ?s3 ex:predicate ex:size ; ex:arg1 ?V3 ; ex:arg2 ?V2 .\n    ?c13 ex:predicate ex:coord1 ; ex:arg1 ?V3 ; ex:arg2 ?V2 .\n    ?p1 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?rhs ex:predicate ex:rhs ; ex:arg1 ?V1 .\n    ?c21 ex:predicate ex:coord2 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?p2 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V2 .\n    ?g2 ex:predicate ex:green ; ex:arg1 ?V2 .\n    ?lhs2 ex:predicate ex:lhs ; ex:arg1 ?V2 .\n    ?p1 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?rhs1 ex:predicate ex:rhs ; ex:arg1 ?V1 .\n    ?g1 ex:predicate ex:green ; ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?m ex:predicate ex:medium ; ex:arg1 ?V1 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V2 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V2 ; ex:arg2 ?V1 .\n    ?c2 ex:predicate ex:coord2 ; ex:arg1 ?V2 ; ex:arg2 ?V1 .\n    ?b ex:predicate ex:blue ; ex:arg1 ?V2 .\n    ?st ex:predicate ex:strange ; ex:arg1 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?p2 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V2 .\n    ?g2 ex:predicate ex:green ; ex:arg1 ?V2 .\n    ?lhs2 ex:predicate ex:lhs ; ex:arg1 ?V2 .\n    ?p1 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?g1 ex:predicate ex:green ; ex:arg1 ?V1 .\n    ?u1 ex:predicate ex:upright ; ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?p3 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V3 .\n    ?g3 ex:predicate ex:green ; ex:arg1 ?V3 .\n    ?lhs3 ex:predicate ex:lhs ; ex:arg1 ?V3 .\n    ?c13 ex:predicate ex:coord1 ; ex:arg1 ?V3 ; ex:arg2 ?V2 .\n    ?p1 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?s1 ex:predicate ex:size ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?m ex:predicate ex:medium ; ex:arg1 ?V1 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V2 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V2 ; ex:arg2 ?V1 .\n    ?c2 ex:predicate ex:coord2 ; ex:arg1 ?V2 ; ex:arg2 ?V1 .\n    ?r ex:predicate ex:red ; ex:arg1 ?V2 .\n    ?u ex:predicate ex:upright ; ex:arg1 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?lg ex:predicate ex:large ; ex:arg1 ?V2 .\n    ?p ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?c1 ex:predicate ex:coord1 ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n    ?st ex:predicate ex:strange ; ex:arg1 ?V1 .\n    ?g ex:predicate ex:green ; ex:arg1 ?V1 .\n    ?s ex:predicate ex:size ; ex:arg1 ?V1 ; ex:arg2 ?V2 .\n  }\n\n  UNION\n\n  {\n    ?p2 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V2 .\n    ?r2 ex:predicate ex:red ; ex:arg1 ?V2 .\n    ?lhs2 ex:predicate ex:lhs ; ex:arg1 ?V2 .\n    ?p1 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?lhs1 ex:predicate ex:lhs ; ex:arg1 ?V1 .\n    ?g1 ex:predicate ex:green ; ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?p2 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V2 .\n    ?p1 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?g2 ex:predicate ex:green ; ex:arg1 ?V2 .\n    ?c11 ex:predicate ex:coord1 ; ex:arg1 ?V1 ; ex:arg2 ?V3 .\n    ?c12 ex:predicate ex:coord1 ; ex:arg1 ?V2 ; ex:arg2 ?V3 .\n    ?lhs1 ex:predicate ex:lhs ; ex:arg1 ?V1 .\n  }\n\n  UNION\n\n  {\n    ?p2 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V2 .\n    ?r2 ex:predicate ex:red ; ex:arg1 ?V2 .\n    ?p3 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V3 .\n    ?g3 ex:predicate ex:green ; ex:arg1 ?V3 .\n    ?p1 ex:predicate ex:piece ; ex:arg1 ?V0 ; ex:arg2 ?V1 .\n    ?b1 ex:predicate ex:blue ; ex:arg1 ?V1 .\n  }\n}\nORDER BY ?V0"
  }
  def yeastVirtuoso(graphUri: String): String = {
    "SPARQL\n" +
      "PREFIX ex: <http://example.org/>\n\nSELECT DISTINCT ?V0\nWHERE {\n  GRAPH <http://example.org/graph/proteins> {\n    {\n      ?path ex:predicate ex:path ;\n            ex:arg1 ?V0 ;\n            ex:arg2 ?V2 .\n\n      ?location ex:predicate ex:location ;\n                ex:arg1 ?V0 ;\n                ex:arg2 ?V1 .\n    }\n\n    UNION\n\n    {\n      ?enzyme ex:predicate ex:enzyme ;\n              ex:arg1 ?V0 ;\n              ex:arg2 ?V1 .\n\n      ?renzyme ex:predicate ex:renzyme ;\n               ex:arg1 ?V0 ;\n               ex:arg2 ?V1 .\n    }\n\n    UNION\n\n    {\n      ?path ex:predicate ex:path ;\n            ex:arg1 ?V2 ;\n            ex:arg2 ?V3 .\n\n      ?interaction ex:predicate ex:interaction ;\n                   ex:arg1 ?V3 ;\n                   ex:arg2 ?V0 ;\n                   ex:arg3 ?V1 .\n    }\n\n    UNION\n\n    {\n      ?protein_class ex:predicate ex:protein_class ;\n                     ex:arg1 ?V0 ;\n                     ex:arg2 ?V1 .\n\n      ?rprotein_class ex:predicate ex:rprotein_class ;\n                      ex:arg1 ?V0 ;\n                      ex:arg2 ?V1 .\n    }\n\n    UNION\n\n    {\n      ?protein_class ex:predicate ex:protein_class ;\n                     ex:arg1 ?V0 ;\n                     ex:arg2 ?V4 .\n\n      ?interaction ex:predicate ex:interaction ;\n                   ex:arg1 ?V2 ;\n                   ex:arg2 ?V0 ;\n                   ex:arg3 ?V1 .\n\n      ?rprotein_class ex:predicate ex:rprotein_class ;\n                      ex:arg1 ?V3 ;\n                      ex:arg2 ?V4 .\n    }\n\n    UNION\n\n    {\n      ?phenotype ex:predicate ex:phenotype ;\n                 ex:arg1 ?V0 ;\n                 ex:arg2 ?V3 .\n\n      ?renzyme ex:predicate ex:renzyme ;\n               ex:arg1 ?V0 ;\n               ex:arg2 ?V2 .\n\n      ?rphenotype ex:predicate ex:rphenotype ;\n                  ex:arg1 ?V1 ;\n                  ex:arg2 ?V3 .\n    }\n\n    UNION\n\n    {\n      ?protein_class ex:predicate ex:protein_class ;\n                     ex:arg1 ?V0 ;\n                     ex:arg2 ?V3 .\n\n      ?rprotein_class ex:predicate ex:rprotein_class ;\n                      ex:arg1 ?V2 ;\n                      ex:arg2 ?V3 .\n\n      ?enzyme ex:predicate ex:enzyme ;\n              ex:arg1 ?V2 ;\n              ex:arg2 ?V1 .\n    }\n\n    UNION\n\n    {\n      ?interaction ex:predicate ex:interaction ;\n                   ex:arg1 ?V3 ;\n                   ex:arg2 ?V0 ;\n                   ex:arg3 ?V1 .\n\n      ?protein_class ex:predicate ex:protein_class ;\n                     ex:arg1 ?V3 ;\n                     ex:arg2 ?V2 .\n\n      ?rprotein_class ex:predicate ex:rprotein_class ;\n                      ex:arg1 ?V3 ;\n                      ex:arg2 ?V2 .\n    }\n\n    UNION\n\n    {\n      ?path ex:predicate ex:path ;\n            ex:arg1 ?V2 ;\n            ex:arg2 ?V1 .\n\n      ?interaction ex:predicate ex:interaction ;\n                   ex:arg1 ?V2 ;\n                   ex:arg2 ?V0 ;\n                   ex:arg3 ?V3 .\n\n      ?rprotein_class ex:predicate ex:rprotein_class ;\n                      ex:arg1 ?V0 ;\n                      ex:arg2 ?V4 .\n    }\n  }\n}\nORDER BY ?V0"
  }

  def pteVirtuoso(graphUri: String): String = {
    s"""SPARQL\nPREFIX ex: <http://example.org/>

    SELECT DISTINCT ?V0
      FROM <${graphUri}>
    WHERE {

      {
        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V1 ;
        ex:arg3 ?V2 ;
        ex:arg4 ?V4 ;
        ex:arg5 ?V3 .

        ?phenol ex:predicate ex:pte_phenol ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .

        ?ketone ex:predicate ex:pte_ketone ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .
      }

      UNION

      {
        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V1 ;
        ex:arg3 ?V2 ;
        ex:arg4 ?V4 ;
        ex:arg5 ?V3 .

        ?nitro ex:predicate ex:pte_nitro ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .

        ?ring5 ex:predicate ex:pte_non_ar_hetero_5_ring ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .
      }

      UNION

      {
        ?alkyl_halide ex:predicate ex:pte_alkyl_halide ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .

        ?methyl ex:predicate ex:pte_methyl ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V1 ;
        ex:arg3 ?V2 ;
        ex:arg4 ?V4 ;
        ex:arg5 ?V3 .
      }

      UNION

      {
        ?alcohol ex:predicate ex:pte_alcohol ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .

        ?ester ex:predicate ex:pte_ester ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V1 ;
        ex:arg3 ?V2 ;
        ex:arg4 ?V4 ;
        ex:arg5 ?V3 .
      }

      UNION

      {
        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V1 ;
        ex:arg3 ?V2 ;
        ex:arg4 ?V4 ;
        ex:arg5 ?V3 .

        ?imine ex:predicate ex:pte_imine ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .

        ?ames ex:predicate ex:pte_ames ;
        ex:arg1 ?V5 .
      }

      UNION

      {
        ?sulfide ex:predicate ex:pte_sulfide ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .

        ?alkyl_halide ex:predicate ex:pte_alkyl_halide ;
        ex:arg1 ?V5 ;
        ex:arg2 ?V1 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V1 ;
        ex:arg3 ?V2 ;
        ex:arg4 ?V4 ;
        ex:arg5 ?V3 .
      }

      UNION

      {
        ?methyl ex:predicate ex:pte_methyl ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?five_ring ex:predicate ex:pte_five_ring ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?ames ex:predicate ex:pte_ames ;
        ex:arg1 ?V1 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V3 ;
        ex:arg3 ?V4 ;
        ex:arg4 ?V2 ;
        ex:arg5 ?V5 .
      }

      UNION

      {
        ?sulfo ex:predicate ex:pte_sulfo ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?ames ex:predicate ex:pte_ames ;
        ex:arg1 ?V1 .

        ?mutagenic ex:predicate ex:pte_mutagenic ;
        ex:arg1 ?V1 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V3 ;
        ex:arg3 ?V4 ;
        ex:arg4 ?V2 ;
        ex:arg5 ?V5 .
      }

      UNION

      {
        ?six_ring ex:predicate ex:pte_six_ring ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?ames ex:predicate ex:pte_ames ;
        ex:arg1 ?V1 .

        ?ester ex:predicate ex:pte_ester ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V3 ;
        ex:arg3 ?V4 ;
        ex:arg4 ?V2 ;
        ex:arg5 ?V5 .
      }

      UNION

      {
        ?ether ex:predicate ex:pte_ether ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?phenol ex:predicate ex:pte_phenol ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?ames ex:predicate ex:pte_ames ;
        ex:arg1 ?V1 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V3 ;
        ex:arg3 ?V4 ;
        ex:arg4 ?V2 ;
        ex:arg5 ?V5 .
      }

      UNION

      {
        ?ring6 ex:predicate ex:pte_non_ar_hetero_6_ring ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?ames ex:predicate ex:pte_ames ;
        ex:arg1 ?V1 .

        ?amine ex:predicate ex:pte_amine ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V3 ;
        ex:arg3 ?V4 ;
        ex:arg4 ?V2 ;
        ex:arg5 ?V5 .
      }

      UNION

      {
        ?ketone ex:predicate ex:pte_ketone ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?mutagenic ex:predicate ex:pte_mutagenic ;
        ex:arg1 ?V1 .

        ?methoxy ex:predicate ex:pte_methoxy ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V3 ;
        ex:arg3 ?V4 ;
        ex:arg4 ?V2 ;
        ex:arg5 ?V5 .
      }

      UNION

      {
        ?ames ex:predicate ex:pte_ames ;
        ex:arg1 ?V1 .

        ?amine ex:predicate ex:pte_amine ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?atm ex:predicate ex:pte_atm ;
        ex:arg1 ?V0 ;
        ex:arg2 ?V3 ;
        ex:arg3 ?V4 ;
        ex:arg4 ?V2 ;
        ex:arg5 ?V5 .

        ?methyl ex:predicate ex:pte_methyl ;
        ex:arg1 ?V1 ;
        ex:arg2 ?V3 .

        ?mutagenic ex:predicate ex:pte_mutagenic ;
        ex:arg1 ?V1 .
      }
    }
    ORDER BY ?V0"""
  }

  def webkbVirtuoso(graphUri: String): String = {
    s"""
       |SPARQL
       |PREFIX ex: <http://example.org/>
       |
       |SELECT DISTINCT ?V0
       |FROM <$graphUri>
       |WHERE {
       |
       |  {
       |    ?cp1 ex:predicate ex:courseprof ;
       |         ex:argument ?cp1_a1 ;
       |         ex:argument ?cp1_a2 .
       |
       |    ?cp1_a1 ex:index 1 ;
       |             ex:value ?V1 .
       |
       |    ?cp1_a2 ex:index 2 ;
       |             ex:value ?V0 .
       |
       |    ?pr1 ex:predicate ex:project ;
       |         ex:argument ?pr1_a1 ;
       |         ex:argument ?pr1_a2 .
       |
       |    ?pr1_a1 ex:index 1 ;
       |             ex:value ?V4 .
       |
       |    ?pr1_a2 ex:index 2 ;
       |             ex:value ?V0 .
       |
       |    ?pr2 ex:predicate ex:project ;
       |         ex:argument ?pr2_a1 ;
       |         ex:argument ?pr2_a2 .
       |
       |    ?pr2_a1 ex:index 1 ;
       |             ex:value ?V4 .
       |
       |    ?pr2_a2 ex:index 2 ;
       |             ex:value ?V3 .
       |
       |    ?ta1 ex:predicate ex:courseta ;
       |         ex:argument ?ta1_a1 ;
       |         ex:argument ?ta1_a2 .
       |
       |    ?ta1_a1 ex:index 1 ;
       |             ex:value ?V2 .
       |
       |    ?ta1_a2 ex:index 2 ;
       |             ex:value ?V3 .
       |  }
       |
       |  UNION
       |
       |  {
       |    ?cp1 ex:predicate ex:courseprof ;
       |         ex:argument ?cp1_a1 ;
       |         ex:argument ?cp1_a2 .
       |
       |    ?cp1_a1 ex:index 1 ;
       |             ex:value ?V5 .
       |
       |    ?cp1_a2 ex:index 2 ;
       |             ex:value ?V0 .
       |
       |    ?ta1 ex:predicate ex:courseta ;
       |         ex:argument ?ta1_a1 ;
       |         ex:argument ?ta1_a2 .
       |
       |    ?ta1_a1 ex:index 1 ;
       |             ex:value ?V5 .
       |
       |    ?ta1_a2 ex:index 2 ;
       |             ex:value ?V3 .
       |
       |    ?ta2 ex:predicate ex:courseta ;
       |         ex:argument ?ta2_a1 ;
       |         ex:argument ?ta2_a2 .
       |
       |    ?ta2_a1 ex:index 1 ;
       |             ex:value ?V4 .
       |
       |    ?ta2_a2 ex:index 2 ;
       |             ex:value ?V3 .
       |
       |    ?cp2 ex:predicate ex:courseprof ;
       |         ex:argument ?cp2_a1 ;
       |         ex:argument ?cp2_a2 .
       |
       |    ?cp2_a1 ex:index 1 ;
       |             ex:value ?V4 .
       |
       |    ?cp2_a2 ex:index 2 ;
       |             ex:value ?V2 .
       |
       |    ?pr1 ex:predicate ex:project ;
       |         ex:argument ?pr1_a1 ;
       |         ex:argument ?pr1_a2 .
       |
       |    ?pr1_a1 ex:index 1 ;
       |             ex:value ?V1 .
       |
       |    ?pr1_a2 ex:index 2 ;
       |             ex:value ?V2 .
       |  }
       |}
       |ORDER BY ?V0
       |""".stripMargin
  }

}
