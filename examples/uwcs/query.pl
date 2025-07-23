advisedBy(V0,V1):- ta(V3,V0,V4),taughtBy(V3,V1,V4),taughtBy(V5,V0,V2).
advisedBy(V0,V1):- student(V0),tempAdvisedBy(V3,V1),taughtBy(V4,V0,V5),taughtBy(V4,V1,V2).
advisedBy(V0,V1):- ta(V5,V0,V3),taughtBy(V5,V1,V3),tempAdvisedBy(V4,V1),publication(V2,V0),publication(V2,V1),publication(V2,V4).
