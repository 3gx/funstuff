type Name = String
type Act = String

data Proc = Con Name
          | Pre Act Proc
          | Cho [Proc]
          | Par Proc Proc
          | Res Proc Act
          | Rel Proc (Act -> Act)

data Tree = Node [(Act, Tree)]
