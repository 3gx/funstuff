data StackOps a = Pop (StackOps a) | Push a (StackOps a) | End

v :: StackOps Integer
v = Push 1 $ Pop $ Push 3 $ Push 7$ Pop $ Push 2 $ End

interpret :: StackOps a -> [a] -> [a]
interpret (Pop (Push _ ops))  stack = interpret ops stack
interpret (Pop ops)           stack = tail $ interpret ops stack
interpret (Push e ops)        stack = e : interpret ops stack
interpret End                 stack = stack

stack = [10,20,13,23,44]

main = do
  print stack
  print $ interpret v stack
