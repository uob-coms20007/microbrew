
Normal forms

  $ echo "Z()" | microbrew
  [µBrew]
  Z()
  

  $ echo "S(Z())" | microbrew
  [µBrew]
  S(Z())
  

Lexical errors

  $ echo "Z();" | microbrew
  [µBrew]
  LEX ERROR: Expected valid character but found ;.
  

Parse errors

  $ echo "Add(Z,x)" | microbrew
  [µBrew]
  PARSE ERROR: Expected ( but got ,.
  

Naturals

  $ microbrew < nat.br
  [µBrew]
  S(S(S(S(S(Z())))))
  S(S(S(S(S(S(Z()))))))
  S(S(S(S(S(S(S(S(Z()))))))))
  

Booleans

  $ microbrew < bool.br
  [µBrew]
  ItemA()
  

Lists

  $ microbrew < list.br
  [µBrew]
  ItemE()
  S(S(S(S(S(S(S(Z())))))))
  
