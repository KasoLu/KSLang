# module name { var1, var2, var3 }
# module name { ... }
# import name { var1, var2, var3 }
# import name { ... }

let def-var = func (param-var1, param-var2, param-var3) {
  # comment
  let var0 = nil, var1 = expr1, var2 = var1, var3 = 1, var4 = true, var5 = false
  # let var6 = "string"
  # let var6 = add(1, 2), var7 = mul(1, 2), var8 = sub(1, 2), var9 = div(1, 2)
  # let var10 = and?(true, false), var11 = or?(true, false), var12 = not?(true)
  # let var13 = eq?(true, true), var14 = gt?(var1, var2), var15 = ge?, var16 = le?, var17 = lt?
  # let var18 = pipe { add(1, 2) => mul($, 2) => sub(2, $) => div($) }
  # let curry-func = curry { add($1, $0) }
  # let var20 = func (pv1, ~pv2, *pv3) 10

  func-expr(param-var1 = param-val1, param-var2, var3, var4, var5)

  cond {
    cond-expr -> {
      expr-true
    }
    else -> {
      expr-else
    }
  }

  # var10 = false

  # Dict { key1 = val1, key2 = val2 }
  # let var20 = List { val1, val2, val3 }
  # null?(var20)
  # head(var20)
  # rest(var20)
  # Set { val1, val2, val3 }
  
  # struct Name { field1, field2, field3 }
  # let name = Name { field1 = val1, field2 = val2, field3 = val3 }
  # name.field1 = val10

  # match expr {
  #   Number [var] -> expr
  #   Bool [var] -> expr
  #   Name [field1, field2, field3] -> expr
  #   else -> expr
  # }
}


