#
#library(rstatic)
#f = function(n) {
#   total = 0
#   for(i in 1:n)
#      total = total + 1L
#
#   total
#}
#
#g = function(n) {
#   total = 0
#   for(i in 2:n)
#      total = total + 1L
#
#   total
#}
#
#h = function(n) {
#   total = 0
#   for(i in n:1)
#      total = total + 1L
#
#   total
#}
#
#m = function(n) {
#   total = 0
#   for(i in 1:n)
#      for(j in 1:i)
#         total = total + 1L
#
#   total
#}
#
#
#af = to_ast(f)
#n = af$copy()
#invisible(astTraverse(n, rewriteFor))
#
