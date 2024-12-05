import sys, os
sys.path.insert(0,"../..")

from .node import Node

class LambdaExpression(Node):
	def __init__(self, children=[], leaf=None):
		count_params = len(children[0])
		if count_params > 1:
			next_param = children[0][0]
			children[0] = children[0][1:]
			Node.__init__(self, 'ExprLambda', [next_param, LambdaExpression(children=children)], leaf)
		elif count_params == 1:
			Node.__init__(self, 'ExprLambda', [children[0][0], children[1]], leaf)

	def __str__(self, level=0):
		ret = " "*level+ "[\"" +str(self.typeNode)+"\", \""+self.children[0]+"\",\n"
		ret += self.children[1].__str__(level+1) + "\n"
		ret += " "*level+ "]"
		return ret