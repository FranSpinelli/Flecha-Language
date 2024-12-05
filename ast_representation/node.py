import sys, os
sys.path.insert(0,"../..")

class Node:

	def __init__(self,typeNode,children=None,leaf=None):
	  	self.typeNode = typeNode
	  	if children:
	  		self.children = children
	  	else:
	  		self.children = [ ]
	  		self.leaf = leaf

	def __str__(self, level=0):
		ret = " "*level+ "[\n" +str(self.typeNode)
		ret = ret + ", " + self.leaf if self.children == [] else ret
		if self.children:
			ret += self.children[0].__str__(level+1)
			for child in self.children[1:]:
				ret += ",\n" + child.__str__(level+1)
			ret += " "*level+ "\n]"
		return ret