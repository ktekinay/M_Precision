#tag Class
Protected Class QuadTests
Inherits TestGroup
	#tag Method, Flags = &h0
		Sub ToAndFromDoubleTest()
		  dim doubles() as double = array( _
		  7.0, _
		  1.1, _
		  0.012, _
		  3.1415, _
		  9887334556.234 _
		  )
		  
		  for each d as double in doubles
		    dim mult as double = 1.0
		    for i as integer = 1 to 2
		      dim d1 as double = d * mult
		      dim q as Quad_MTC = d1
		      dim d2 as double = q
		      Assert.AreEqual d1, d2
		      
		      mult = 0.0 - mult
		    next
		  next
		End Sub
	#tag EndMethod


End Class
#tag EndClass
