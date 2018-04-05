#tag Class
Protected Class QuadTests
Inherits TestGroup
	#tag Method, Flags = &h0
		Sub AddTest()
		  dim doubles1() as double = array( 1.0, 2.5, 476.33, 0.0 )
		  dim doubles2() as double = array( 3.0, 8.9, 576.98, 0.0 )
		  
		  for i1 as integer = 0 to doubles1.Ubound
		    dim mult as double = 1.0
		    
		    for inner as integer = 1 to 2
		      
		      dim d1 as double = doubles1( i1 ) * mult
		      dim q1 as Quad_MTC = d1
		      
		      for i2 as integer = 0 to doubles2.Ubound
		        
		        dim d2 as double = doubles2( i2 ) * mult
		        dim q2 as Quad_MTC = d2
		        
		        dim expected as double = d1 + d2
		        dim qSum as Quad_MTC = q1 + q2
		        dim actual as double = qSum
		        
		        Assert.AreEqual expected, actual
		      next
		      
		      mult = 0.0 - mult
		    next
		    
		  next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CompareTest()
		  dim q1 as Quad_MTC
		  dim q2 as Quad_MTC
		  
		  q1 = 0.0
		  q2 = 0.0
		  
		  Assert.IsTrue q1 = q2, "Both zero"
		  Assert.IsFalse q1 < q2, "Both zero 2"
		  
		  q1 = 0.0
		  q2 = 1.0
		  Assert.IsTrue q1 < q2, "0 < 1"
		  Assert.IsFalse q1 > q2, "0 is not > 1"
		  Assert.IsFalse q1 = q2, "0 ≠ 1"
		  Assert.IsFalse q2 < q1, "1 is not < 0"
		  Assert.IsTrue q2 > q1, "1 > 0"
		  Assert.IsFalse q2 = q1, "1 ≠ 0"
		  
		  q1 = 0.0
		  q2 = -1.0
		  Assert.IsFalse q1 < q2, "0 is not < -1"
		  Assert.IsTrue q1 > q2, "0 > -1"
		  Assert.IsFalse q1 = q2, "0 ≠ -1"
		  Assert.IsTrue q2 < q1, "-1 < 0"
		  Assert.IsFalse q2 > q1, "-1 is not > 0"
		  Assert.IsFalse q2 = q1, "-1 ≠ 0"
		  
		  q1 = 2.5
		  q2 = 3.0
		  Assert.IsTrue q1 < q2, "2.5 < 3"
		  Assert.IsFalse q1 > q2, "2.5 is not > 3"
		  Assert.IsFalse q1 = q2, "2.5 ≠ 3"
		  Assert.IsFalse q2 < q1, "3 is not < 2.5"
		  Assert.IsTrue q2 > q1, "3 > 2.5"
		  Assert.IsFalse q2 = q1, "3 ≠ 2.5"
		  
		  q1 = -2.5
		  q2 = -3.0
		  Assert.IsFalse q1 < q2, "-2.5 is not < -3"
		  Assert.IsTrue q1 > q2, "-2.5 > -3"
		  Assert.IsFalse q1 = q2, "-2.5 ≠ -3"
		  Assert.IsTrue q2 < q1, "-3 < -2.5"
		  Assert.IsFalse q2 > q1, "-3 is not > -2.5"
		  Assert.IsFalse q2 = q1, "-3 ≠ -2.5"
		  
		  q1 = -2.5
		  q2 = 3.0
		  Assert.IsTrue q1 < q2, "-2.5 < 3"
		  Assert.IsFalse q1 > q2, "-2.5 is not > 3"
		  Assert.IsFalse q1 = q2, "-2.5 ≠ 3"
		  Assert.IsFalse q2 < q1, "3 is not < -2.5"
		  Assert.IsTrue q2 > q1, "3 > -2.5"
		  Assert.IsFalse q2 = q1, "3 ≠ -2.5"
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MultiplyTest()
		  dim doubles1() as double = array( 3855.0, 3.0, 8.9, 576.98, 0.0 )
		  dim doubles2() as double = array( 1017.0, 1.0, 2.5, 476.33, 0.0 )
		  
		  for i1 as integer = 0 to doubles1.Ubound
		    dim mult as double = 1.0
		    
		    for inner as integer = 1 to 2
		      
		      dim d1 as double = doubles1( i1 ) * mult
		      dim q1 as Quad_MTC = d1
		      
		      for i2 as integer = 0 to doubles2.Ubound
		        
		        dim d2 as double = doubles2( i2 ) * mult
		        dim q2 as Quad_MTC = d2
		        
		        #if DebugBuild then
		          if d1 = 0.0 and d2 = -2.5 then
		            d1 = d1 // A place to break
		          end if
		        #endif
		        
		        dim expected as double = d1 * d2
		        dim qSum as Quad_MTC = q1 * q2
		        dim actual as double = qSum
		        
		        Assert.AreEqual expected, actual, 2, d1.ToText + " * " + d2.ToText
		      next
		      
		      mult = 0.0 - mult
		    next
		    
		  next
		  
		  return
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub NegateTest()
		  dim d as double
		  
		  dim q as Quad_MTC
		  
		  d = 2.0
		  q = d
		  d = q
		  Assert.AreEqual 2.0, d
		  
		  q = -q
		  d = q
		  Assert.AreEqual -2.0, d
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SubtractTest()
		  dim doubles1() as double = array( 3855.0, 3.0, 8.9, 576.98, 0.0 )
		  dim doubles2() as double = array( 1017.0, 1.0, 2.5, 476.33, 0.0 )
		  
		  for i1 as integer = 0 to doubles1.Ubound
		    dim mult as double = 1.0
		    
		    for inner as integer = 1 to 2
		      
		      dim d1 as double = doubles1( i1 ) * mult
		      dim q1 as Quad_MTC = d1
		      
		      for i2 as integer = 0 to doubles2.Ubound
		        
		        dim d2 as double = doubles2( i2 ) * mult
		        dim q2 as Quad_MTC = d2
		        
		        #if DebugBuild then
		          if d1 = 8.9 and d2 = 1.0 then
		            d1 = d1 // A place to break
		          end if
		        #endif
		        
		        dim expected as double = d1 - d2
		        dim qSum as Quad_MTC = q1 - q2
		        dim actual as double = qSum
		        
		        Assert.AreEqual expected, actual, 2, d1.ToText + " - " + d2.ToText
		      next
		      
		      mult = 0.0 - mult
		    next
		    
		  next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ToAndFromDoubleTest()
		  dim doubles() as double = array( _
		  val( "NaN"), _
		  val( "INF" ), _
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
		      Assert.IsTrue d1.Equals( d2 ), d1.ToText + "=" + d2.ToText
		      
		      mult = 0.0 - mult
		    next
		  next
		End Sub
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="Duration"
			Group="Behavior"
			Type="Double"
		#tag EndViewProperty
		#tag ViewProperty
			Name="FailedTestCount"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="IncludeGroup"
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="IsRunning"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="NotImplementedCount"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="PassedTestCount"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="RunTestCount"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="SkippedTestCount"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="StopTestOnFail"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="TestCount"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
