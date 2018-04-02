#tag Class
Class Quad_MTC
	#tag Method, Flags = &h0
		Sub Constructor()
		  Data.Exp = &b1011111111111111
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub Constructor(useStruct As QuadStruct)
		  Data = useStruct
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function DoAddition(toQuad As Quad_MTC) As Quad_MTC
		  dim higher as Quad_MTC
		  dim lower as Quad_MTC
		  
		  if self.TrueExponent >= toQuad.TrueExponent then
		    higher = self
		    lower = toQuad
		  else
		    higher = toQuad
		    lower = self
		  end if
		  
		  dim shift as Int16 = higher.TrueExponent - lower.TrueExponent
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function DoSubtraction(baseQuad As Quad_MTC, minusQuad As Quad_MTC) As Quad_MTC
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub FromDouble(d As Double)
		  dim mb as new MemoryBlock( 8 )
		  mb.LittleEndian = false
		  mb.DoubleValue( 0 ) = d
		  
		  dim exp as Int16 = mb.Int16Value( 0 ) and &b1111111111110000
		  dim sign as Int16 = exp and &b1000000000000000
		  exp = exp and &b0111111111111111
		  exp = exp \ CType( 2 ^ 4, UInt16 )
		  exp = exp - 1023 + kBias
		  
		  Data.Exp = sign or exp
		  
		  mb.UInt64Value( 0 ) = mb.UInt64Value( 0 ) * CType( 2 ^ 12, UInt64 )
		  
		  for i as integer = 0 to 3
		    Data.Nums( i ) = mb.UInt16Value( i * 2 )
		  next
		  
		  return
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Add(toQuad As Quad_MTC) As Quad_MTC
		  if toQuad.IsNegative = IsNegative then
		    return DoAddition( toQuad )
		  elseif toQuad.IsNegative then
		    return DoSubtraction( self, toQuad )
		  else
		    return DoSubtraction( toQuad, self )
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Convert() As Double
		  return ToDouble
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Operator_Convert(d As Double)
		  FromDouble d
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function ToDouble() As Double
		  dim mb as new MemoryBlock( 8 )
		  mb.LittleEndian = false
		  
		  for i as integer = 0 to 3
		    mb.UInt16Value( i * 2 ) = Data.Nums( i )
		  next
		  mb.UInt64Value( 0 ) = mb.UInt64Value( 0 ) \ CType( 2 ^ 12, UInt64 )
		  
		  dim exp as Int16 = Data.Exp
		  dim sign as Int16 = exp and &b1000000000000000
		  exp = exp and &b0111111111111111
		  exp = exp - kBias + 1023
		  exp = exp * CType( 2 ^ 4, Int16 )
		  exp = exp or sign
		  exp = exp or mb.Int16Value( 0 )
		  mb.Int16Value( 0 ) = exp
		  
		  return mb.DoubleValue( 0 )
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private Data As QuadStruct
	#tag EndProperty

	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  return ( Data.Exp and &b1000000000000000 ) <> 0
			End Get
		#tag EndGetter
		Private IsNegative As Boolean
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  return ( Data.Exp and &b0111111111111111 ) - kBias
			  
			  
			End Get
		#tag EndGetter
		Private TrueExponent As Int16
	#tag EndComputedProperty


	#tag Constant, Name = kBias, Type = Double, Dynamic = False, Default = \"16383", Scope = Private
	#tag EndConstant


	#tag Structure, Name = QuadStruct, Flags = &h21
		Exp As Int16
		Nums(6) As UInt16
	#tag EndStructure


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
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
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
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
