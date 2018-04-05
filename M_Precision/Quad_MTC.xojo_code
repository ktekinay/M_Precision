#tag Class
Class Quad_MTC
	#tag Method, Flags = &h0
		Function Abs() As Quad_MTC
		  if IsNegative then
		    return Operator_Negate
		  else
		    return self
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub Constructor()
		  //
		  // Have to assign a value
		  //
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub Constructor(useStruct As QuadStruct)
		  Data = useStruct
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function DoAddition(toQuad As Quad_MTC) As Quad_MTC
		  //
		  // Assumes signs are the same
		  //
		  
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
		  if shift >= 112 then
		    //
		    // No point in adding since we can't hold it anyway
		    //
		    return higher
		  end if
		  
		  dim mbSum as MemoryBlock = ToMemoryBlock( lower, true )
		  ShiftRight mbSum, shift
		  
		  dim mbOther as MemoryBlock = ToMemoryBlock( higher, true )
		  
		  dim carry as UInt32
		  for byteIndex as integer = 14 downto 0 step 2
		    dim v1 as UInt32 = mbSum.UInt16Value( byteIndex )
		    dim v2 as UInt32 = mbOther.UInt16Value( byteIndex )
		    dim newValue as UInt32 = v1 + v2 + carry
		    carry = ( newValue and CType( &hFFFF0000, UInt32 ) ) \ CType( 2 ^ 16, UInt32 )
		    newValue = newValue and CType( &h0000FFFF, UInt32 )
		    mbSum.UInt16Value( byteIndex ) = newValue
		  next
		  
		  dim expValue as Int16 = mbSum.Int16Value( 0 )
		  if expValue = 2 then
		    ShiftRight mbSum, 1
		    expValue = higher.TrueExponent + 1
		  else
		    expValue = higher.TrueExponent
		  end if
		  
		  dim sumStruct as QuadStruct
		  sumStruct.StringValue( mbSum.LittleEndian ) = mbSum
		  
		  dim sum as new Quad_MTC( sumStruct )
		  sum.TrueExponent = expValue
		  sum.IsNegative = self.IsNegative
		  
		  return sum
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function DoMultiplication(other As Quad_MTC) As Quad_MTC
		  //
		  // Both are good values, but might be negative
		  //
		  
		  dim makeNegative as boolean = self.IsNegative xor other.IsNegative
		  
		  dim higher as Quad_MTC
		  dim lower as Quad_MTC
		  
		  if self >= other then
		    higher = self
		    lower = other
		  else
		    higher = other
		    lower = self
		  end if
		  
		  dim mbHigher as MemoryBlock = ToMemoryBlock( higher, true )
		  
		  dim shift as Int16 = higher.TrueExponent - lower.TrueExponent
		  if shift >= 112 then
		    //
		    // No point in adding since we can't hold it anyway
		    //
		    return higher
		  end if
		  
		  dim mbLower as MemoryBlock = ToMemoryBlock( lower, true )
		  ShiftRight mbLower, shift
		  
		  dim mbResult as new MemoryBlock( mbHigher.Size )
		  
		  for lowerIndex as integer = 14 downto 0 step 2
		    dim lowerValue as integer = mbLower.UInt16Value( lowerIndex )
		    
		    dim carry as integer
		    
		    for higherIndex as integer = lowerIndex downto 0 step 2
		      dim resultValue as integer = mbResult.UInt16Value( higherIndex )
		      
		      dim higherValue as integer = mbHigher.UInt16Value( higherIndex )
		      resultValue = higherValue * lowerValue + resultValue
		      resultValue = resultValue + carry
		      
		      carry = resultValue and &hFFFF0000 \ CType( 2 ^ 16, integer )
		      resultValue = resultValue and &h0000FFFF
		      mbResult.UInt16Value( higherIndex ) = resultValue
		    next
		  next
		  
		  dim exp as integer = ShiftToExponent( mbResult, higher.TrueExponent )
		  dim resultStruct as QuadStruct
		  resultStruct.StringValue( mbResult.LittleEndian ) = mbResult
		  
		  dim result as new Quad_MTC( resultStruct )
		  result.TrueExponent = exp
		  result.IsNegative = makeNegative
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function DoSubtraction(minusQuad As Quad_MTC) As Quad_MTC
		  //
		  // Expects two positive values, neither being Zero, INF, or NAN
		  // However, minusQuad might be a larger value
		  //
		  
		  dim higher as Quad_MTC
		  dim lower as Quad_MTC
		  dim doNegate as boolean
		  
		  if self = minusQuad then
		    return 0.0
		  elseif self < minusQuad then
		    higher = minusQuad
		    lower = self
		    doNegate = true
		  else 
		    higher = self
		    lower = minusQuad
		  end if
		  
		  dim shift as Int16 = higher.TrueExponent - lower.TrueExponent
		  if shift >= 112 then
		    //
		    // No point in subtracting since we can't hold it anyway
		    //
		    if doNegate then
		      return -higher
		    else
		      return higher
		    end if
		  end if
		  
		  dim mbResult as MemoryBlock = ToMemoryBlock( higher, true )
		  
		  dim mbOther as MemoryBlock = ToMemoryBlock( lower, true )
		  ShiftRight mbOther, shift
		  
		  dim carry as integer
		  
		  for byteIndex as integer = 14 downto 0 step 2
		    dim v1 as integer = mbResult.UInt16Value( byteIndex ) 
		    dim v2 as integer = mbOther.UInt16Value( byteIndex )
		    
		    if carry <> 0 and v1 >= carry then
		      v1 = v1 - 1
		      carry = carry - 1
		    end if
		    
		    if v1 < v2 then
		      carry = carry + 1
		      v1 = v1 + &h010000
		    end if
		    
		    v1 = v1 - v2
		    
		    mbResult.UInt16Value( byteIndex ) = v1
		  next
		  
		  //
		  // See if we have value
		  //
		  if mbResult.Int64Value( 0 ) = 0 and mbResult.Int64Value( 8 ) = 0 then
		    return 0.0
		  end if
		  
		  dim expValue as integer = ShiftToExponent( mbResult, higher.TrueExponent )
		  
		  dim resultStruct as QuadStruct
		  resultStruct.StringValue( mbResult.LittleEndian ) = mbResult
		  
		  dim result as new Quad_MTC( resultStruct )
		  result.TrueExponent = expValue
		  result.IsNegative = doNegate
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Add(toQuad As Quad_MTC) As Quad_MTC
		  if IsINF then
		    return self
		    
		  elseif toQuad.IsINF then
		    return toQuad
		    
		  elseif IsNan then
		    return self
		    
		  elseif toQuad.IsNan then
		    return toQuad
		    
		  elseif IsZero then
		    return toQuad
		    
		  elseif toQuad.IsZero then
		    return self
		    
		  elseif toQuad.IsNegative = IsNegative then
		    return DoAddition( toQuad )
		    
		  elseif toQuad.IsNegative then
		    return DoSubtraction( toQuad.Abs )
		    
		  else
		    return toQuad - Abs
		    
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Compare(other As Quad_MTC) As Integer
		  if self is other then
		    return 0
		  end if
		  
		  dim isZero as boolean = self.IsZero
		  dim otherIsZero as boolean = other.IsZero
		  
		  dim isNegative as boolean = self.IsNegative
		  dim otherIsNegative as boolean = other.IsNegative
		  
		  if isZero and otherIsZero then
		    return 0
		    
		  elseif isZero and not otherIsZero then
		    if otherIsNegative then
		      return 1
		    else
		      return -1
		    end if
		    
		  elseif not isZero and otherIsZero then
		    if isNegative then
		      return -1
		    else
		      return 1
		    end if
		    
		  elseif isNegative and not otherIsNegative then
		    return -1
		    
		  elseif not isNegative and otherIsNegative then
		    return 1
		    
		  end if
		  
		  dim isNan as boolean = self.IsNan
		  dim otherIsNan as boolean = other.IsNan
		  
		  if isNan and otherIsNan then
		    return 0
		  elseif isNan and not otherIsNan then
		    return -1
		  elseif not isNan and otherIsNan then
		    return 1
		  end if
		  
		  dim isInf as boolean = self.IsINF
		  dim otherIsInf as boolean = other.IsINF
		  
		  if isInf and otherIsInf then
		    return 0
		  elseif isInf and not otherIsInf then
		    return if( otherIsNegative, -1, 1 )
		  elseif not isInf and otherIsInf then
		    return if( otherIsNegative, 1, -1 )
		  end if
		  
		  dim result as integer
		  dim exp as Int16 = TrueExponent
		  dim otherExp as Int16 = other.TrueExponent
		  
		  if exp < otherExp then
		    result = -1
		  elseif exp > otherExp then
		    result = 1
		    
		  else
		    for i as integer = 0 to Data.Nums.Ubound
		      dim myN as UInt16 = Data.Nums( i )
		      dim otherN as UInt16 = other.Data.Nums( i )
		      
		      if myN < otherN then
		        result = -1
		        exit
		        
		      elseif myN > otherN then
		        result = 1
		        exit
		        
		      end if
		    next
		    
		  end if
		  
		  if isNegative then
		    result = 0 - result
		  end if
		  
		  return result
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Convert() As Double
		  return DoubleValue
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Operator_Convert(d As Double)
		  DoubleValue = d
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Multiply(other As Quad_MTC) As Quad_MTC
		  if IsZero then
		    return other
		    
		  elseif other.IsZero then
		    return self
		    
		  elseif IsINF then
		    return self
		    
		  elseif other.IsINF then
		    return other
		    
		  elseif IsNan then
		    return self
		    
		  elseif other.IsNan then
		    return other
		    
		  else
		    return DoMultiplication( other )
		    
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Negate() As Quad_MTC
		  dim q as new Quad_MTC( Data )
		  q.IsNegative = not IsNegative
		  return q
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Operator_Subtract(minusQuad As Quad_MTC) As Quad_MTC
		  if IsINF then
		    return self
		    
		  elseif minusQuad.IsINF then
		    return minusQuad
		    
		  elseif IsNan then
		    return self
		    
		  elseif minusQuad.IsNan then
		    return minusQuad
		    
		  elseif minusQuad.IsZero then
		    return self
		    
		  elseif IsZero then
		    return -minusQuad
		    
		  elseif not IsNegative and minusQuad.IsNegative then
		    return DoAddition( minusQuad.Abs )
		    
		  elseif IsNegative and not minusQuad.IsNegative then
		    dim r as Quad_MTC = Abs + minusQuad
		    r.IsNegative = true
		    return r
		    
		  elseif IsNegative and minusQuad.IsNegative then
		    return MinusQuad.Abs - Abs
		    
		  else
		    return DoSubtraction( minusQuad )
		    
		  end if
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Shared Sub ShiftLeft(mb As MemoryBlock, shift As Integer)
		  if shift = 0 then
		    return
		  end if
		  
		  dim p as ptr = mb
		  
		  while shift >= 64
		    p.UInt64( 0 ) = p.UInt64( 64 )
		    p.UInt64( 64 ) = CType( 0, UInt64 )
		    shift = shift - 64
		  wend
		  
		  while shift >= 32
		    p.UInt32( 0 ) = p.UInt32( 32 )
		    p.UInt32( 32 ) = p.UInt32( 64 )
		    p.UInt32( 64 ) = p.UInt32( 96 )
		    p.UInt32( 96 ) = CType( 0, UInt32 )
		    shift = shift - 32
		  wend
		  
		  while shift >= 16
		    for byteIndex as integer = 0 to 12 step 2
		      p.UInt16( byteIndex ) = p.UInt16( byteIndex + 2 )
		    next
		    p.UInt16( 14 ) = CType( 0, UInt16 )
		    shift = shift - 16
		  wend
		  
		  while shift >= 8
		    for byteIndex as integer = 0 to 14
		      p.UInt8( byteIndex ) = p.UInt8( byteIndex + 1 )
		    next
		    p.UInt8( 15 ) = CType( 0, UInt8 )
		    shift = shift - 8
		  wend
		  
		  if shift <> 0 then
		    dim leftShifter as UInt8 = CType( 2 ^ shift, UInt8 )
		    dim rightShifter as UInt8 = CType( 2 ^ ( 8 - shift ), UInt8 )
		    
		    for byteIndex as integer = 0 to 14
		      dim thisByte as UInt8 = p.UInt8( byteIndex )
		      dim nextByte as UInt8 = p.UInt8( byteIndex + 1 )
		      
		      dim mask as UInt8 = nextByte \ rightShifter
		      thisByte = ( thisByte * leftShifter ) or mask
		      p.UInt8( byteIndex ) = thisByte 
		    next
		    
		    p.Byte( 15 ) = p.Byte( 15 ) * leftShifter
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Shared Sub ShiftRight(mb As MemoryBlock, shift As Integer)
		  if shift = 0 then
		    return
		  end if
		  
		  dim p as ptr = mb
		  
		  while shift >= 64
		    p.UInt64( 64 ) = p.UInt64( 0 )
		    p.UInt64( 0 ) = CType( 0, UInt64 )
		    shift = shift - 64
		  wend
		  
		  while shift >= 32
		    p.UInt32( 96 ) = p.UInt32( 64 )
		    p.UInt32( 64 ) = p.UInt32( 32 )
		    p.UInt32( 32 ) = p.UInt32( 0 )
		    p.UInt32( 0 ) = CType( 0, UInt32 )
		    shift = shift - 32
		  wend
		  
		  while shift >= 16
		    for byteIndex as integer = 14 downto 2 step 2
		      p.UInt16( byteIndex ) = p.UInt16( byteIndex - 2 )
		    next
		    p.UInt16( 0 ) = CType( 0, UInt16 )
		    shift = shift - 16
		  wend
		  
		  while shift >= 8
		    for byteIndex as integer = 15 downto 1
		      p.UInt8( byteIndex ) = p.UInt8( byteIndex - 1 )
		    next
		    p.UInt8( 0 ) = CType( 0, UInt8 )
		    shift = shift - 8
		  wend
		  
		  if shift <> 0 then
		    dim rightShifter as UInt8 = CType( 2 ^ shift, UInt8 )
		    dim leftShifter as UInt8 = CType( 2 ^ ( 8 - shift ), UInt8 )
		    
		    for byteIndex as integer = 15 downto 1
		      dim thisByte as UInt8 = p.UInt8( byteIndex )
		      dim prevByte as UInt8 = p.UInt8( byteIndex - 1 )
		      
		      dim mask as UInt8 = prevByte * leftShifter
		      thisByte = ( thisByte \ rightShifter ) or mask
		      p.UInt8( byteIndex ) = thisByte 
		    next
		    
		    p.Byte( 0 ) = p.Byte( 0 ) \ rightShifter
		  end if
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Shared Function ShiftToExponent(mb As MemoryBlock, startingExp As Integer) As Integer
		  //
		  // Will shift a MemoryBlock 
		  // until the first Int16 = 1
		  // The caller must confirm that it has a value
		  //
		  
		  dim firstValue as integer = mb.UInt16Value( 0 ) and &b0111111111111111
		  if firstValue = 1 then
		    return startingExp
		  elseif firstValue <> 0 then
		    dim shift as integer
		    dim tester as integer = &b1000000000000000
		    for i as integer = 15 downto 0
		      if firstValue >= tester then
		        shift = i - 1
		        exit
		      end if
		      tester = tester \ 2
		    next
		    ShiftRight mb, shift
		    return startingExp + shift
		  end if
		  
		  //
		  // Find the first byte with content
		  //
		  dim p as ptr = mb
		  
		  dim targetByteIndex as integer
		  dim targetByte as byte
		  
		  dim lastByteIndex as integer = mb.Size - 1
		  for targetByteIndex = 2 to lastByteIndex
		    targetByte = p.Byte( targetByteIndex )
		    if targetByte <> 0 then
		      exit
		    end if
		  next
		  
		  dim targetBitIndex as integer
		  select case targetByte
		  case is >= &b10000000
		    targetBitIndex = 1
		  case is >= &b01000000
		    targetBitIndex = 2
		  case is >= &b00100000
		    targetBitIndex = 3
		  case is >= &b00010000
		    targetBitIndex = 4
		  case is >= &b00001000
		    targetBitIndex = 5
		  case is >= &b00000100
		    targetBitIndex = 6
		  case is >= &b00000010
		    targetBitIndex = 7
		  case else
		    targetBitIndex = 8
		  end select
		  
		  dim shift as integer = ( ( targetByteIndex - 2 ) * 8 ) + targetBitIndex
		  ShiftLeft mb, shift
		  return startingExp - shift
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Shared Function ToMemoryBlock(q As Quad_MTC, forMath As Boolean) As MemoryBlock
		  dim littleEndian as boolean = not forMath
		  
		  dim mb as MemoryBlock = q.Data.StringValue( littleEndian )
		  mb.LittleEndian = littleEndian
		  
		  if forMath then
		    dim p as ptr = mb
		    if p.Int16( 0 ) <> CType( 0, Int16 ) then
		      p.Byte( 0 ) = 0
		      p.Byte( 1 ) = 1
		    end if
		  end if
		  
		  return mb
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h21
		Private Data As QuadStruct
	#tag EndProperty

	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  dim bin() as string
			  bin.Append if( IsNegative, "1", "0" )
			  bin.Append " "
			  
			  dim exp as Int16 = Data.Exp and &b0111111111111111
			  bin.Append exp.ToBinary( 15 )
			  
			  for i as integer = 0 to Data.Nums.Ubound
			    bin.Append " "
			    bin.Append Data.Nums( i ).ToBinary( 16 )
			  next
			  
			  return join( bin, "" )
			End Get
		#tag EndGetter
		Private DebugBin As String
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  static kNan as double = val( "NaN" )
			  static kInf as double = val( "INF" )
			  
			  if IsZero then
			    return 0.0
			    
			  elseif IsNan then
			    if IsNegative then
			      return -kNan
			    else
			      return kNan
			    end if
			    
			  elseif IsINF then
			    if IsNegative then
			      return -kInf
			    else
			      return kInf
			    end if
			    
			  else
			    
			    dim mb as new MemoryBlock( 8 )
			    mb.LittleEndian = false
			    
			    for i as integer = 0 to 3
			      mb.UInt16Value( i * 2 ) = Data.Nums( i )
			    next
			    mb.UInt64Value( 0 ) = mb.UInt64Value( 0 ) \ CType( 2 ^ 12, UInt64 )
			    
			    dim exp as Int16 = Data.Exp
			    dim sign as Int16 = exp and &b1000000000000000
			    exp = exp and &b0111111111111111
			    
			    if exp <> CType( 0, Int16 ) then
			      exp = exp - kBias + CType( 1023, Int16 )
			      exp = exp * CType( 2 ^ 4, Int16 )
			    end if
			    
			    exp = exp or sign
			    exp = exp or mb.Int16Value( 0 )
			    
			    mb.Int16Value( 0 ) = exp
			    
			    return mb.DoubleValue( 0 )
			    
			  end if
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  static kNan as double = val( "NaN" )
			  static kInf as double = val( "INF" )
			  
			  if value = 0.0 then
			    Data.Exp = 0
			    for i as integer = 0 to Data.Nums.Ubound
			      Data.Nums( i ) = 0
			    next
			    
			  elseif value.Equals( kInf ) then
			    Data.Exp = &h7FFF
			    
			  elseif value.Equals( -kInf ) then
			    Data.Exp = &hFFFF
			    
			  elseif value.Equals( kNan ) then
			    Data.Exp = &h7FFF
			    Data.Nums( 0 ) = CType( &b1000000000000000, UInt16 )
			    
			  elseif value.Equals( -kNan ) then
			    Data.Exp = &hFFFF
			    Data.Nums( 0 ) = CType( &b1000000000000000, UInt16 )
			    
			  else
			    dim mb as new MemoryBlock( 8 )
			    mb.LittleEndian = false
			    mb.DoubleValue( 0 ) = value
			    
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
			  end if
			  
			End Set
		#tag EndSetter
		Private DoubleValue As Double
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  dim exp as Int16 = Data.Exp and CType( &b0111111111111111, Int16 )
			  if exp = CType( &h7FFF, Int16 ) and Data.Nums( 0 ) = CType( 0, UInt16 ) then
			    return true
			  else
			    return false
			  end if
			  
			End Get
		#tag EndGetter
		Private IsINF As Boolean
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  dim exp as Int16 = Data.Exp and CType( &b0111111111111111, Int16 )
			  if exp = CType( &h7FFF, Int16 ) and Data.Nums( 0 ) <> CType( 0, UInt16 ) then
			    return true
			  else
			    return false
			  end if
			  
			End Get
		#tag EndGetter
		Private IsNan As Boolean
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  return ( Data.Exp and CType( &b1000000000000000, Int16 ) ) <> CType( 0, Int16 )
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  if value then
			    Data.Exp = Data.Exp or &b1000000000000000
			  else
			    Data.Exp = Data.Exp and &b0111111111111111
			  end if
			  
			End Set
		#tag EndSetter
		Private IsNegative As Boolean
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  if ( Data.Exp and CType( &b0111111111111111, Int16 ) ) <> CType( 0, Int16 ) then
			    return false
			  end if
			  
			  for i as integer = 0 to Data.Nums.Ubound
			    if Data.Nums( i ) <> CType( 0, UInt16 ) then
			      return false
			    end if
			  next
			  
			  return true
			  
			End Get
		#tag EndGetter
		Private IsZero As Boolean
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h21
		#tag Getter
			Get
			  const kZero as Int16 = 0
			  
			  dim exp as Int16 = Data.Exp and &b0111111111111111
			  if exp = kZero then
			    return kZero
			  else
			    return exp - kBias
			  end if
			  
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  const kZero as Int16 = 0
			  
			  dim sign as Int16 = Data.Exp and ( &b100000000000000 )
			  
			  if value = kZero then
			    Data.Exp = sign
			  else
			    Data.Exp = sign or ( value + CType( kBias, UInt16 ) )
			  end if
			  
			End Set
		#tag EndSetter
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
