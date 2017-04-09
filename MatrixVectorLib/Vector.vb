Imports System.Math
Imports System.Text.RegularExpressions

Public Class Vector
    Implements ICloneable

    Private val() As Double

    Public Sub New(n As Integer)
        ReDim val(n - 1)
    End Sub
    Public Sub New(a() As Double)
        ReDim val(a.Length - 1)
        val = a.Clone
    End Sub
    Public Sub New(s As String)
        Dim tArr() As String
        tArr = Regex.Split(s, "\s*[, ]\s*")
        ReDim val(tArr.Length - 1)
        For i As Integer = 0 To tArr.Length - 1
            val(i) = CDbl(tArr(i))
        Next
    End Sub


    Public Function Clone() As Object Implements ICloneable.Clone
        Dim newvec As Vector = DirectCast(MemberwiseClone(), Vector)
        newvec.val = newvec.val.Clone
        Return newvec
    End Function

    Default Public Property Item(i As Integer) As Double
        Get
            Return val(i)
        End Get
        Set(value As Double)
            val(i) = value
        End Set
    End Property

    ''' <summary>
    ''' Return slice of the vector.
    ''' </summary>
    ''' <param name="n">Index of the first element</param>
    ''' <param name="len">Length f the slice</param>
    Public Property Slice(n As Integer, len As Integer) As Vector
        Get
            Dim v As New Vector(len)
            If n < 0 OrElse n > Me.Size Then Return v
            For i As Integer = n To Min(Me.Size - 1, n + len - 1)
                v(i - n) = val(i)
            Next
            Return v
        End Get
        Set(value As Vector)
            If n < 0 OrElse n > Me.Size Then Return
            For i As Integer = n To Min(Me.Size - 1, n + len - 1)
                val(i) = value(i - n)
            Next
        End Set
    End Property

    Public ReadOnly Property Size As Integer
        Get
            Return val.Length
        End Get
    End Property

    Public ReadOnly Property Norm As Double
        Get
            Norm = 0
            For i As Integer = 0 To val.GetUpperBound(0)
                Norm += val(i) ^ 2
            Next
            Return Sqrt(Norm)
        End Get
    End Property

    Public Sub Normalize()
        Dim tmp As Double = 1.0 / Me.Norm
        For i As Integer = 0 To val.GetUpperBound(0)
            val(i) = val(i) * tmp
        Next
    End Sub

    Public Function Normalized() As Vector
        Dim tmp As Vector = New Vector(Me.Size)
        Dim tmp_norm As Double = Me.Norm
        For i As Integer = 0 To val.GetUpperBound(0)
            tmp.val(i) = val(i) / tmp_norm
        Next
        Return tmp
    End Function

    Public Function Add(ByVal a As Double) As Vector
        Dim tmp As Vector = Me.Clone
        For i As Integer = 0 To val.GetUpperBound(0)
            tmp.val(i) += a
        Next
        Return tmp
    End Function
    Public Function Add(ByVal v As Vector) As Vector
        If (Me.Size <> v.Size) Then
            Throw New Exception("Wrong size of vector")
        End If
        Dim tmp As Vector = Me.Clone
        For i As Integer = 0 To val.GetUpperBound(0)
            tmp.val(i) += v(i)
        Next
        Return tmp
    End Function
    Public Function Subtract(ByVal a As Double) As Vector
        Dim tmp As Vector = Me.Clone
        For i As Integer = 0 To val.GetUpperBound(0)
            tmp.val(i) -= a
        Next
        Return tmp
    End Function
    Public Function Subtract(ByVal v As Vector) As Vector
        If (Me.Size <> v.Size) Then
            Throw New Exception("Wrong size of vector")
        End If
        Dim tmp As Vector = Me.Clone
        For i As Integer = 0 To val.GetUpperBound(0)
            tmp.val(i) -= v(i)
        Next
        Return tmp
    End Function

    Public Function Mult(ByVal a As Double) As Vector
        Dim tmp As Vector = Me.Clone
        For i As Integer = 0 To val.GetUpperBound(0)
            tmp.val(i) *= a
        Next
        Return tmp
    End Function

    Public Function Mult(m As Matrix) As Vector
        If (m.RowCount <> Me.Size) Then
            Throw New Exception("Wrong dimensions of matrix and vector")
        End If
        Dim v2 As Vector = New Vector(m.ColumnCount)
        For i As Integer = 0 To m.ColumnCount
            For j As Integer = 0 To Me.Size
                v2(i) += val(j) * m(j, i)
            Next
        Next
        Return v2
    End Function

    Public Function Dot(ByVal v As Vector) As Double
        If (Me.Size <> v.Size) Then
            Throw New Exception("Wrong size of vector")
        End If
        Dim tmp As Double = 0.0
        For i As Integer = 0 To val.GetUpperBound(0)
            tmp += val(i) * v(i)
        Next
        Return tmp
    End Function

    ''' <summary>
    ''' Returns element-wise multiplication of two vectors (Hadamard product)
    ''' </summary>
    Public Function PointWiseMult(v As Vector) As Vector
        If (Me.Size <> v.Size) Then
            Throw New Exception("Wrong size of vector")
        End If
        Dim v2 As Vector = New Vector(Me.Size)
        For i As Integer = 0 To val.GetUpperBound(0)
            v2(i) = val(i) * v(i)
        Next
        Return v2
    End Function

    ''' <summary>
    ''' Returns element-wise division of two vectors
    ''' </summary>
    Public Function PointWiseDivide(v As Vector) As Vector
        If (Me.Size <> v.Size) Then
            Throw New Exception("Wrong size of vector")
        End If
        Dim v2 As Vector = New Vector(Me.Size)
        For i As Integer = 0 To val.GetUpperBound(0)
            If v(i) = 0 Then
                Throw New Exception("Divide by Zero")
            End If
            v2(i) = val(i) / v(i)
        Next
        Return v2
    End Function

    Public Overrides Function ToString() As String
        Dim str As New System.Text.StringBuilder
        str.Append(String.Format("Vector: Size={0}", Me.Size) + vbCrLf)
        For i As Integer = val.GetLowerBound(0) To val.GetUpperBound(0)
            str.Append(String.Format("{0,9:g4}", val(i)) + " ")
        Next
        Return str.ToString
    End Function

    ' Operators overloads
    '-----------------------------------------------------------------
    Public Shared Operator +(v As Vector, a As Vector) As Vector
        Return v.Add(a)
    End Operator
    Public Shared Operator -(v As Vector) As Vector
        Return v.Mult(-1.0)
    End Operator
    Public Shared Operator -(v As Vector, a As Vector) As Vector
        Return v.Subtract(a)
    End Operator
    Public Shared Operator *(v As Vector, a As Double) As Vector
        Return v.Mult(a)
    End Operator
    Public Shared Operator *(v As Vector, m As Matrix) As Vector
        Return v.Mult(m)
    End Operator
    Public Shared Operator *(a As Double, v As Vector) As Vector
        Return v.Mult(a)
    End Operator
    Public Shared Operator *(v As Vector, a As Vector) As Double
        Return v.Dot(a)
    End Operator

End Class

