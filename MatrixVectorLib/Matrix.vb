Imports System.Math
Imports System.Text.RegularExpressions
Imports System.IO



Public Class Matrix
    Implements ICloneable

    Private val(,) As Double

    Public L As Matrix ' L-matrix of LU decomposition
    Public U As Matrix ' U-matrix of LU decomposition
    Private perm() As Integer ' Permutation vector of LU decomposition
    Private detOfP As Double = 1



    Public Sub New(n As Integer, m As Integer)
        ReDim val(n - 1, m - 1)
    End Sub
    Public Sub New(n As Integer, m As Integer, s As String)
        Dim tArr() As String
        tArr = Regex.Split(s, "\s*[, ]\s*")
        If (tArr.Length < n * m) Then
            Throw New Exception("Insuficient data in string")
        End If
        ReDim val(n - 1, m - 1)
        For i As Integer = 0 To n - 1
            For j As Integer = 0 To m - 1
                val(i, j) = CDbl(tArr(i * m + j))
            Next
        Next
    End Sub

    Public Shared Function IdentityMatrix(n As Integer) As Matrix
        Dim m As Matrix = New Matrix(n, n)
        For i As Integer = 0 To n - 1
            m(i, i) = 1.0
        Next
        Return m
    End Function

    Public Function Clone() As Object Implements ICloneable.Clone
        Dim newmat As Matrix = DirectCast(MemberwiseClone(), Matrix)
        newmat.val = newmat.val.Clone
        If L IsNot Nothing Then newmat.L = newmat.L.Clone
        If U IsNot Nothing Then newmat.U = newmat.U.Clone
        If perm IsNot Nothing Then newmat.perm = newmat.perm.Clone
        Return newmat
    End Function

    Public ReadOnly Property RowCount As Integer
        Get
            Return val.GetUpperBound(0) + 1
        End Get
    End Property
    Public ReadOnly Property ColumnCount As Integer
        Get
            Return val.GetUpperBound(1) + 1
        End Get
    End Property

    Public Property Row(n As Integer) As Vector
        Get
            Dim v As Vector = New Vector(Me.ColumnCount)
            For i As Integer = 0 To Me.ColumnCount - 1
                v(i) = val(n - 1, i)
            Next
            Return v
        End Get
        Set(value As Vector)
            If (Me.ColumnCount <> value.Size) Then
                Throw New Exception("Wrong size of vector")
            End If
            For i As Integer = 0 To Me.ColumnCount - 1
                val(n - 1, i) = value(i)
            Next
            L = Nothing ' Invalidate LU decomposition
        End Set
    End Property

    Public Property Column(n As Integer) As Vector
        Get
            Dim v As Vector = New Vector(Me.RowCount)
            For i As Integer = 0 To Me.RowCount - 1
                v(i) = val(i, n - 1)
            Next
            Return v
        End Get
        Set(value As Vector)
            If (Me.RowCount <> value.Size) Then
                Throw New Exception("Wrong size of vector")
            End If
            For i As Integer = 0 To Me.RowCount - 1
                val(i, n - 1) = value(i)
            Next
            L = Nothing ' Invalidate LU decomposition
        End Set
    End Property

    Public Property Diagonal As Vector
        Get
            If (Me.RowCount <> Me.ColumnCount) Then
                Throw New Exception("The matrix is not square!")
            End If
            Dim v As Vector = New Vector(Me.RowCount)
            For i As Integer = 0 To Me.RowCount - 1
                v(i) = val(i, i)
            Next
            Return v
        End Get
        Set(value As Vector)
            If (Me.RowCount <> Me.ColumnCount) Then
                Throw New Exception("The matrix is not square!")
            End If
            If (Me.RowCount <> value.Size) Then
                Throw New Exception("Wrong size of vector")
            End If
            For i As Integer = 0 To Me.RowCount - 1
                val(i, i) = value(i)
            Next
            L = Nothing ' Invalidate LU decomposition
        End Set
    End Property

    Default Public Property Item(i As Integer, j As Integer) As Double
        Get
            Return val(i, j)
        End Get
        Set(value As Double)
            val(i, j) = value
            L = Nothing ' Invalidate LU decomposition
        End Set
    End Property

    ''' <summary>
    ''' Return slice of the matrix.
    ''' </summary>
    ''' <param name="i">Row index of first element</param>
    ''' <param name="j">Column index of first element</param>
    ''' <param name="rows">Number of rows in slice</param>
    ''' <param name="columns">Number of columns in slice</param>
    Public Property Slice(i As Integer, j As Integer, rows As Integer, columns As Integer) As Matrix
        Get
            Dim M As New Matrix(rows, columns)
            If i < 0 OrElse j < 0 Then Return M
            For l As Integer = i To Min(i + rows - 1, Me.RowCount - 1)
                For k As Integer = j To Min(j + columns - 1, Me.ColumnCount)
                    M(l - i, k - j) = val(l, k)
                Next
            Next
            Return M
        End Get
        Set(value As Matrix)
            If i < 0 OrElse j < 0 Then Return
            For l As Integer = i To Min(i + rows - 1, Me.RowCount - 1)
                For k As Integer = j To Min(j + columns - 1, Me.ColumnCount)
                    val(l, k) = value(l - i, k - j)
                Next
            Next
        End Set
    End Property

    Public Function Transpose() As Matrix
        Dim m As Matrix = New Matrix(Me.ColumnCount, Me.RowCount)
        For i As Integer = val.GetLowerBound(0) To val.GetUpperBound(0)
            For j As Integer = val.GetLowerBound(1) To val.GetUpperBound(1)
                m(j, i) = val(i, j)
            Next
        Next
        Return m
    End Function

    ''' <summary>
    ''' Takes upper triangular submatrix and creates symmetrical matrix
    ''' </summary>
    Public Function Symmetry() As Matrix

        If Me.RowCount <> Me.ColumnCount Then
            Throw New Exception("Matrix must be square")
        End If

        Dim M As Matrix = Me.Clone
        For i As Integer = 0 To Me.RowCount - 1
            For j As Integer = i + 1 To Me.ColumnCount - 1
                M(j, i) = M(i, j)
            Next
        Next
        Return M
    End Function

    Public Function Add(m As Matrix) As Matrix
        If (Me.RowCount <> m.RowCount OrElse Me.ColumnCount <> m.RowCount) Then
            Throw New Exception(String.Format("Matrix Add: wrong dimensions -> ({0} x {1}) + ({2} x {3})", Me.RowCount, Me.ColumnCount, m.RowCount, m.ColumnCount))
        End If
        Dim m2 As Matrix = New Matrix(Me.RowCount, Me.ColumnCount)
        For i As Integer = val.GetLowerBound(0) To val.GetUpperBound(0)
            For j As Integer = val.GetLowerBound(1) To val.GetUpperBound(1)
                m2(i, j) = val(i, j) + m(i, j)
            Next
        Next
        Return m2
    End Function

    Public Function Subtract(m As Matrix) As Matrix
        If (Me.RowCount <> m.RowCount OrElse Me.ColumnCount <> m.RowCount) Then
            Throw New Exception("Wrong dimensions of matrix")
        End If
        Dim m2 As Matrix = New Matrix(Me.RowCount, Me.ColumnCount)
        For i As Integer = val.GetLowerBound(0) To val.GetUpperBound(0)
            For j As Integer = val.GetLowerBound(1) To val.GetUpperBound(1)
                m2(i, j) = val(i, j) - m(i, j)
            Next
        Next
        Return m2
    End Function

    Public Function Mult(m As Matrix) As Matrix
        If (Me.ColumnCount <> m.RowCount) Then
            Throw New Exception(String.Format("Matrix mult: wrong dimensions -> ({0} x {1}) * ({2} x {3})", Me.RowCount, Me.ColumnCount, m.RowCount, m.ColumnCount))
        End If
        Dim m2 As Matrix = New Matrix(Me.RowCount, m.ColumnCount)
        For i As Integer = val.GetLowerBound(0) To val.GetUpperBound(0)
            For j As Integer = val.GetLowerBound(1) To val.GetUpperBound(1)
                For k As Integer = 0 To m.ColumnCount - 1
                    m2(i, k) += val(i, j) * m(j, k)
                Next
            Next
        Next
        Return m2
    End Function

    Public Function Mult(a As Double) As Matrix
        Dim m2 As Matrix = New Matrix(Me.RowCount, Me.ColumnCount)
        For i As Integer = val.GetLowerBound(0) To val.GetUpperBound(0)
            For j As Integer = val.GetLowerBound(1) To val.GetUpperBound(1)
                m2(i, j) = val(i, j) * a
            Next
        Next
        Return m2
    End Function

    Public Function Mult(v As Vector) As Vector
        If (Me.ColumnCount <> v.Size) Then
            Throw New Exception(String.Format("Wrong dimensions of matrix and vector -> ({0} x {1}) * [{2}]", Me.RowCount, Me.ColumnCount, v.Size))
        End If
        Dim v2 As Vector = New Vector(Me.RowCount)
        For i As Integer = val.GetLowerBound(0) To val.GetUpperBound(0)
            For j As Integer = val.GetLowerBound(1) To val.GetUpperBound(1)
                v2(i) += val(i, j) * v(j)
            Next
        Next
        Return v2
    End Function

    Public Sub SwapColumns(i As Integer, j As Integer)
        Dim t As Double
        For k As Integer = 0 To Me.RowCount - 1
            t = val(k, j)
            val(k, j) = val(k, i)
            val(k, i) = t
        Next
    End Sub

    Public Sub SwapRows(i As Integer, j As Integer)
        Dim t As Double
        For k As Integer = 0 To Me.ColumnCount - 1
            t = val(i, k)
            val(i, k) = val(j, k)
            val(j, k) = t
        Next
    End Sub

    Public Sub Copy(M As Matrix, i As Integer, j As Integer)
        For k As Integer = 0 To M.RowCount - 1
            For l As Integer = 0 To M.ColumnCount - 1
                val(k + i, l + j) = M(k, l)
            Next
        Next
    End Sub

    Public Sub MakeLU()
        ' LU decomposition
        Dim rows As Integer = Me.RowCount
        Dim cols As Integer = Me.ColumnCount
        If (rows <> cols) Then
            Throw New Exception(String.Format("MakeLU sub: the matrix is not square! -> ({0} x {1})", Me.RowCount, Me.ColumnCount))
        End If
        L = IdentityMatrix(rows)
        U = Me.Clone
        detOfP = 1.0

        perm = New Integer(rows - 1) {}
        For i As Integer = 0 To rows - 1
            perm(i) = i
        Next

        Dim p As Double = 0
        Dim tmpd As Double
        Dim k0 As Integer = 0
        Dim tmp As Integer = 0

        For k As Integer = 0 To cols - 2
            p = 0
            For i As Integer = k To rows - 1
                ' find the row with the biggest pivot
                If Abs(U(i, k)) > p Then
                    p = Abs(U(i, k))
                    k0 = i
                End If
            Next
            If p = 0 Then
                Throw New Exception("MakeLU sub: the matrix is singular!")
            End If

            If k <> k0 Then
                detOfP *= -1

                ' swap rows k and k0
                tmp = perm(k)
                perm(k) = perm(k0)
                perm(k0) = tmp
                For i As Integer = 0 To k - 1
                    tmpd = L(k, i)
                    L(k, i) = L(k0, i)
                    L(k0, i) = tmpd
                Next
                For i As Integer = 0 To cols - 1
                    tmpd = U(k, i)
                    U(k, i) = U(k0, i)
                    U(k0, i) = tmpd
                Next
            End If

            For i As Integer = k + 1 To rows - 1
                L(i, k) = U(i, k) / U(k, k)
                For j As Integer = k To cols - 1
                    U(i, j) = U(i, j) - L(i, k) * U(k, j)
                Next
            Next
        Next
    End Sub
    Private Function SubsForth(b As Vector) As Vector
        ' Function solves Lx = b
        If Me.L Is Nothing Then
            Me.MakeLU()
        End If
        Dim n As Integer = Me.RowCount
        Dim x As New Vector(n)

        For i As Integer = 0 To n - 1
            x(i) = b(i)
            For j As Integer = 0 To i - 1
                x(i) -= L(i, j) * x(j)
            Next
            x(i) = x(i) / L(i, i)
        Next
        Return x
    End Function
    Private Function SubsBack(b As Vector) As Vector
        ' Function solves Ux = b
        If Me.L Is Nothing Then
            Me.MakeLU()
        End If
        Dim n As Integer = Me.RowCount
        Dim x As New Vector(n)

        For i As Integer = n - 1 To -1 + 1 Step -1
            x(i) = b(i)
            For j As Integer = n - 1 To i + 1 Step -1
                x(i) -= Me.U(i, j) * x(j)
            Next
            x(i) = x(i) / Me.U(i, i)
        Next
        Return x
    End Function

    Public Function Solve(v As Vector) As Vector
        ' Function solves Ax = v
        Dim rows As Integer = Me.RowCount
        Dim cols As Integer = Me.ColumnCount
        If (rows <> cols) Then
            Throw New Exception("The matrix is not square!")
        End If
        If rows <> v.Size Then
            Throw New Exception("Wrong size of the vector!")
        End If
        If L Is Nothing Then
            MakeLU()
        End If

        Dim b As New Vector(rows)
        ' swap items in "v" due to permutation matrix
        For i As Integer = 0 To rows - 1
            b(i) = v(perm(i))
        Next
        Dim z As Vector = SubsForth(b)
        Dim x As Vector = SubsBack(z)

        Return x
    End Function

    ''' <summary>
    ''' Solve rectangular system Ax=b
    ''' </summary>
    ''' <param name="b">Right side vector</param>
    ''' <returns></returns>
    Public Function LeastSquare(b As Vector) As Vector
        If (Me.RowCount < Me.ColumnCount) Then
            Throw New Exception(String.Format("LeastSquare sub: the shape of the matrix is not correct -> ({0} x {1})", Me.RowCount, Me.ColumnCount))
        End If
        If (Me.RowCount <> b.Size) Then
            Throw New Exception("LeastSquare sub: Wrong size of the vector!")
        End If

        Dim at As Matrix = Me.Transpose
        Dim an As Matrix = at * Me
        Dim bn As Vector = at * b
        Dim x As Vector = an.Solve(bn)
        Return x
    End Function

    ''' <summary>
    ''' Solve rectangular system Ax=b using 'w' as weight vector
    ''' </summary>
    ''' <param name="b">Right side vector</param>
    ''' <param name="w">Weight vector</param>
    ''' <returns></returns>
    Public Function WeightedLeastSquare(b As Vector, w As Vector) As Vector
        ' Solve Ax=b using 'w' as weight vector
        If (Me.RowCount < Me.ColumnCount) Then
            Throw New Exception(String.Format("WeightedLeastSquare sub: the shape of the matrix is not correct -> ({0} x {1})", Me.RowCount, Me.ColumnCount))
        End If
        If (Me.RowCount <> b.Size) Then
            Throw New Exception("WeightedLeastSquare sub: Wrong size of the vector 'b'!")
        End If
        If (Me.RowCount <> w.Size) Then
            Throw New Exception("WeightedLeastSquare sub: Wrong size of the vector 'w'!")
        End If

        Dim at As Matrix = Me.Transpose
        Dim Wdiag As Matrix = New Matrix(Me.RowCount, Me.RowCount)
        Wdiag.Diagonal = w
        Dim an As Matrix = at * Wdiag * Me
        ' Multiply right side by weight vector
        Dim bn As Vector = b.PointWiseMult(w)
        bn = at * bn
        Dim x As Vector = an.Solve(bn)
        Return x
    End Function

    Public Function Residuals(x As Vector, b As Vector) As Vector
        Return Me * x - b
    End Function
    Public Function ResidualsNorm(x As Vector, b As Vector) As Double
        Return (Me * x - b).Norm
    End Function
    Public Function WeightedResidualsNorm(x As Vector, b As Vector, w As Vector) As Double
        Return (Me * x - b).PointWiseMult(w).Norm
    End Function

    Public Function Inverse() As Matrix
        If L Is Nothing Then
            MakeLU()
        End If
        Dim inv As New Matrix(Me.RowCount, Me.ColumnCount)

        For i As Integer = 0 To Me.RowCount - 1
            Dim b As Vector = New Vector(Me.RowCount)
            b(i) = 1.0
            Dim col As Vector = Me.Solve(b)
            inv.Column(i + 1) = col
        Next
        Return inv
    End Function

    ''' <summary>
    ''' Right Pseudoinverse of matrix A:
    ''' A_T * (A * A_T)^-1
    ''' </summary>
    ''' <returns></returns>
    Public Function RightPseudoinverse() As Matrix
        Return Me.Transpose * (Me * Me.Transpose).Inverse
    End Function

    ''' <summary>
    ''' Returns permutation matrix "P" such, that A = P*L*U
    ''' </summary>
    Public Function P() As Matrix
        If L Is Nothing Then
            MakeLU()
        End If
        Dim matrix As Matrix = New Matrix(Me.RowCount, Me.RowCount)
        For i As Integer = 0 To Me.RowCount - 1
            matrix(perm(i), i) = 1
        Next
        Return matrix
    End Function

    Public Function Det() As Double
        If L Is Nothing Then
            MakeLU()
        End If
        Dim dett As Double = detOfP
        For i As Integer = 0 To Me.RowCount - 1
            dett *= U(i, i)
        Next
        Return dett
    End Function

    ''' <summary>
    ''' Generates random n*m matrix with elements in (-dispersion, dispersion) range
    ''' </summary>
    Public Shared Function RandomMatrix(n As Integer, m As Integer, dispersion As Double) As Matrix
        ' Function generates the random matrix
        Dim rnd As New Random()
        Dim matrix As New Matrix(n, m)
        For i As Integer = 0 To n - 1
            For j As Integer = 0 To m - 1
                matrix(i, j) = 2 * rnd.NextDouble() * dispersion - dispersion
            Next
        Next
        Return matrix
    End Function

    Public Overrides Function ToString() As String
        Dim str As New System.Text.StringBuilder
        str.Append(String.Format("Matrix: {0} x {1}", val.GetUpperBound(0) + 1, val.GetUpperBound(1) + 1) + vbCrLf)
        For i As Integer = val.GetLowerBound(0) To val.GetUpperBound(0)
            For j As Integer = val.GetLowerBound(1) To val.GetUpperBound(1)
                str.Append(String.Format("{0,10:g4}", val(i, j)) + " ")
            Next
            str.Append(vbCrLf)
        Next
        Return str.ToString
    End Function


    ' Operators overloads
    '-----------------------------------------------------------------
    Public Shared Operator +(m As Matrix, a As Matrix) As Matrix
        Return m.Add(a)
    End Operator
    Public Shared Operator -(m As Matrix, a As Matrix) As Matrix
        Return m.Subtract(a)
    End Operator
    Public Shared Operator -(m As Matrix) As Matrix
        Return m.Mult(-1.0)
    End Operator
    Public Shared Operator *(m As Matrix, a As Matrix) As Matrix
        Return m.Mult(a)
    End Operator
    Public Shared Operator *(m As Matrix, v As Vector) As Vector
        Return m.Mult(v)
    End Operator
    Public Shared Operator *(m As Matrix, a As Double) As Matrix
        Return m.Mult(a)
    End Operator
    Public Shared Operator *(a As Double, m As Matrix) As Matrix
        Return m.Mult(a)
    End Operator
    Public Shared Operator /(m As Matrix, a As Double) As Matrix
        Return m.Mult(1.0 / a)
    End Operator



End Class

