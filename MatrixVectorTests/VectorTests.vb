Imports System.Text
Imports MatrixVectorLib
Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass()> Public Class VectorTests

    <TestMethod()> Public Sub VectorSliceTest()
        Dim v As New Vector({1, 2, 3, 4, 5})

        Assert.IsTrue((v.Slice(0, 1) - New Vector({1})).Norm < 0.000000000001)
        Assert.IsTrue((v.Slice(0, 5) - New Vector({1, 2, 3, 4, 5})).Norm < 0.000000000001)
        Assert.IsTrue((v.Slice(0, 6) - New Vector({1, 2, 3, 4, 5, 0})).Norm < 0.000000000001)
        Assert.IsTrue((v.Slice(3, 2) - New Vector({4, 5})).Norm < 0.000000000001)

    End Sub

End Class