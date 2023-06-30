program main
    use iso_fortran_env
    use cafut

    type(TestSuite) :: ts
    type(TestRealVal) :: trv1, trv2
    type(TestRealArrVal) :: trav1, trav2

    real(kind=real64) :: right, wrong
    real(kind=real64), allocatable, dimension(:) :: aright, awrong

    right = 1.
    wrong = 2.

    allocate(aright(2), awrong(2))
    aright = 1.
    awrong = 2.

    ts = TestSuite("TEST 1")

    trv1 = TestRealVal("subtest 1.1 (should pass)")
    trv1%res = right
    trv1%tgt = right
    call ts%add(trv1)

    trv2 = TestRealVal("subtest 1.2 (should fail)")
    trv2%res = right
    trv2%tgt = wrong
    call ts%add(trv2)

    trav1 = TestRealArrVal("subtest 1.3 (should pass)")
    allocate(trav1%res(2), trav1%tgt(2))
    trav1%res = aright
    trav1%tgt = aright
    call ts%add(trav1)

    trav2 = TestRealArrVal("subtest 1.4 (should fail)")
    allocate(trav2%res(2), trav2%tgt(2))
    trav2%res = aright
    trav2%tgt = awrong
    call ts%add(trav2)

    call ts%runTests()

    ts = TestSuite("TEST 2")
    call ts%add(TestRealVal("subtest 2.1 (should fail)"), right, wrong)
    call ts%add(TestRealVal("subtest 2.2 (should pass)"), right, right)

    call ts%add(TestRealArrVal("subtest 2.3 (should fail)"), aright, awrong)
    call ts%add(TestRealArrVal("subtest 2.4 (should pass)"), aright, aright)

    call ts%runTests()
end program main
