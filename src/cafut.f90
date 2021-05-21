module cafut

    use iso_fortran_env, only: real64

    implicit none

    integer, parameter :: wp = real64
    integer, parameter :: NAME_LENGTH = 20
    real(kind=real64), parameter :: eps = 1.0d-5

    character(len=*), parameter :: TEST_START = &
        '("==================== ", A, " ====================")'

    character(len=*), parameter :: TEST_END = &
        '(A, " FINISHED WITH ", I3, "/", I3, " TESTS PASSED")'

    character(len=*), parameter :: SUBTEST_START = &
        '("> ", A20)'

    character(len=*), parameter :: SUBTEST_END = &
        '("finished with" , I3, "/", I3, " images passed")'

    character(len=*), parameter :: SINGLE_VAL_FMT = &
        '(">> TEST FAILED | Image: ", I3, " | Got: ", F5.2, " | Expected: ", F5.2)'

    character(len=*), parameter :: ARR_VAL_IMG = &
        '(">> Image: ", I3)'

    character(len=*), parameter :: ARR_VAL_RES = &
        ">>> Got: "

    character(len=*), parameter :: ARR_VAL_EXP = &
        ">>> Expected: "

    type, public :: TestSuite
        integer, private :: n_tests
        character(len=NAME_LENGTH), private :: test_suite_name
        class(Test), public, pointer :: test
    contains
        procedure, pass :: add => addUnitTest
        procedure, pass :: runTests
        final :: deleteTestSuite
    end type TestSuite

    interface TestSuite
        module procedure newTestSuite
    end interface TestSuite

    type, abstract, private :: Test
        character(len=NAME_LENGTH), public :: test_name
        class(Test), private, pointer :: next
    contains
        procedure(runInterface), deferred, pass :: run
    end type Test

    abstract interface 
        function runInterface(self) result(tests_passed)
            import Test
            class(Test), intent(in) :: self
            integer :: tests_passed
        end function runInterface
    end interface 

    type, public, extends(Test) :: TestRealVal
        procedure(realCompInterface), nopass, pointer :: compare
        real(kind=wp) :: res, tgt
    contains
        procedure, pass :: run => runTestRealVal
        procedure, nopass :: printFail => printFailTestRealVal
        final :: deleteTestRealVal
    end type TestRealVal

    interface TestRealVal
        module procedure newTestRealVal_name
    end interface TestRealVal

    interface
        function realCompInterface(res, tgt) result(comp)
            import wp
            real(kind=wp), intent(in) :: res, tgt
            logical :: comp
        end function realCompInterface
    end interface

    type, public, extends(Test) :: TestRealArrVal
        procedure(realArrCompInterface), nopass, pointer :: compare
        real(kind=wp), allocatable, dimension(:) :: res, tgt
    contains
        procedure, pass :: run => runTestRealArrVal
        procedure, nopass :: printFail => printFailTestRealArrVal
        final :: deleteTestRealArrVal
    end type TestRealArrVal

    interface TestRealArrVal
        module procedure newTestRealArrVal_name
    end interface TestRealArrVal

    interface
        function realArrCompInterface(res, tgt) result(comp)
            import wp
            real(kind=wp), dimension(:), intent(in) :: res, tgt
            logical :: comp
        end function realArrCompInterface
    end interface

contains

    ! TestSuite

    function newTestSuite(ts_name) result(new_ts)
        character(len=*), intent(in) :: ts_name
        type(TestSuite) :: new_ts

        new_ts%n_tests = 0
        new_ts%test => null()

        new_ts%test_suite_name = ts_name
    end function newTestSuite

    subroutine deleteTestSuite(self)
        type(TestSuite), intent(inout) :: self

        deallocate(self%test)
    end subroutine deleteTestSuite

    subroutine runTests(self)
        class(TestSuite), intent(in) :: self

        integer :: tot_passed

        if (this_image() == 1) then
            print TEST_START, trim(self%test_suite_name)
        end if

        tot_passed = 0
        if (associated(self%test)) tot_passed = self%test%run()

        if (this_image() == 1) then
            print TEST_END, trim(self%test_suite_name), tot_passed, self%n_tests
        end if
    end subroutine runTests

    subroutine addUnitTest(self, ut)
        class(TestSuite), intent(inout) :: self
        class(Test), target, intent(inout) :: ut

        class(Test), pointer :: next
            
        if (this_image() == 1) self%n_tests = self%n_tests + 1

        allocate(next)

        next => self%test
        ut%next => next
        self%test => ut
    end subroutine addUnitTest

    ! Test

    !! Comparison Functions

    function realEq(res, tgt) result(comp)
        real(kind=wp), intent(in) :: res, tgt
        logical :: comp

        comp = abs(res-tgt) < eps
    end function realEq

    function realArrEq(res, tgt) result(comp)
        real(kind=wp), dimension(:), intent(in) :: res, tgt
        logical :: comp

        comp = all(abs(res-tgt) < eps)
    end function realArrEq

    !! Constructors

    function newTestRealVal_name(ts_name) result(new_ts)
        character(len=*), intent(in) :: ts_name
        type(TestRealVal) :: new_ts

        new_ts%test_name = ts_name
        new_ts%next => null()
        new_ts%compare => realEq
        !TODO: create a bunch of subclasses with different comparisson
        ! operators.
        new_ts%res = 0
        new_ts%tgt = 0
    end function newTestRealVal_name

    function newTestRealArrVal_name(ts_name) result(new_ts)
        character(len=*), intent(in) :: ts_name
        type(TestRealArrVal) :: new_ts

        new_ts%test_name = ts_name
        new_ts%next => null()
        new_ts%compare => realArrEq
        !TODO: create a bunch of subclasses with different comparisson
        ! operators.
    end function newTestRealArrVal_name

    !! Destructors

    subroutine deleteTestRealVal(self) 
        type(TestRealVal), intent(inout) :: self
        deallocate(self%next)
    end subroutine deleteTestRealVal

    subroutine deleteTestRealArrVal(self) 
        type(TestRealArrVal), intent(inout) :: self
        deallocate(self%next)
        deallocate(self%res)
        deallocate(self%tgt)
    end subroutine deleteTestRealArrVal

    !! Print Fail Functions

    subroutine printFailTestRealVal(img, res, tgt)
        integer, intent(in) :: img
        real(kind=wp), intent(in) :: res, tgt
        print SINGLE_VAL_FMT, img, res, tgt
    end subroutine printFailTestRealVal

    subroutine printFailTestRealArrVal(img, res, tgt)
        integer, intent(in) :: img
        real(kind=wp), dimension(:), intent(in) :: res, tgt

        print ARR_VAL_IMG, img
        print '(A)', ARR_VAL_RES 
        print *, res
        print '(A)', ARR_VAL_EXP
        print *, tgt
    end subroutine printFailTestRealArrVal

    !! Run Functions

    function runTestRealVal(self) result(tests_passed)
        class(TestRealVal), intent(in) :: self
        integer :: tests_passed

        real(kind=wp), allocatable, codimension[:] :: res, tgt
        integer :: img_passed, i

        if (associated(self%next)) then
            tests_passed = self%next%run()
        else
            tests_passed = 0
        end if

        allocate(res[*], tgt[*])
        res = self%res
        tgt = self%tgt

        sync all
        img_passed = 0
        if (this_image() == 1) then
            print SUBTEST_START, self%test_name
            do i=1, num_images()
                if (self%compare(res[i], tgt[i])) then
                    img_passed = img_passed + 1
                else
                    call self%printFail(i, res[i], tgt[i])
                end if
            end do
            print SUBTEST_END, img_passed, num_images()
        end if

        if (img_passed == num_images()) tests_passed = tests_passed + 1
    end function runTestRealVal

    function rootToAll(x) result(s)
        integer, intent(in) :: x
        integer :: s

        integer, allocatable, codimension[:]  :: y
        integer :: L, me, p

            me = this_image()
            p = num_images()

            allocate(y[*])
            !TODO: add error checking to all allocate statements.

            if (me == 1) y[1] = x
            sync all

            L = 1
            do while (L < p)
                L = 2*L
            end do
            do while (L > 0)
                if ((me+L <= p).and.(mod(me-1,2*L)==0)) y[me+L] = y
                L = L/2
                sync all
            end do
            s = y
    end function rootToAll

    function maxToRoot(x) result(s)
        integer, intent(in) :: x
        integer :: s

        integer, allocatable, codimension[:]  :: y
        integer :: L, me, p

            me = this_image()
            p = num_images()

            allocate(y[*])
            y = x
            sync all

            L = 1
            do while (L < p)
                if ((me+L <= p).and.(mod(me-1,2*L)==0)) y = max(y, y[me+L])
                L = 2*L
                sync all
            end do

            s = 0.0
            if (me == 1) s = y[1]
    end function maxToRoot

    function maxToAll(x) result(s)
        integer, intent(in) :: x
        integer :: s

            !TODO: consider creating a specific sum function if
            ! there is even a slight speedup
            s = rootToAll(maxToRoot(x))
    end function maxToAll

    function runTestRealArrVal(self) result(tests_passed)
        class(TestRealArrVal), intent(in) :: self
        integer :: tests_passed

        real(kind=wp), allocatable, dimension(:), codimension[:] :: res, tgt
        integer, allocatable, codimension[:] :: res_n, tgt_n

        integer :: max_res_n, max_tgt_n
        integer :: img_passed, i

        if (associated(self%next)) then
            tests_passed = self%next%run()
        else
            tests_passed = 0
        end if

        allocate(res_n[*], tgt_n[*])
        res_n = size(self%res)
        tgt_n = size(self%tgt)

        max_res_n = maxToAll(res_n)
        max_tgt_n = maxToAll(tgt_n)

        allocate(res(max_res_n)[*], tgt(max_tgt_n)[*])
        res(:res_n) = self%res
        tgt(:res_n) = self%tgt

        sync all
        img_passed = 0
        if (this_image() == 1) then
            print SUBTEST_START, self%test_name
            do i=1, num_images()
                if (self%compare(res(:res_n[i])[i], tgt(:tgt_n[i])[i])) then
                    img_passed = img_passed + 1
                else
                    call self%printFail(i, res(:res_n[i])[i], tgt(:tgt_n[i])[i])
                end if
            end do
            print SUBTEST_END, img_passed, num_images()
        end if

        if (img_passed == num_images()) tests_passed = tests_passed + 1

        deallocate(res_n, tgt_n, res, tgt)
    end function runTestRealArrVal

end module cafut
