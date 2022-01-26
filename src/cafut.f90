module cafut

use iso_fortran_env, only: real64

implicit none

!> Define real number kind.
integer, private, parameter :: wp = real64

!> Define maximum name length.
integer, private, parameter :: NAME_LENGTH = 50

!> Define margin of floating point error for real value comparissons.
real(kind=real64), private, parameter :: default_eps = 1.0d-5

!> Format of the start of a unit test.
character(len=*), private, parameter :: TEST_START = &
    '("==================== ", A, " ====================")'

!> Format of the end of a unit test.
character(len=*), private, parameter :: TEST_END = &
    '(A, " FINISHED WITH ", I3, "/", I3, " TESTS PASSED")'

!> Format of the start of a unit test.
character(len=*), private, parameter :: SUBTEST_START = &
    '("> ", A20)'

!> Format of the end of a unit test.
character(len=*), private, parameter :: SUBTEST_END = &
    '("finished with" , I3, "/", I3, " images passed")'

!> Format of the failure description of an image comparing real values.
character(len=*), private, parameter :: SINGLE_VAL_FMT = &
    '(">> TEST FAILED | Image: ", I3, " | Got: ", F10.5, " | Expected: ", F10.5)'

!> Format of the header for a failed test of an image comparring arrays.
character(len=*), private, parameter :: ARR_VAL_IMG = &
    '(">> Image: ", I3)'

!> String preceeding result array.
character(len=*), private, parameter :: ARR_VAL_RES = &
    ">>> Got: "

!> String preceeding expected array.
character(len=*), private, parameter :: ARR_VAL_EXP = &
    ">>> Expected: "

type, public :: TestSuite
    !! Holds tests and manages their executtion.
    !! Represents a set of procedures to test a certain feature.
    !! First node of the test linked list.

    integer, private :: n_tests
        !! Number of tests in a test suite.
    character(len=NAME_LENGTH), private :: test_suite_name
        !! Name of the test suite.
    class(Test), public, pointer :: test
        !! Current test whose attributes are available to be set.

contains

    procedure, public, pass :: addUnitTest
    procedure, private, pass :: addTestRealVal
    procedure, private, pass :: addTestRealArrVal
    generic, public :: add => addUnitTest, addTestRealVal, addTestRealArrVal

    procedure, public, pass :: runTests
    final :: deleteTestSuite

end type TestSuite

interface TestSuite
    !! Constructor interface for a TestSuite object.

    module procedure newTestSuite
end interface TestSuite

type, abstract, private :: Test
    !! Abstract class for a single test case. A node in the test linked list.

    character(len=NAME_LENGTH), public :: test_name
        !! Name of the test case.
    class(Test), private, pointer :: next
        !! Next test case or null() if this is the first test inserted (last
        !! test in the linked list).
contains
    procedure(runInterface), deferred, pass :: run
end type Test

abstract interface
    function runInterface(self) result(tests_passed)
        !! Abstract function interface for running a test.

        import Test
        class(Test), intent(in) :: self
            !! The test itself. The Test object should contain all information
            !! needed to run the test.
        integer :: tests_passed
            !! Return total number of tests which passed in the linked list
            !! up to and including this test.
    end function runInterface
end interface

type, public, extends(Test) :: TestRealVal
    !! Test performed on single real values.

    procedure(realCompInterface), public, nopass, pointer :: compare
        !! Pointer to a comparisson function used to perform the test.
    real(kind=wp), private :: eps
        !! Allowed margin of error between real numbers
    real(kind=wp), public :: res
        !! Real value result from some process.
    real(kind=wp), public :: tgt
        !! Target real value result for some process.
contains
    procedure, public, pass :: run => runTestRealVal
    procedure, private, nopass :: printFail => printFailTestRealVal
    final :: deleteTestRealVal
end type TestRealVal

interface TestRealVal
    !! Constructor interface for a TestRealVal object.

    module procedure newTestRealVal_name
end interface TestRealVal

interface
    function realCompInterface(res, tgt, eps) result(comp)
        !! Abstract function interface for a value comparisson function.

        import wp
        real(kind=wp), intent(in) :: res
            !! Result being tested.
        real(kind=wp), intent(in) :: tgt
            !! Target value used to compare result to.
        real(kind=wp) :: eps
            !! Allowed margin of error
        logical :: comp
            !! Return whether or not the test succeeded based on a comparrison.
    end function realCompInterface
end interface

type, public, extends(Test) :: TestRealArrVal
        !! Test performed on an array of real values.

    procedure(realArrCompInterface), nopass, pointer :: compare
        !! Pointer to a comparisson function used to perform the test.

    real(kind=wp), private :: eps
        !! Allowed margin of error between real numbers
    real(kind=wp), public, allocatable, dimension(:) :: res
        !! Real array result from some process.
    real(kind=wp), public, allocatable, dimension(:) :: tgt
        !! Target real array result for some process.
contains
    procedure, public, pass :: run => runTestRealArrVal
    procedure, private, nopass :: printFail => printFailTestRealArrVal
    final :: deleteTestRealArrVal
end type TestRealArrVal

interface TestRealArrVal
    !! Constructor interface for a TestRealArrVal object.

    module procedure newTestRealArrVal_name
end interface TestRealArrVal

interface
    function realArrCompInterface(res, tgt, eps) result(comp)
        !! Abstract function interface for an array comparisson function.
        import wp
        real(kind=wp), dimension(:), intent(in) :: res, tgt
            !! See res and tgt in TestRealArrVal class.
        real(kind=wp) :: eps
            !! Allowed margin of error
        logical :: comp
            !! Return whether or not the test succeeded based on a comparrison.
    end function realArrCompInterface
end interface

contains

! TestSuite
! =========

function newTestSuite(ts_name) result(new_ts)
    !! Construct a new test suite.

    character(len=*), intent(in) :: ts_name
        !! Name of the test suite.
    type(TestSuite) :: new_ts
        !! Return the new test suite.

    new_ts%n_tests = 0
    new_ts%test => null()

    new_ts%test_suite_name = ts_name
end function newTestSuite

subroutine deleteTestSuite(self)
    !! Destruct a test suite by deallocating its test pointer attribute.
    type(TestSuite), intent(inout) :: self

    if (associated(self%test)) deallocate(self%test)
end subroutine deleteTestSuite

subroutine runTests(self)
    !! Run all tests contained in a test suite.

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
    !! Add a Test object to the test suite and make it available for setup.

    class(TestSuite), intent(inout) :: self
    class(Test), target, intent(inout) :: ut
        !! Object derived from the Test abstract type.

    class(Test), pointer :: next

    if (this_image() == 1) self%n_tests = self%n_tests + 1

    if (associated(self%test)) allocate(next, source=self%test)
    next => self%test

    ut%next => next
    self%test => ut
end subroutine addUnitTest

subroutine addTestRealVal(self, ut, res, tgt)
    !! Compact alternative to add a TestRealVal object to the test suite.

    class(TestSuite), intent(inout) :: self
    type(TestRealVal), intent(in) :: ut
        !! An initialized TestRealVal object with the desired name.
    real(kind=wp), intent(in) :: res, tgt
        !! See TestRealVal.

    class(Test), pointer :: next

    if (this_image() == 1) self%n_tests = self%n_tests + 1

    if (associated(self%test)) allocate(next, source=self%test)
    next => self%test

    allocate(self%test, source=ut)

    associate (t => self%test)
    select type(t)
    type is (TestRealVal)
        t%next => next
        t%test_name = ut%test_name
        t%res = res
        t%tgt = tgt
    end select
    end associate
end subroutine addTestRealVal

subroutine addTestRealArrVal(self, ut, res, tgt)
    !! Compact alternative to add a TestRealArrVal object to the test suite.

    class(TestSuite), intent(inout) :: self
    type(TestRealArrVal), intent(in) :: ut
        !! An initialized TestRealArrVal object with the desired name.
    real(kind=wp), allocatable, dimension(:) :: res, tgt
        !! See TestRealArrVal.

    class(Test), pointer :: next

    if (this_image() == 1) self%n_tests = self%n_tests + 1

    if (associated(self%test)) allocate(next, source=self%test)
    next => self%test

    allocate(self%test, source=ut)

    associate (t => self%test)
    select type(t)
    type is (TestRealArrVal)
        t%next => next
        t%test_name = ut%test_name
        t%res = res
        t%tgt = tgt
    end select
    end associate
end subroutine addTestRealArrVal

! Test
! ====

! Comparison Functions

function realEq(res, tgt, eps) result(comp)
    !! Test if two real values are equal. Uses an epsilon value to account
    !! for floating point error.

    real(kind=wp), intent(in) :: res
        !! Real value result being tested.
    real(kind=wp), intent(in) :: tgt
        !! Target real value to compare result to.
    real(kind=wp) :: eps
        !! Allowed margin of error
    logical :: comp
        !! Return whether both values are equal.

    comp = abs(res-tgt) < eps
end function realEq

function realArrEq(res, tgt, eps) result(comp)
    !! Test if two real arrays are _exactly_ equal. Arrays must be of the
    !! same length and have the same values in the same positions. Uses
    !! epsilon value to account for floating point error.

    real(kind=wp), dimension(:), intent(in) :: res
        !! Real value result being tested.
    real(kind=wp), dimension(:), intent(in) :: tgt
        !! Target real value to compare result to.
    real(kind=wp) :: eps
        !! Allowed margin of error
    logical :: comp

    if (size(res) /= size(tgt)) then
        comp = .false.
        return
    end if

    comp = all(abs(res-tgt) < eps)
end function realArrEq

! Constructors

function newTestRealVal_name(ts_name, eps) result(new_ts)
    !! Construct new TestRealVal given a name.

    character(len=*), intent(in) :: ts_name
        !! Name of the new TestRealVal object.
    real(kind=wp), optional :: eps
        !! Allowed margin of error
    type(TestRealVal) :: new_ts
        !! Return new TestRealVal object.

    new_ts%test_name = ts_name
    if (present(eps)) then
        new_ts%eps = eps
    else
        new_ts%eps = default_eps
    end if
    new_ts%next => null()
    new_ts%compare => realEq
    !TODO: create a bunch of subclasses with different comparisson
    ! operators.
    new_ts%res = 0
    new_ts%tgt = 0
end function newTestRealVal_name

function newTestRealArrVal_name(ts_name, eps) result(new_ts)
    !! Construct new TestRealArrVal given a name.

    character(len=*), intent(in) :: ts_name
        !! Name of the new TestRealArrVal object.
    real(kind=wp), optional :: eps
        !! Allowed margin of error
    type(TestRealArrVal) :: new_ts
        !! Return new TestRealArrVal object.

    new_ts%test_name = ts_name
    if (present(eps)) then
        new_ts%eps = eps
    else
        new_ts%eps = default_eps
    end if
    new_ts%next => null()
    new_ts%compare => realArrEq
    !TODO: create a bunch of subclasses with different comparisson
    ! operators.
end function newTestRealArrVal_name

! Destructors

subroutine deleteTestRealVal(self)
    !! Destruct TestRealVal object by deallocating its next object pointer.

    type(TestRealVal), intent(inout) :: self
    deallocate(self%next)
end subroutine deleteTestRealVal

subroutine deleteTestRealArrVal(self)
    !! Destruct TestRealVal object by deallocating its next object pointer
    !! as well as its res and tgt arrays.

    type(TestRealArrVal), intent(inout) :: self
    deallocate(self%next)
    deallocate(self%res)
    deallocate(self%tgt)
end subroutine deleteTestRealArrVal

! Print Fail Functions

subroutine printFailTestRealVal(img, res, tgt)
    !! Print failure message of a real value comparrison.

    integer, intent(in) :: img
        !! Image where the failure occured.
    real(kind=wp), intent(in) :: res
        !! (Incorrect) result value of some procedure.
    real(kind=wp), intent(in) :: tgt
        !! (Correct) target result value of some procedure.

    print SINGLE_VAL_FMT, img, res, tgt
end subroutine printFailTestRealVal

subroutine printFailTestRealArrVal(img, res, tgt)
    !! Print failure message of a real array comparrison.

    integer, intent(in) :: img
        !! Image where the failure occured.
    real(kind=wp), dimension(:), intent(in) :: res
        !! (Incorrect) result array of some procedure.
    real(kind=wp), dimension(:), intent(in) :: tgt
        !! (Correct) target result array of some procedure.

    print ARR_VAL_IMG, img
    print '(A)', ARR_VAL_RES
    print *, res
    print '(A)', ARR_VAL_EXP
    print *, tgt
end subroutine printFailTestRealArrVal

! Run Functions

function runTestRealVal(self) result(tests_passed)
    !! Run test on real values and print summary report for images.

    class(TestRealVal), intent(in) :: self
    integer :: tests_passed
        !! Return the tests that passed up to and including this one in
        !! the linked list.

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
            if (self%compare(res[i], tgt[i], self%eps)) then
                img_passed = img_passed + 1
            else
                call self%printFail(i, res[i], tgt[i])
            end if
        end do
        print SUBTEST_END, img_passed, num_images()
    end if

    if (img_passed == num_images()) tests_passed = tests_passed + 1
end function runTestRealVal

function runTestRealArrVal(self) result(tests_passed)
    !! Run test on real arrays and print summary report for images.

    class(TestRealArrVal), intent(in) :: self
    integer :: tests_passed
        !! Return the tests that passed up to and including this one in
        !! the linked list.

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

    max_res_n = res_n
    call co_max(max_res_n)
    max_tgt_n = tgt_n
    call co_max(max_tgt_n)

    allocate(res(max_res_n)[*], tgt(max_tgt_n)[*])
    res(:res_n) = self%res
    tgt(:res_n) = self%tgt

    sync all
    img_passed = 0
    if (this_image() == 1) then
        print SUBTEST_START, self%test_name
        do i=1, num_images()
            if (self%compare(res(:res_n[i])[i], tgt(:tgt_n[i])[i], self%eps)) then
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
