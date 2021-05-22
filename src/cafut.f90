module cafut

use iso_fortran_env, only: real64

implicit none

integer, parameter :: wp = real64
!> Define real number kind.

integer, parameter :: NAME_LENGTH = 20
!> Define maximum name length.

real(kind=real64), parameter :: eps = 1.0d-5
!> Define margin of floating point error for real value comparissons.

character(len=*), parameter :: TEST_START = &
    '("==================== ", A, " ====================")'
!> Format of the start of a unit test.

character(len=*), parameter :: TEST_END = &
    '(A, " FINISHED WITH ", I3, "/", I3, " TESTS PASSED")'
!> Format of the end of a unit test.

character(len=*), parameter :: SUBTEST_START = &
    '("> ", A20)'
!> Format of the start of a unit test.

character(len=*), parameter :: SUBTEST_END = &
    '("finished with" , I3, "/", I3, " images passed")'
!> Format of the end of a unit test.

character(len=*), parameter :: SINGLE_VAL_FMT = &
    '(">> TEST FAILED | Image: ", I3, " | Got: ", F5.2, " | Expected: ", F5.2)'
!> Format of the failure description of an image comparing real values.

character(len=*), parameter :: ARR_VAL_IMG = &
    '(">> Image: ", I3)'
!> Format of the header for a failed test of an image comparring arrays.

character(len=*), parameter :: ARR_VAL_RES = &
    ">>> Got: "
!> String preceeding result array.

character(len=*), parameter :: ARR_VAL_EXP = &
    ">>> Expected: "
!> String preceeding expected array.

private :: rootToAll, maxToRoot, maxToAll

type, public :: TestSuite
    !> Holds tests and manages their executtion.
    !> Represents a set of procedures to test a certain feature.
    !> First node of the test linked list.

    integer, private :: n_tests
        !> Number of tests in a test suite.
    character(len=NAME_LENGTH), private :: test_suite_name
        !> Name of the test suite.
    class(Test), public, pointer :: test
        !> Current test whose attributes are available to be set.

contains

    procedure, pass :: add => addUnitTest
    procedure, pass :: runTests
    final :: deleteTestSuite

end type TestSuite

interface TestSuite
    !> Constructor interface for a TestSuite object.

    module procedure newTestSuite
end interface TestSuite

type, abstract, private :: Test
    !> Abstract class for a single test case. A node in the test linked list.

    character(len=NAME_LENGTH), public :: test_name
        !> Name of the test case.
    class(Test), private, pointer :: next
        !> Next test case or null() if this is the first test inserted (last 
        !> test in the linked list).
contains
    procedure(runInterface), deferred, pass :: run
end type Test

abstract interface 
    function runInterface(self) result(tests_passed)
        !> Abstract function interface for running a test.

        import Test
        class(Test), intent(in) :: self
            !> The test itself. The Test object should contain all information 
            !> needed to run the test.
        integer :: tests_passed
            !> Return total number of tests which passed in the linked list
            !> up to and including this test.
    end function runInterface
end interface 

type, public, extends(Test) :: TestRealVal
    !> Test performed on single real values.

    procedure(realCompInterface), public, nopass, pointer :: compare
        !> Pointer to a comparisson function used to perform the test.
    real(kind=wp), public :: res
        !> Real value result from some process.
    real(kind=wp), public :: tgt
        !> Target real value result for some process.
contains
    procedure, pass :: run => runTestRealVal
    procedure, nopass :: printFail => printFailTestRealVal
    final :: deleteTestRealVal
end type TestRealVal

interface TestRealVal
    !> Constructor interface for a TestRealVal object.

    module procedure newTestRealVal_name
end interface TestRealVal

interface
    function realCompInterface(res, tgt) result(comp)
        !> Abstract function interface for a value comparisson function.

        import wp
        real(kind=wp), intent(in) :: res
            !> Result being tested.
        real(kind=wp), intent(in) :: tgt
            !> Target value used to compare result to.
        logical :: comp
            !> Return whether or not the test succeeded based on a comparrison.
    end function realCompInterface
end interface

type, public, extends(Test) :: TestRealArrVal
        !> Test performed on an array of real values.

    procedure(realArrCompInterface), nopass, pointer :: compare
        !> Pointer to a comparisson function used to perform the test.

    real(kind=wp), allocatable, dimension(:) :: res
        !> Real array result from some process.
    real(kind=wp), allocatable, dimension(:) :: tgt
        !> Target real array result for some process.
contains
    procedure, pass :: run => runTestRealArrVal
    procedure, nopass :: printFail => printFailTestRealArrVal
    final :: deleteTestRealArrVal
end type TestRealArrVal

interface TestRealArrVal
    !> Constructor interface for a TestRealArrVal object.

    module procedure newTestRealArrVal_name
end interface TestRealArrVal

interface
    function realArrCompInterface(res, tgt) result(comp)
        !> Abstract function interface for an array comparisson function.
        import wp
        real(kind=wp), dimension(:), intent(in) :: res, tgt
            !> See res and tgt in TestRealArrVal class.
        logical :: comp
            !> Return whether or not the test succeeded based on a comparrison.
    end function realArrCompInterface
end interface

contains

! Private Helper Methods

function rootToAll(x) result(s)
    !> Helper method to spread a value from the root image (1) to all other
    !> images.

    integer, intent(in) :: x
        !> Value being spread. The only value actually spread is the one
        !> input by the root image (1). The values input by all other 
        !> images will be ignored (and not modified).
    integer :: s
        !> Return the value input by the root image (1). This will be the same
        !> for all images.

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
    !> Helper method to return the maximum value from the inputs of each image
    !> to the root image (1) only.

    integer, intent(in) :: x
        !> Input by each image. This value will be compared to the values 
        !> input by every other image.
    integer :: s
        !> Return the maximum value from all `x` inputs from all images. This
        !> will only be significant to the root image (1); the return to every
        !> other image will not be significant and thus should not be used by
        !> them in any way.

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
    !> Helper method to collect inputs from every image and return the largest
    !> of these inputs back to each image.

    integer, intent(in) :: x
        !> Value input by each image, which will be compared to the inputs of 
        !> all other images.
    integer :: s
        !> The maximum value from the ones input into `x`.

        s = rootToAll(maxToRoot(x))
end function maxToAll

! TestSuite

function newTestSuite(ts_name) result(new_ts)
    !> Construct a new test suite.

    character(len=*), intent(in) :: ts_name
        !> Name of the test suite.
    type(TestSuite) :: new_ts
        !> Return the new test suite.

    new_ts%n_tests = 0
    new_ts%test => null()

    new_ts%test_suite_name = ts_name
end function newTestSuite

subroutine deleteTestSuite(self)
    !> Destruct a test suite by deallocating its test pointer attribute.
    type(TestSuite), intent(inout) :: self

    deallocate(self%test)
end subroutine deleteTestSuite

subroutine runTests(self)
    !> Run all tests contained in a test suite.

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
    !> Add a Test object to the test suite and make it available for setup.

    class(TestSuite), intent(inout) :: self
    class(Test), target, intent(inout) :: ut
        !> Object derived from the Test abstract type.

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
    !> Test if two real values are equal. Uses an epsilon value to account
    !> for floating point error.

    real(kind=wp), intent(in) :: res
        !> Real value result being tested. 
    real(kind=wp), intent(in) :: tgt
        !> Target real value to compare result to.
    logical :: comp
        !> Return whether both values are equal.

    comp = abs(res-tgt) < eps
end function realEq

function realArrEq(res, tgt) result(comp)
    !> Test if two real arrays are _exactly_ equal. Arrays must be of the
    !> same length and have the same values in the same positions. Uses
    !> epsilon value to account for floating point error.

    real(kind=wp), dimension(:), intent(in) :: res
        !> Real value result being tested. 
    real(kind=wp), dimension(:), intent(in) :: tgt
        !> Target real value to compare result to.
    logical :: comp

    if (size(res) /= size(tgt)) then
        comp = .false.
        return
    end if

    comp = all(abs(res-tgt) < eps)
end function realArrEq

!! Constructors

function newTestRealVal_name(ts_name) result(new_ts)
    !> Construct new TestRealVal given a name.

    character(len=*), intent(in) :: ts_name
        !> Name of the new TestRealVal object.
    type(TestRealVal) :: new_ts
        !> Return new TestRealVal object.

    new_ts%test_name = ts_name
    new_ts%next => null()
    new_ts%compare => realEq
    !TODO: create a bunch of subclasses with different comparisson
    ! operators.
    new_ts%res = 0
    new_ts%tgt = 0
end function newTestRealVal_name

function newTestRealArrVal_name(ts_name) result(new_ts)
    !> Construct new TestRealArrVal given a name.

    character(len=*), intent(in) :: ts_name
        !> Name of the new TestRealArrVal object.
    type(TestRealArrVal) :: new_ts
        !> Return new TestRealArrVal object.

    new_ts%test_name = ts_name
    new_ts%next => null()
    new_ts%compare => realArrEq
    !TODO: create a bunch of subclasses with different comparisson
    ! operators.
end function newTestRealArrVal_name

!! Destructors

subroutine deleteTestRealVal(self) 
    !> Destruct TestRealVal object by deallocating its next object pointer.

    type(TestRealVal), intent(inout) :: self
    deallocate(self%next)
end subroutine deleteTestRealVal

subroutine deleteTestRealArrVal(self) 
    !> Destruct TestRealVal object by deallocating its next object pointer
    !> as well as its res and tgt arrays.

    type(TestRealArrVal), intent(inout) :: self
    deallocate(self%next)
    deallocate(self%res)
    deallocate(self%tgt)
end subroutine deleteTestRealArrVal

!! Print Fail Functions

subroutine printFailTestRealVal(img, res, tgt)
    !> Print failure message of a real value comparrison.

    integer, intent(in) :: img
        !> Image where the failure occured.
    real(kind=wp), intent(in) :: res
        !> (Incorrect) result value of some procedure.
    real(kind=wp), intent(in) :: tgt
        !> (Correct) target result value of some procedure.

    print SINGLE_VAL_FMT, img, res, tgt
end subroutine printFailTestRealVal

subroutine printFailTestRealArrVal(img, res, tgt)
    !> Print failure message of a real array comparrison.

    integer, intent(in) :: img
        !> Image where the failure occured.
    real(kind=wp), dimension(:), intent(in) :: res
        !> (Incorrect) result array of some procedure.
    real(kind=wp), dimension(:), intent(in) :: tgt
        !> (Correct) target result array of some procedure.

    print ARR_VAL_IMG, img
    print '(A)', ARR_VAL_RES 
    print *, res
    print '(A)', ARR_VAL_EXP
    print *, tgt
end subroutine printFailTestRealArrVal

!! Run Functions

function runTestRealVal(self) result(tests_passed)
    !> Run test on real values and print summary report for images.

    class(TestRealVal), intent(in) :: self
    integer :: tests_passed
        !> Return the tests that passed up to and including this one in
        !> the linked list.

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

function runTestRealArrVal(self) result(tests_passed)
    !> Run test on real arrays and print summary report for images.

    class(TestRealArrVal), intent(in) :: self
    integer :: tests_passed
        !> Return the tests that passed up to and including this one in
        !> the linked list.

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
