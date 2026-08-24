package rummikub.ai.cpsat

import com.google.ortools.Loader
import com.google.ortools.sat.{CpModel, CpSolver, CpSolverStatus}

class OrToolsSmokeSpec extends munit.FunSuite:

  test("CP-SAT native libraries load and solve a trivial model") {
    Loader.loadNativeLibraries()

    val model = new CpModel
    val x = model.newIntVar(0, 10, "x")
    val y = model.newIntVar(0, 10, "y")
    model.addEquality(model.newConstant(7), x)
    model.addLessOrEqual(y, x)
    model.maximize(y)

    val solver = new CpSolver
    val status = solver.solve(model)

    assert(status == CpSolverStatus.OPTIMAL || status == CpSolverStatus.FEASIBLE)
    assertEquals(solver.value(x), 7L)
    assertEquals(solver.value(y), 7L)
  }
