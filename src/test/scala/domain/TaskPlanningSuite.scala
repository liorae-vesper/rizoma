package rizoma.domain

import rizoma.domain.crop.activity
import rizoma.persistence.{SqliteAccess, SqliteTasks}
import rizoma.prelude.Id

class TaskPlanningSuite extends BusinessFixture:
  test("planned work completes once and creates a journal activity atomically"):
    withFixture { f =>
      import f.*
      seed()
      def fresh = Id.generate().fold(e => fail(e.toString), identity)
      val tasks = new SqliteTasks(db)
      val access = new SqliteAccess(db)
      val worker = fresh
      assert(access.registerUser(worker, "Worker").isSuccess)
      assert(access.claimFarm(farmId, worker).isSuccess)
      val taskId = fresh
      val command = task.Create
        .from(
          farmId,
          "Weed",
          Some(at),
          Some(worker),
          Some(plot.PlotId.from(plotId)),
          Some(activityInput.cycleId)
        )
        .fold(e => fail(e), identity)
      assertEquals(
        task.create(command)(using tasks, generator(taskId)),
        Right(task.TaskId.from(taskId))
      )
      val secondWorker = fresh
      assert(access.registerUser(secondWorker, "Second").isSuccess)
      assert(
        access.setMember(worker, farmId, secondWorker, rizoma.domain.access.Role.Editor).isSuccess
      )
      val assigned = task.Create
        .from(farmId, "Irrigate", None, Some(secondWorker), None, None)
        .fold(e => fail(e), identity)
      val assignedId = fresh
      assert(task.create(assigned)(using tasks, generator(assignedId)).isRight)
      assert(access.removeMember(worker, farmId, secondWorker).isFailure)
      assertEquals(
        task.status(task.TaskId.from(assignedId), task.Status.Cancelled)(using tasks),
        Right(())
      )
      assert(access.removeMember(worker, farmId, secondWorker).isSuccess)
      assert(task.create(assigned)(using tasks, generator(fresh)).isLeft)
      val activityId = fresh
      val journal = activity.Registration
        .from(activityInput.cycleId, "Weeding", at)
        .fold(e => fail(e.toString), identity)
      assertEquals(
        task.complete(task.TaskId.from(taskId), Some(journal))(using tasks, generator(activityId)),
        Right(())
      )
      val found = task
        .get(task.TaskId.from(taskId))(using tasks)
        .fold(e => fail(e.toString), identity)
        .getOrElse(fail("task"))
      assertEquals(found.status, task.Status.Completed)
      assertEquals(found.activityId, Some(activity.ActivityId.from(activityId)))
      assert(
        activity
          .get(activity.ActivityId.from(activityId))(using activities)
          .map(_.nonEmpty) == Right(true)
      )
      assert(
        task.complete(task.TaskId.from(taskId), Some(journal))(using tasks, generator(fresh)).isLeft
      )
      assertEquals(task.listByFarm(farmId)(using tasks).map(_.size), Right(2))
      assert(
        activity
          .delete(activity.Delete(activity.ActivityId.from(activityId)))(using activities)
          .isLeft
      )
    }

  test("failed activity creation rolls back task completion"):
    withFixture { f =>
      import f.*
      seed()
      def fresh = Id.generate().fold(e => fail(e.toString), identity)
      val tasks = new SqliteTasks(db)
      val id = fresh
      val command = task.Create
        .from(farmId, "Work", None, None, None, Some(activityInput.cycleId))
        .fold(e => fail(e), identity)
      assert(task.create(command)(using tasks, generator(id)).isRight)
      val wrongCycle = rizoma.domain.crop.cycle.CycleId.from(fresh)
      val event =
        activity.Registration.from(wrongCycle, "Weeding", at).fold(e => fail(e.toString), identity)
      assert(task.complete(task.TaskId.from(id), Some(event))(using tasks, generator(fresh)).isLeft)
      assertEquals(
        task.get(task.TaskId.from(id))(using tasks).map(_.map(_.status)),
        Right(Some(task.Status.Planned))
      )
    }
