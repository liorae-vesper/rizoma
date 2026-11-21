package rizoma.domain

import rizoma.domain.crop.activity
import rizoma.persistence.{SqliteAccess, SqliteAudit, SqliteDatabase}
import rizoma.prelude.Id

class AccessAuditSuite extends BusinessFixture:
  test("farm roles control access and changes retain actor and prior values"):
    withFixture { f =>
      import f.*
      seed()
      val accessStore = new SqliteAccess(db)
      val actor = Id.generate().fold(e => fail(e.toString), identity)
      val viewer = Id.generate().fold(e => fail(e.toString), identity)
      assert(accessStore.registerUser(actor, "Owner").isSuccess)
      assert(accessStore.registerUser(viewer, "Viewer").isSuccess)
      assert(accessStore.claimFarm(farmId, actor).isSuccess)
      assert(accessStore.setMember(actor, farmId, viewer, access.Role.Viewer).isSuccess)
      assertEquals(accessStore.can(actor, farmId, access.Permission.Manage).get, true)
      assertEquals(accessStore.can(viewer, farmId, access.Permission.Read).get, true)
      assertEquals(accessStore.can(viewer, farmId, access.Permission.Write).get, false)
      val viewerStore = new SqliteAccess(SqliteDatabase.asUser(db, viewer))
      assert(viewerStore.withPermission(viewer, farmId, access.Permission.Read)(_ => ()).isSuccess)
      assert(viewerStore.withPermission(viewer, farmId, access.Permission.Write)(_ => ()).isFailure)
      assert(viewerStore.withPermission(actor, farmId, access.Permission.Manage)(_ => ()).isFailure)
      assert(accessStore.setMember(viewer, farmId, viewer, access.Role.Owner).isFailure)
      assert(accessStore.removeMember(actor, farmId, actor).isFailure)
      val audited = SqliteDatabase.asUser(db, actor)
      val _ =
        audited.execute("UPDATE farms SET name = ? WHERE id = ?", Seq("New name", farmId.value))
      val history = new SqliteAudit(db).history("farms", farmId.value).get
      assertEquals(history.lastOption.map(_.actor), Some(actor.value))
      assert(history.lastOption.flatMap(_.before).exists(_.contains("Farm")))
      assert(history.lastOption.flatMap(_.after).exists(_.contains("New name")))
      assert(scala.util.Try(db.execute("DELETE FROM audit_history")).isFailure)
      val actorActivities = new rizoma.persistence.SqliteActivities(audited)
      assert(
        activity
          .delete(activity.Delete(activity.ActivityId.from(activityId)))(using actorActivities)
          .isRight
      )
      val deleted = new SqliteAudit(db)
        .history("activities", activityId.value)
        .get
        .lastOption
        .getOrElse(fail("missing audit"))
      assertEquals(deleted.actor, actor.value)
      assert(deleted.before.exists(_.contains("\"deleted\":0")))
      assert(deleted.after.exists(_.contains("\"deleted\":1")))
    }
