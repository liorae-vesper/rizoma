package rizoma.persistence

import rizoma.domain.access.{Permission, Role}
import rizoma.prelude.Id
import rizoma.prelude.jdbc.{Database, SqlSession}

import scala.util.Try

object SqliteAccess:
  private[persistence] def initialize(session: SqlSession): Unit =
    val _ =
      session.execute("CREATE TABLE IF NOT EXISTS users (id TEXT PRIMARY KEY, name TEXT NOT NULL)")
    val _ = session.execute(
      "CREATE TABLE IF NOT EXISTS farm_memberships (farm_id TEXT NOT NULL REFERENCES farms(id), user_id TEXT NOT NULL REFERENCES users(id), role TEXT NOT NULL CHECK (role IN ('Owner','Editor','Viewer')), PRIMARY KEY(farm_id, user_id))"
    )

final class SqliteAccess(db: Database):
  @SuppressWarnings(Array("scalafix:DisableSyntax"))
  def registerUser(id: Id, name: String): Try[Unit] = Try(db.transaction { session =>
    if session.queryOne("SELECT rizoma_actor()")(_.getString(1)) != Some("system") then
      throw new IllegalAccessException("System actor required to register users")
    if name.trim.isEmpty then throw new IllegalArgumentException("User name cannot be blank")
    val _ = session.execute("INSERT INTO users (id, name) VALUES (?, ?)", Seq(id.value, name.trim))
  })

  @SuppressWarnings(Array("scalafix:DisableSyntax"))
  def claimFarm(farm: Id, owner: Id): Try[Unit] = Try(db.transaction { session =>
    if session.queryOne("SELECT rizoma_actor()")(_.getString(1)) != Some("system") then
      throw new IllegalAccessException("System actor required to claim a farm")
    val count = session.execute(
      "INSERT INTO farm_memberships (farm_id, user_id, role) SELECT ?, ?, 'Owner' WHERE EXISTS (SELECT 1 FROM farms WHERE id = ?) AND EXISTS (SELECT 1 FROM users WHERE id = ?) AND NOT EXISTS (SELECT 1 FROM farm_memberships WHERE farm_id = ?)",
      Seq(farm.value, owner.value, farm.value, owner.value, farm.value)
    )
    if count != 1 then throw new IllegalStateException("Farm cannot be claimed")
  })

  def can(user: Id, farm: Id, permission: Permission): Try[Boolean] = Try {
    db.queryOne(
      "SELECT role FROM farm_memberships WHERE farm_id = ? AND user_id = ?",
      Seq(farm.value, user.value)
    )(_.getString(1))
      .flatMap(raw => Role.values.find(_.toString == raw))
      .exists(Permission.allowed(_, permission))
  }

  /** The callback is trusted application code and must scope its queries to this farm. */
  @SuppressWarnings(Array("scalafix:DisableSyntax"))
  def withPermission[A](user: Id, farm: Id, permission: Permission)(run: SqlSession => A): Try[A] =
    Try(db.transaction { session =>
      val identity = session.queryOne("SELECT rizoma_actor()")(_.getString(1)).getOrElse("")
      val role = session
        .queryOne(
          "SELECT role FROM farm_memberships WHERE farm_id = ? AND user_id = ?",
          Seq(farm.value, user.value)
        )(_.getString(1))
        .flatMap(raw => Role.values.find(_.toString == raw))
      if identity != user.value || !role.exists(Permission.allowed(_, permission)) then
        throw new IllegalAccessException("Farm permission denied")
      run(session)
    })

  @SuppressWarnings(Array("scalafix:DisableSyntax"))
  private def ensureOwner(session: SqlSession, actor: Id, farm: Id): Unit =
    val identity = session.queryOne("SELECT rizoma_actor()")(_.getString(1)).getOrElse("")
    val owns = session
      .queryOne(
        "SELECT 1 FROM farm_memberships WHERE farm_id = ? AND user_id = ? AND role = 'Owner'",
        Seq(farm.value, actor.value)
      )(_ => true)
      .getOrElse(false)
    if !owns || (identity != "system" && identity != actor.value) then
      throw new IllegalAccessException("Owner role required")

  @SuppressWarnings(Array("scalafix:DisableSyntax"))
  def setMember(actor: Id, farm: Id, user: Id, role: Role): Try[Unit] = Try(
    db.transaction { session =>
      ensureOwner(session, actor, farm)
      val owners = session
        .queryOne(
          "SELECT COUNT(*) FROM farm_memberships WHERE farm_id = ? AND role = 'Owner'",
          Seq(farm.value)
        )(_.getInt(1))
        .getOrElse(0)
      val wasOwner = session
        .queryOne(
          "SELECT 1 FROM farm_memberships WHERE farm_id = ? AND user_id = ? AND role = 'Owner'",
          Seq(farm.value, user.value)
        )(_ => true)
        .getOrElse(false)
      if wasOwner && role != Role.Owner && owners <= 1 then
        throw new IllegalStateException("Farm must retain an owner")
      val _ = session.execute(
        "INSERT INTO farm_memberships (farm_id, user_id, role) VALUES (?, ?, ?) ON CONFLICT(farm_id,user_id) DO UPDATE SET role = excluded.role",
        Seq(farm.value, user.value, role.toString)
      )
    }
  )

  @SuppressWarnings(Array("scalafix:DisableSyntax"))
  def removeMember(actor: Id, farm: Id, user: Id): Try[Unit] = Try(db.transaction { session =>
    ensureOwner(session, actor, farm)
    val assigned = session
      .queryOne(
        "SELECT 1 FROM tasks WHERE farm_id = ? AND assignee = ? AND deleted = 0 AND status IN ('Planned', 'InProgress') LIMIT 1",
        Seq(farm.value, user.value)
      )(_ => true)
      .getOrElse(false)
    if assigned then throw new IllegalStateException("Member has assigned tasks")
    val count = session.execute(
      "DELETE FROM farm_memberships WHERE farm_id = ? AND user_id = ? AND (role <> 'Owner' OR (SELECT COUNT(*) FROM farm_memberships WHERE farm_id = ? AND role = 'Owner') > 1)",
      Seq(farm.value, user.value, farm.value)
    )
    if count != 1 then throw new IllegalStateException("Membership not found or last owner")
  })
