package rizoma.domain.access

enum Role:
  case Owner, Editor, Viewer

enum Permission:
  case Read, Write, Manage

object Permission:
  def allowed(role: Role, permission: Permission): Boolean = permission match
    case Permission.Read   => true
    case Permission.Write  => role == Role.Owner || role == Role.Editor
    case Permission.Manage => role == Role.Owner
