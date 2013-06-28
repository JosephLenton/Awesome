
# fact
#
# When called, this will run the line given, and return from
# the callers level, if that line returns a falsy value.
#
# The last value is skipped, as it is presumed to be an error message.
#
# Examples:
#
#   fact checkLogin                         {you are not logged in}
#   fact set isLoggedIn [ checkLogin ]      {you are not logged in}
#   fact hasPermissionToEdit userID blogID  {no permission to edit that}
#
proc fact args {
    set err [ lindex $args end ]
    set args [ lreplace $args end end ]

    set r [ uplevel 1 $args ]

    if { $r == {} || $r == 0 || $r == false } {
        return -level 2
    }
}

