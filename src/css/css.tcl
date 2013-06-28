
# 
# A package for building CSS sheets, and storing them centrally in one place.
#
package provide css 1.0

namespace eval ::css {
    namespace export prefix sheet rule
    namespace ensemble create

    variable Prefixed
    variable CSS 
    variable IsDebug

    array set Prefixed {}
    array set CSS {}
    set IsDebug 0
}

proc ::css::debug {args} {
    variable IsDebug

    if { [llength args] == 0 } {
        set IsDebug 1
    } else {
        set isDebug [lindex $args 0]

        if { $isDebug } {
            set IsDebug 1
        } else {
            set IsDebug 0
        }
    }
}

proc ::css::prefix {args} {
    variable Prefixed

    foreach {property} $args {
        set Prefixed($property) 1
    }
}

proc ::css::sheetStr {rules} {
    variable IsDebug

    set str {}

    #array set rs $rules
    foreach {clause rule} $rules {
        append str [ css::rule $clause $rule ]

        if { $IsDebug == 1 } {
            append str "\n"
        }
    }

    return $str
}

proc ::css::list {} {
    variable CSS

    set names [list]
    foreach { sheetName rules } { array get CSS } {
        lappend $names $sheetName 
    }

    return $names
}

proc ::css::get {name} {
    variable CSS

    return CSS($name)
}

proc ::css::sheet {args} {
    variable CSS

    if { [llength $args] == 1 } {
        set name style
        set rules [lindex $args 0]
    } else {
        set name  [lindex $args 0]
        set rules [lindex $args 1]
    }

    set str [ css::sheetStr $rules ]

    set currRules [ get CSS($name) ]
    lappend $currRules $str

    return $str
}

proc ::css::rule {rule props} {
    variable Prefixed
    variable IsDebug

    set str $rule

    if { $IsDebug == 1 } {
        append str " \{"
        append str "\n"
    } else {
        append str "\{"
    }

    foreach { type value } $props {
        if { $IsDebug == 1 } {
            append str "    "
        }

        if { [array get Prefixed $type] == 1 } {
            if { $IsDebug == 1 } {
                append str "-webkit-$type: $value; -moz-$type: $value; -ms-$type: $value; -o-$type: $value; $type: $value;"
            } else {
                append str "-webkit-$type:$value;-moz-$type:$value;-ms-$type:$value;-o-$type:$value;$type:$value;"
            }
        } else {
            if { $IsDebug == 1 } {
                append str "$type\t: $value\t;"
            } else {
                append str "$type:$value;"
            }
        }

        if { $IsDebug == 1 } {
            append str "\n"
        }
    }

    if { $IsDebug == 1 } {
        append str " \}"
    } else {
        append str "\}"
    }

    return $str
}

#
# Prints out all of the CSS sheets and rules set, in one.
#
proc ::css::print {} {
    variable IsDebug
    variable CSS

    foreach { name rules } [ array get CSS ] {
        if { $IsDebug } {
            puts "\n/**\n * $name\n */\n"
            puts [ join $rules "\n" ]
            puts {}
        } else {
            puts [ join $rules {} ]
        }
    }
}

