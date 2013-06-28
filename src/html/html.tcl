
package require struct::set

package provide html 1.0

namespace eval ::html {
    namespace export html htmlStr
    namespace ensemble create
}

# 
# This builds the HTML, and then returns it as a string.
#
proc html::htmlStr args {
    set tagMeta [ lindex $args 0 ]
    set content [ lreplace $args 0 0 ]
    set tag [ getTagType $tagMeta ]

    return "<[ getOpenTag $tagMeta ]>[ join $content {} ]</$tag>"
}

#
# This is the *same* as html::htmlStr, except the result is printed instead of
# returned. That is the only difference.
#
#    [ html div 
#        [ html h1 my article title ]
#    
#        here is some example text
#    
#        [ html article 
#            the body of the article goes here
#        ]
#    ]
#
proc html::html args {
    puts [ html::htmlStr {*}$args ]
}

proc getTagType tag {
    # find out the tag type
    set firstChar [ string index $tag 0 ]

    if { $firstChar == "." || $firstChar == "#" } {
        return div
    } else {
        set nextDot  [ string first "." $tag ]
        set nextHash [ string first "#" $tag ]

        if { $nextDot == -1 } {
            if { $nextHash != -1 } {
                return [ string range $tag 0 [ expr $nextHash -1 ] ]
            } else {
                return $tag
            }
        } elseif { $nextHash == -1 } {
            if { $nextDot != -1 } {
                return [ string range $tag 0 [ expr $nextDot -1 ] ]
            } else { 
                return $tag
            }
        } else {
            return [ string range $tag 0 [ expr min($nextDot, $nextHash) -1 ] ]
        }

        return div
    }
}

proc getOpenTag tag {
    set class {}
    set id {}

    # chomp along the tag, collecting classes and id's
    set i 0
    set lastI 0
    set nextDot -1
    set nextHash -1
    set isClass 0
    while { $i < [string length $tag] } {
        set nextDot  [ expr [ string first "." $tag $i ] + 1 ]
        set nextHash [ expr [ string first "#" $tag $i ] + 1 ]

        if { $nextDot != 0 && ( $nextHash == 0 || $nextDot < $nextHash ) } {
            set i $nextDot
            set nextClass 1

        } elseif { $nextHash != 0 } {
            set i $nextHash
            set nextClass 0

        # no IDs or Classes in the string
        } elseif { $lastI == 0 } {
            break

        # there are IDs and Classes, but we are at the end
        } else {
            if { $isClass } {
                append class [ string range $tag $lastI end ]
            } else {
                append id [ string range $tag $lastI end ]
            }

            break
        }

        # chomp the next class/ID, and then continue
        if { $lastI != 0 && $lastI < [expr $i-2] } {
            if { $isClass } {
                append class [ string range $tag $lastI [expr $i-2] ]
            } else {
                append id [ string range $tag $lastI [expr $i-2] ]
            }
        }

        # info for the next iteration, to chomp from this to the next location
        set isClass $nextClass
        set lastI $i
    }

    # finally build the tag it's self
    set openTag "[ getTagType $tag ]"
    if { $id != {} } {
        append openTag " id=\"$id\""
    }

    if { $class != {} } {
        append openTag " class=\"$class\""
    }

    return $openTag
}

