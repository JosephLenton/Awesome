
package require struct::set

package provide html 1.0

namespace html {
    namespace export html
    namespace ensemble create

    variable htmlTags
    variable viewStore

    strict::set include htmlTags div span p article header footer
    strict::set include htmlTags h1 h2 h3 h4 h5
    strict::set include htmlTags ul ol li
    strict::set include htmlTags canvas video audio
    strict::set include htmlTags table tbody thead th tr td
    strict::set include htmlTags form input textarea label select option
    strict::set include htmlTags code pre

    array set viewStore {}
}

proc html {rules} {
    set str {}

    foreach html $rules {
        append str [ htmlRule $html ]
    }
    
    return str
}

#
#    html div {
#        [ html h1 my article title ]
#    
#        here is some example text
#    
#        [ html article 
#            the body of the article goes here
#        ]
#   }
#
proc html {id} {
    variable htmlTags

    if { struct::set contains htmlTags $id } {
        set tag $id
    } else {
        set tag div
    }

    return "<$tag>$content</$tag>"
}
