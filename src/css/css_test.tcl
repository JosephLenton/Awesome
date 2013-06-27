
source {./css.tcl}

css::debug true

set back black

css::sheet grid "
    .background {
        background $back
        color   red
        width   50%
        height  100%
        display block
    }
"

css::print

