
on get /users {
    view users/index
}

on get /users/* {
    set user [ param 2 ]
    set userInfo [ users::get-by-name user ]

    if { [val $userInfo id] == [val $session id] } {
        view user/edit $userInfo
    } else {
        view user/user $userInfo
    }
}

