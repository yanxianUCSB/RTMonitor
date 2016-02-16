# menu
    menu.config <- function(default = 9) {
        # print main menu
        cat("---------Main-----------\n")
        cat(' 1 . Modify \n')
        cat(' 2 . Save \n')
        cat(' 3 . Load \n')
        cat(' 9 . Continue \n')
        cat(' 0 . Back \n')
        
        while (T) {
            choice <- input.number('>> [9] ', default = default)
            if (choice %in% c(1,2,3,9,0))
                break
            tryagain()
        }
        return(choice)
    }
    menu.edit <- function(default = 1) {
        # Editing Menu
        cat("--------Main/Edit/------\n")
        cat(' 1 . Modify \n')
        cat(' 2 . Add \n')
        cat(' 3 . Delete \n')
        cat(' 0 . Back \n')
        
        while (T) {
            choice <- input.number('>> [1] ', default = default)
            if (choice %in% c(1,2,3,0))
                break
            tryagain()
        }
        return(choice)
    }
    menu.edit.modify <- function(length.of.object, default = 0) {
        # Modifying Menu
        cat("--------Main/1-Edit/1-Modify--------------------\n")
        cat("Enter config number above to change.            \n")
        cat("When finished, Enter 0                          \n")
        
        while (T) {
            choice <- input.number('>> [0] ', default = default)
            if (choice %in% 0:length.of.object)
                break
            tryagain()
        }
        return(choice)
    }
    menu.edit.delete <- function(length.of.object, default = 0) {
        # Deleting Menu
        cat("--------Main/1-Edit/2-Delete/-----------\n")
        cat("Enter config number to delete.          \n")
        cat("When finished, Enter 0                          \n")
        
        while (T) {
            choice <- input.number('>> [0] ', default = default)
            if (choice %in% 0:length.of.object)
                break
            tryagain()
        }
        return(choice)
    }
    menu.edit.add <- function(default = '0') {
        # Adding Menu
        cat("-----------------Main/Edit/Add/---------------\n")
        cat("Enter parameter name to add.                  \n")
        cat('Enter 0 to cancel and go back                 \n')
        
        choice <- input.string('>> ')
    }
    menu.save <- function(default = 0) {
        cat("-------Main/Save/-------\n")
        cat(' 1 . Save as default    \n')
        cat(' 2 . Save as ...        \n')
        cat(' 0 . Cancel             \n')
        
        while (T) {
            choice <- input.number('>> [0] ', default = default)
            if (choice %in% c(1,2,0))
                break
            tryagain()
        }
        return(choice)
    }
