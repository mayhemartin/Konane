#light
module konane
    exception UnknownError of string
    //enumerable types
    module Definitions =
        type KEAELE =
            |EMPTY = 3
            |KEA = 2
            |ELE = 1
        
        //individual peice status
        type kanaka = {
            mutable Player : KEAELE
        } with
            //print whose turn it is
            member this.printTurn() = 
                match this.Player with
                |KEAELE.ELE -> printf "ELE"
                |KEAELE.KEA -> printf "KEA"
                |_ -> printf ""
            //print formated board puka based on current enum
            member this.print()=
                match this.Player with
                |KEAELE.KEA -> printf " W |"
                |KEAELE.ELE -> printf " B |"
                |KEAELE.EMPTY -> printf "   |"
                |_ -> printf ""
            //set a puka status to empty
            member this.remove() =
                this.Player <- KEAELE.EMPTY
            //set a puka status to a passed in enum status
            member this.set(turn : KEAELE) =
                this.Player <- turn
            //test if a puka is empty
            member this.empty():bool = 
                if this.Player = KEAELE.EMPTY then
                    true
                else
                    false
        //given an tuple value pair, checks it against an array of pairs
        //used to prevent duplicates, and enforces proper user selection
        let valid (possible, value) = 
            if Array.exists (fun elem -> value = elem)possible then true else false

        //Game Board details (of type Record)
        type game = {
            Rows : int; //# of rows
            Cols : int; //# of columns
            mutable Papamu: kanaka [,] //Papamu = board, kanaka=peices
        } with
            //sets a peice to EMPTY, calls kanaka member function
            member this.remove(row:int, col:int) =
                this.Papamu.[row,col].remove()
            //Fills the papamu accordingly, alternating rows of alternating peices
            member this.populate()=
                for i in 0 .. this.Rows-1 do
                    for j in 0 .. this.Cols-1 do
                        if (i%2) <> 1 then
                            match (j%2) with
                            |0 -> this.Papamu.[i,j] <- {Player=KEAELE.ELE}
                            |_ -> this.Papamu.[i,j] <- {Player=KEAELE.KEA}
                        else 
                        match (j%2) with
                            |0 -> this.Papamu.[i,j] <- {Player=KEAELE.KEA}
                            |_ -> this.Papamu.[i,j] <- {Player=KEAELE.ELE}
            //Retrieves all puka with an EMPTY status
            member this.getEmpty() = 
                let mutable empty = [||]
                for r in 0 .. this.Cols-1 do
                    for c in 0 .. this.Rows-1 do
                        if this.Papamu.[r,c].empty() then
                            empty <- Array.append empty [|(r,c)|]
                empty //returns array
            //Retreives the first possible moves
            //ELE - (Black) starts the game
            //Kea - (White) must also remove a peice based on ELE's first selection
            member this.getFirstMoves(turn : KEAELE) = 
                let mutable moves = [||]
                if turn = KEAELE.ELE then
                    moves <- Array.append moves [|(0,0)|]
                    moves <- Array.append moves [|(this.Rows-1, this.Cols-1)|]
                    moves <- Array.append moves [|(this.Rows/2, this.Cols/2)|]
                    moves <- Array.append moves [|((this.Rows/2)-1, (this.Cols/2)-1)|]
                else
                    let r,c = (this.getEmpty()).[0]
                    match r with
                        |r when r=0 || r=this.Rows-1 ->
                            moves <- Array.append moves [|(this.Rows-1,0)|]
                            moves <- Array.append moves [|(0, this.Cols-1)|]
                        |r when r=this.Rows/2 || r=(this.Rows/2)-1 ->
                            moves <- Array.append moves [|((this.Rows/2)-1,this.Cols/2)|]
                            moves <- Array.append moves [|(this.Rows/2, (this.Cols/2)-1)|]
                        |_ -> printf "error in first move pattern matching"

                moves //Return array of possible moves
            //Retreive all peices that may move
            member this.getPeices(turn: KEAELE) = 
                let empty = this.getEmpty()
                let checkFor = if turn = KEAELE.KEA then KEAELE.ELE else KEAELE.KEA
                let checkAgainst = if turn = KEAELE.KEA then KEAELE.KEA else KEAELE.ELE
                let mutable moves = [||]
                for ea in empty do
                    let r,c = ea
                    //check four directions
                    if r-1 > 0 then
                        if this.Papamu.[r-1,c].Player = checkFor && this.Papamu.[r-2,c].Player = turn then
                            if not (valid(moves,(r-2,c))) then
                                moves <- Array.append moves [|(r-2,c)|]
                    if r+1 < this.Rows-1 then
                        if this.Papamu.[r+1,c].Player = checkFor && this.Papamu.[r+2,c].Player = turn then
                            if not (valid(moves,(r-2,c))) then
                                moves <- Array.append moves [|(r+2,c)|]
                    if c-1 > 0 then
                        if this.Papamu.[r,c-1].Player = checkFor && this.Papamu.[r,c-2].Player = turn then
                            if not (valid(moves,(r-2,c))) then
                                moves <- Array.append moves [|(r,c-2)|]
                    if c+1 < this.Cols-1 then
                        if this.Papamu.[r,c+1].Player = checkFor && this.Papamu.[r,c+2].Player = turn then
                            if not (valid(moves,(r-2,c))) then
                                moves <- Array.append moves [|(r,c+2)|]
                moves
            //Given a peice and a turn, retreives all possible moves for that peice
            member this.getMoves(r:int, c:int, turn:KEAELE)=
                let checkFor = if turn=KEAELE.KEA then KEAELE.ELE else KEAELE.KEA
                let mutable moves = [||]
                if r-1 > 0 then
                    if this.Papamu.[r-1,c].Player = checkFor && this.Papamu.[r-2,c].Player = KEAELE.EMPTY then
                        moves <- Array.append moves [|(r-2,c)|]
                if r+1 < this.Rows-1 then
                    if this.Papamu.[r+1,c].Player = checkFor && this.Papamu.[r+2,c].Player = KEAELE.EMPTY then
                        moves <- Array.append moves [|(r+2,c)|]
                if c-1 > 0 then
                    if this.Papamu.[r,c-1].Player = checkFor && this.Papamu.[r,c-2].Player = KEAELE.EMPTY then
                        moves <- Array.append moves [|(r,c-2)|]
                if c+1 < this.Cols-1 then
                    if this.Papamu.[r,c+1].Player = checkFor && this.Papamu.[r,c+2].Player = KEAELE.EMPTY then
                        moves <- Array.append moves [|(r,c+2)|]
                moves
            //Game board member method to display arrays, whether peices to move, or moves for a peice
            member this.displayMoves(moves, turn: KEAELE)=
                Array.iter (fun (a,b) -> printf "(%d,%d) " a b) moves
                printf "\n"
                moves
            //Display's entire board, in a formatted structure
            member this.displayBoard()=
                //display column numbers
                printf "\n     0  "
                for c in 1 .. this.Cols-1 do
                    printf " %d  " c
                //display dividers between rows
                for r in 0 .. this.Rows-1 do
                    printf "\n   -----"
                    for i in 0 .. this.Cols-2 do
                        printf "----"
                    //display row numbers
                    printf "\n %d |" r
                    //display columns
                    for c in 0 .. this.Cols-1 do
                        this.Papamu.[r,c].print()
                //display bottom border
                printf "\n   -----"
                for c in 0 .. this.Cols-2 do
                    printf "----"
                printf "\n"

    module Functions =
        exception InvalidChoice of string
        //Creates the board
        let newGame(row:int, col:int) = {Definitions.Rows=row; Definitions.Cols=col; Definitions.Papamu=Array2D.zeroCreate row col}
        //prints who the current turn belongs to
        let printTurn(turn : Definitions.KEAELE) = 
            match turn with
            |Definitions.KEAELE.ELE -> printf "ELE"
            |Definitions.KEAELE.KEA -> printf "KEA"
            |_ -> printf ""
        //prints who the current turn does not belong to
        let printNotTurn(turn : Definitions.KEAELE) = 
            match turn with
            |Definitions.KEAELE.ELE -> printf "KEA"
            |Definitions.KEAELE.KEA -> printf "ELE"
            |_ -> printf ""

        //Reads a tuple pair from user input in the console
        //error checking for format and value
        //input must have the format or r c
        //no spaces after c, or you will be prompted to re-enter
        let readTuple(size : int, possible) = 
            let mutable err = false
            let mutable value = (0,0)
            while not err do
                try
                    let input = System.Console.ReadLine();
                    let coords = input.Split[|' '|];
                    if Array.length(coords) < 2 then
                        raise(InvalidChoice("Invalid coordinates, please try again:"))
                    elif Array.length(coords) > 2 then
                        raise(InvalidChoice("Invalid coordinates, please try again:"))
                    else
                        value <- System.Int32.Parse(coords.[0]),System.Int32.Parse(coords.[1]);
                        if fst(value)<0 || snd(value)<0 || fst(value)>size || snd(value)>size then
                            raise(InvalidChoice("Coordinates out of range, please try again:"))

                        if not (Definitions.valid(possible,value)) then
                            raise(InvalidChoice("That is not an option, please try again:"))

                        err <- true;
                with
                | InvalidChoice errMsg -> printf "%s" errMsg
                | :? System.FormatException -> printf "Please enter valid numbers:"
            value
        //Retrieves a y/n option from the user
        //can be characters or any variation there of....
        //with the dependency that the answer starts with a y or n
        let getAnswer(mssg:string) = 
            let mutable err = false
            let mutable answer = true
            while not err do
                try 
                    printf "%s" mssg   
                    let opt = System.Console.ReadLine();
                    match opt.[0] with
                    |opt when opt = 'y' || opt = 'Y' -> 
                        err <- true
                    |opt when opt = 'n' || opt = 'N' ->
                        answer <- false 
                        err <- true
                    |_ -> raise(InvalidChoice("Unrecognized answer, please try again!!\n"))

                with
                | :? System.FormatException -> printfn "Please enter y/n !!\n" 
                |InvalidChoice errMsg -> printf "%s" errMsg
            answer      

    module play = 
        exception InvalidChoice of string
        let mutable contGame = true
        //Its game, time !!!
        //loop for game continuation upon a winner
        while contGame do //'game' loop
            contGame <- false
            let mutable err = false
            let mutable reset = false            
            let mutable size = 4
            printfn "\nWelcome to Konane (Hawaian Checkers)"
            printfn "Some quick rules:"
            printfn "   - for a 4 X 4 board enter 4"
            printfn "   - for a 6 X 6 board enter 6"
            printfn "   - for a 8 X 8 board enter 8"
            printfn "   - etc.....\n\nThe board must be more than 3 and divisible by 2"
            printfn "What size board would you like to play on?";
            //Board size selection, you can change your mind
            while not reset do
                err <- false
                //Board size must be a valid number
                while not err do
                    try
                        printf "Board size:"
                        size <- System.Int32.Parse(System.Console.ReadLine());
                        if size < 4 then
                            raise(InvalidChoice("Must be greater than 4, please try again.\n"))

                        match size%2 with
                        |0 -> err <- true
                        |_ -> raise(InvalidChoice("Must be an even number, please try again.\n"))
                    with
                    | :? System.FormatException -> printfn "Please enter a valid number!!\n"
                    | InvalidChoice errMsg -> printf "%s" errMsg

                err <- false
                if Functions.getAnswer((sprintf "You chose %d X %d, is this correct?" size size)) then
                    reset <- true
                    err <- true
                else
                    reset <- false
                    err <- true

            printfn "\nWhen making your selection, please use the pattern in the example below."
            printfn "EX - Choose a peice to remove: r c\n(where r = row and c = column)"
            let mutable winner = false  //keep going until there is a winner
            //let mutable peice = [||]    //possible peices
            let mutable select = (0,0)
            //let mutable move = [||]     //possible moves
            let mutable possible = [||] //possible selections for peices and moves, if its not in here, you can't choose it
            let mutable pr = 0          //peice row
            let mutable pc = 0          //peice column
            let mutable mr = 0          //move row
            let mutable mc = 0          //move column

            //create the board
            let game = Functions.newGame(size,size);
            //populate the board
            game.populate();

            //start the turns, either ELE (Black) or KEA(White)
            let mutable turn = Definitions.KEAELE.ELE;

            //ELE choose a peice to remove from the board
            game.displayBoard();
            printf "Possible peices to remove:"
            possible <- game.displayMoves(game.getFirstMoves(turn),turn);
            Functions.printTurn(turn);
            printf " - Choose a peice to remove:" 
            select <- Functions.readTuple(size,possible);
            pr <- fst(select);
            pc <- snd(select);
            game.remove(pr,pc);

            //KEA choose a peice to remove from the board
            game.displayBoard();
            turn <- Definitions.KEAELE.KEA;
            printf "Possible peices to remove:"
            possible <- game.displayMoves(game.getFirstMoves(turn),turn);
            Functions.printTurn(turn);
            printf " - Choose a peice to remove:"
            select <- Functions.readTuple(size,possible);
            pr <- fst(select);
            pc <- snd(select);
            game.remove(pr,pc);

            //Switch turn to ELE (Black) and start cycling through until there is a winner
            turn <- Definitions.KEAELE.ELE;
            while not winner do //'game' loop
                game.displayBoard();
                printf "Possible peices to move:"
                possible <- game.displayMoves(game.getPeices(turn),turn); //display peices that can move
                Functions.printTurn(turn);
                printf " - Choose a peice to move:";
                select <- Functions.readTuple(size,possible); //read their peice selection
                pr <- fst(select);
                pc <- snd(select);
                printf "      Move to:"
                possible <- game.displayMoves(game.getMoves(pr,pc,turn),turn); //display possible moves their peice can make
                Functions.printTurn(turn);
                printf " - (%d,%d) to :" pr pc
                select <- Functions.readTuple(size,possible); //read their move selection
                mr <- fst(select);
                mc <- snd(select);

                game.remove(pr,pc); //remove the peices starting position (enum)

                game.Papamu.[mr,mc].set(turn);//set the peices destination (enum)

                //remove the killed peice from play, based on starting position and ending position
                match mr with
                        |mr when mr = pr ->
                            match mc with
                            |mc when mc > pc -> game.remove(pr,mc-1)
                            |_ -> game.remove(pr,mc+1)
                        |_ -> 
                            match mr with
                            |mr when mr > pr -> game.remove(mr-1,pc)
                            |_ -> game.remove(mr+1,pc)

                //test move again in same direction and prompt if continue
                if mr = pr then
                    let prc = mr;
                    possible <- Array.filter (fun elem -> fst(elem) = prc) <| game.getMoves(mr,mc,turn);
                else
                    let pcc = mc;
                    possible <- Array.filter (fun elem -> snd(elem) = pcc) <| game.getMoves(mr,mc,turn);

                //Almighty check for if a peice can continue on the same path
                let mutable cont = true; //user control over continuation
                while (Array.length(possible) > 0) && cont do //Hereby referred to as the 'hop' loop
                    cont <- false;
                    pr <- mr;
                    pc <- mc;
                    game.displayBoard();
                    Functions.printTurn(turn);
                    printf " - You can continue to:"
                    possible <- game.displayMoves(possible,turn); //display the possible moves in the same direction
                    cont <- Functions.getAnswer("Would you like to continue?")
                    if cont then
                        mr <- fst(possible.[0]);
                        mc <- snd(possible.[0]);

                        game.remove(pr,pc); //Remove starting position
                        game.Papamu.[mr,mc].set(turn); //Set destination

                        //Remove the fatality from play
                        match mr with
                        |mr when mr = pr ->
                            match mc with
                            |mc when mc > pc -> game.remove(pr,mc-1)
                            |_ -> game.remove(pr,mc+1)
                        |_ -> 
                            match mr with
                            |mr when mr > pr -> game.remove(mr-1,pc)
                            |_ -> game.remove(mr+1,pc)

                    //Re-check for moves in the same direction
                    if mr = pr then
                        let prc = mr;
                        possible <- Array.filter (fun elem -> fst(elem) = prc) <| game.getMoves(mr,mc,turn);
                    else
                        let pcc = mc;
                        possible <- Array.filter (fun elem -> snd(elem) = pcc) <| game.getMoves(mr,mc,turn);

                    //If there are no more moves, exit from the 'hop' loop
                    if not (Array.length(possible) > 0) then
                        cont <- false

                cont <- true;

                //switch turns
                if turn = Definitions.KEAELE.ELE then
                    turn <- Definitions.KEAELE.KEA
                else
                    turn <- Definitions.KEAELE.ELE
                
                //Check for winning status
                if (game.getPeices(turn)).Length <= 0 then
                    printf "KULANAKILA - "
                    Functions.printNotTurn(turn);
                    printf " !!\n\n--------------------------------------------------------------------------------"
                    winner <- true //exits the 'play' loop

            //prompt to play again
            contGame <- Functions.getAnswer("Would you like to play again?")