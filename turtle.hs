
--define orientation
data Orientation=North|South|East|West
    deriving(Show,Eq)

--define turtle having a strating position and a orientation
data Turtle= Turtle (Int,Int) Orientation 
    deriving(Show,Eq)

--define two actions: Step and Turn
data Action = Step |Turn 
    deriving (Show)

--define the Command type
data Command = Do Action|Repeat Action Int|Wait| Seq Command Command
    deriving(Show)

--this is function that executes a single command
oneCommand::Turtle->Command->Turtle
--step based on orientation
oneCommand (Turtle (x,y) North) (Do Step) = Turtle (x,y+1) North
oneCommand (Turtle (x,y) South) (Do Step) = Turtle (x,y-1) South
oneCommand (Turtle (x,y) East) (Do Step) = Turtle (x+1,y) East
oneCommand (Turtle (x,y) West) (Do Step) = Turtle (x-1,y) West
--this action turns turtle's orientation by 45 degrees clockwise
oneCommand (Turtle (x,y) orientation) (Do Turn) = 
    let newOrientation = case orientation of
            North -> East
            East  -> South
            South -> West
            West  -> North
    in Turtle (x, y) newOrientation
--these actions repeat the same action multiple times
oneCommand (Turtle (x,y) orientation) (Repeat Step reps) = foldl(\final_turtle _ -> oneCommand final_turtle (Do Step)) (Turtle (x,y) orientation) [1..reps] 
oneCommand (Turtle (x,y) orientation) (Repeat Turn reps) = foldl(\final_turtle _ -> oneCommand final_turtle (Do Turn)) (Turtle (x,y) orientation) [1..reps]  
--Wait action does not affect the turtle
oneCommand (Turtle (x,y) orientation) Wait=Turtle (x,y) orientation 
--Seq action executes two commands c1 and c2, in this order
oneCommand (Turtle (x,y) orientation) (Seq c1 c2)=
    let turtle_c1=oneCommand (Turtle (x,y) orientation) c1
    in oneCommand turtle_c1 c2

--this function gets a turtle and list of commands and returns the final position of the turtle
getPizza::Turtle->[Command]->(Int,Int)
getPizza (Turtle (x,y) orientation) commands=
    let Turtle (x_final,y_final) _=foldl oneCommand (Turtle (x,y) orientation) commands
    in (x_final,y_final)


--tests

turtle1 = Turtle (0, 0) North
test1 :: (Int, Int)
test1=getPizza turtle1 [Do Step, Do Turn, Do Step]
--afiseaza (1,1)

turtle2 = Turtle (9,0) East
test2=getPizza turtle2 [Do Step]
--afiseaza (10,0)

turtle3=Turtle (5,6) West
test3 = getPizza turtle3 [Seq(Repeat Turn 2)(Repeat Step 3)]
--afiseaza (8,6)

turtle4 = Turtle (3,0) North
test4 = getPizza turtle4 [Seq (Do Turn) (Seq (Do Step) (Do Turn)), Do Step]
--afiseza (4,-1)

turtle5 = Turtle (7,2) West
test5 = getPizza turtle5 [Wait]
--afiseaza (7,2)