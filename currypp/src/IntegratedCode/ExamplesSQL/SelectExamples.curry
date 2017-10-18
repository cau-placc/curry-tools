{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

--- Test module for integration of SQL

import Database.CDBI.Connection 
import Database.CDBI.ER 
import Time
import Uni_CDBI
import Test.Prop


queryS1 :: IO (SQLResult [Student])
queryS1 = ``sql Select * From Student;''

allStudents :: IO [Student]
allStudents = liftIO fromSQLResult ``sql Select * From Student;''

testS1 :: PropIO
testS1 = queryS1 `returns` Right
    [Student (StudentID 1) "Muster" "Max" 23 "max@email.de" Nothing
    ,Student (StudentID 2) "Arndt" "Annika" 66 "annika@email.de" (Just 25)
    ,Student (StudentID 3) "Ziege" "Stefan" 44 "stefan@email.de" (Just 20)
    ,Student (StudentID 4) "Sonne" "Ruben" 100 "ruben@email.de" (Just 28)
    ,Student (StudentID 5) "Mond" "Thorben" 55 "thorben@email.de" (Just 18)
    ,Student (StudentID 6) "Stern" "Susanne" 77 "susanne@email.de" (Just 18)]

testS2 :: IO() -- SQLResult [(Student,Uni_CDBI.Result)])
testS2 = do 
           result <- ``sql Select * From Student as s Inner Join Result as r On Satisfies s has_a r;'' 
           printResult result

queryS3 :: IO (SQLResult [String])
queryS3 = ``sql Select Distinct s.Name From Student as s;''

testS3 :: PropIO
testS3 = queryS3 `returns`
           Right ["Muster","Arndt","Ziege","Sonne","Mond","Stern"]

testS4 :: IO() -- SQLResult [String])
testS4 = do 
           result <- ``sql Select s.Name From Student as s Cross Join Result as r;''  
           printResult result

testS5 :: IO() -- SQLResult [(String,String)])
testS5 = do 
           result <- ``sql Select s.Firstname, s.Name From Student as s;''  
           printResult result

testS6 :: IO() -- SQLResult [(String, (Maybe Int))])
testS6 = do 
           result <- ``sql Select s.Name, r.Points From Student 
                           as s, Result as r Where r.Points > 45 and Satisfies s has_a r;'' 
           printResult result

testS7 :: IO() -- SQLResult [String])
testS7 = do 
           result <- ``sql Select Name From Student Union Select Name From Lecturer;''
           printResult result

queryS81 :: IO (SQLResult [(String, String)])
queryS81 = ``sql Select Distinct t.Name, l.Title 
                 From Lecture As l, Lecturer As t  
                 Where (Satisfies l taught_by t And t.Name like "Klein");''

testS81 :: PropIO
testS81 = queryS81 `returns` Right [("Klein","Theoretical Computer Science")]

queryS82 :: IO (SQLResult [(String, (Maybe Float))])
queryS82 = ``sql Select Distinct s.Name, r.Grade 
                 From Student as s, Result as r
                 Where Satisfies s has_a r And r.Grade < 2.0;''
                
testS82 :: PropIO
testS82 = queryS82 `returns` Right
            [("Muster",Just 1.3),("Arndt",Just 1.0),("Ziege",Just 1.3)]

testS83 :: IO() -- SQLResult [(String, (Maybe Int))])
testS83 = do 
           result <- ``sql Select s.Firstname, s.Age from Student as s Where (s.Age not null);'' 
           printResult result

testS84 :: IO() -- SQLResult [(String, (Maybe Int))])
testS84 = do 
           result <- ``sql Select s.Firstname, s.Age from Student as s Where s.Age in (20, 25, 30);'' 
           printResult result

testS85 :: IO() -- SQLResult [(String, (Maybe Int))])
testS85 = do 
           result <- ``sql Select s.Firstname, s.Age from Student as s Where s.Age is null;'' 
           printResult result

testS9 :: IO() -- SQLResult [(String, String)])
testS9 = do 
           result <- ``sql Select s.Firstname, s.Email
                           From Student As s Inner Join Result as r On Satisfies s has_a r 
                           Where (r.Attempt = 1 And r.Grade <= 3.0) Or (r.Attempt = 2  And r.Grade = 1.0);''
           printResult result

testS10 :: IO() -- SQLResult [String])
testS10 = do 
           result <- ``sql Select s.Name From Result As r, Student As s 
                           Where s.Key = r.StudentTakingKey Group By s.Name Having r.Grade < 2.0;''
           printResult result

testS11 :: IO() -- SQLResult [(String, (Maybe Int))])
testS11 = do 
           result <- ``sql Select Name, Age From Student Where Age between 20 and 30
                                              Order By Name Asc, Age Desc;''
           printResult result

testS12 :: IO() --SQLResult [String])
testS12 = do 
           result <- ``sql Select s.Name From Result As r, Student As s 
                           Where s.Key = r.StudentTakingKey And s.Age < 20 
                           Group By s.Name Having Sum(r.Points) > 100;''
           printResult result

testS13 :: IO() -- SQLResult [((Maybe Float), Int)])
testS13 = do 
           result <- ``sql Select r.Grade, Count(Distinct r.Grade)
                           From Result as r Inner Join Student as s On Satisfies r belongs_to s 
                           Group By s.Name having Count(Distinct r.Grade) > 1;''
           printResult result

testS14 :: IO() --SQLResult [(Float, Float)])
testS14 = do 
           result <- ``sql Select Avg(r.Grade), Avg(r.Points) 
                           From Result as r Inner Join Student as s On Satisfies r belongs_to s 
                           Group By s.Name having Avg(r.Grade) < 3.0 And Avg(r.Points) > 80.0;''
           printResult result

testS15 :: IO() -- SQLResult [(String, String)])
testS15 = do 
           result <- ``sql Select Distinct Firstname, Name From Student
                           Union 
                           Select Distinct Firstname, Name From Lecturer
                           Order By Firstname Asc
                           Limit 3;''
           printResult result

testS16 :: IO() -- SQLResult [Int])
testS16 = do 
           result <- ``sql Select Count(Distinct Name) From Student;''
           printResult result

testS17 :: IO() -- SQLResult [(String, String)])
testS17 = do 
           result <- ``sql Select s.Name,
                           Case When s.Age < 20 Then "Young" Else "Old" End 
                           From Student as s;''
           printResult result

testS18 :: IO() -- SQLResult [ClockTime])
testS18 = do 
           result <- ``sql Select t.Time from Time as t limit 5;'' 
           printResult result

testS19 :: IO() -- SQLResult [(Student, Uni_CDBI.Result, Exam)])
testS19 = do 
           result <- ``sql Select * from Student, Result, Exam where Satisfies Student has_a Result and Satisfies Exam results_in Result;'' 
           printResult result 

testS20 :: IO() -- SQLResult [(String, String)])
testS20 = do 
           result <- ``sql Select s1.Name, s2.Name From Student as s1, Student as s2 Where s1.Age != s2.Age;''
           printResult result

testS21 :: IO() -- SQLResult [(String, String)])
testS21 = do 
           result <- ``sql Select s.Firstname, l.Title from Student as s, Lecture as l Where Satisfies s Participation l;''
           printResult result

testS22 :: IO() -- SQLResult [(String, String)])
testS22 = do 
           result <- ``sql Select s.Firstname, l.Title from Student as s, Lecture as l Where Satisfies s participates l;''
           printResult result

testS23 :: IO() -- SQLResult [(String, String)])
testS23 = do 
           result <- ``sql Select s.Firstname, l.Title from Student as s, Lecture as l Where Satisfies l participated_by s;''
           printResult result

testS24 :: IO() -- SQLResult [(String, String)])
testS24 = do 
           result <- ``sql Select s.Firstname, l.Title from Student as s Inner Join Lecture as l On Satisfies s participates l;''
           printResult result

testS25 :: IO() --SQLResult [Student])
testS25 = do 
           result <- ``sql Select Distinct * From Student As s 
                           Where Exists
                           ( Select * From Participation As p 
                              Where s.key = p.StudentParticipationKey )
                           Limit 5;''
           printResult result
                     
testS26 :: IO() --SQLResult [Result])
testS26 = do 
           result <- ``sql Select Distinct * From Result As r 
                           Where Exists
                           ( Select * From Participation As p );'' 
           printResult result    
 
testS27 :: IO () --SQLResult [String])
testS27 = do 
           result <- ``sql Select s.Name from Student as s group by s.Age order by s.Name;''  
           printResult result

testS271 :: IO () --SQLResult [Student])
testS271 = do 
           result <- ``sql Select * from Student as s group by s.Age order by s.Name;''  
           printResult result

testS272 :: IO () --SQLResult [(String, Float, Int)])
testS272 = do 
           result <- ``sql Select s.Name, Avg(r.Points), Count(r.Points) from Student as s inner Join result as r on Satisfies s has_a r
                              group by s.Name having count(r.Points) > 1 order by s.Name;''
           printResult result
  
testS273 :: IO () --SQLResult [(Student, Result)])
testS273 = do 
            result <- ``sql Select * from Student as s inner Join result as r on Satisfies s has_a r
                              group by s.Name having count(r.Points) > 1 order by s.Name;''
            printResult result
                   
testS28 :: IO() --SQLResult [String])
testS28 = do 
           result <- ``sql Select s.Name from Student as s
                           intersect
                           Select l.Name from Lecturer as l;''
           printResult result

testS29 :: IO() --SQLResult [String])
testS29 = do 
           result <- ``sql Select l.Name from Student as l
                           intersect
                           Select l.Name from Lecturer as l;''
           printResult result
               
testS30 :: IO() --SQLResult [(String,(Maybe Int))])
testS30 = do 
           result <- ``sql Select s.Name, min(r.Points) from Student as s inner Join result as r on Satisfies s has_a r
                              group by s.Name having min(r.Points) < 40;''
           printResult result

testS31 :: IO() --SQLResult [(String, (Maybe Int))])
testS31 = do 
           result <- ``sql Select s.Name, max(r.Points) from Student as s inner Join result as r on Satisfies s has_a r
                              group by s.Name having max(r.Points) > 90;''
           printResult result

testS32 :: IO() -- SQLResult [(String, (Maybe Int))])
testS32 = do 
           result <- ``sql Select Student.Name, Result.Points From Student , Result;''
           printResult result

                
testS35 :: IO (SQLResult [Int])
testS35 = ``sql Select Count( Distinct r.StudentTakingKey)
                from Result as r, Student as s
                Group By r.Grade Having
                                 (s.Age > 20 and Satisfies r belongs_to s);''

testS36 :: IO (SQLResult [Int])
testS36 = ``sql Select Count( Distinct r.StudentTakingKey)
                from Result as r, Student as s
                Group By r.Grade Having (Satisfies r belongs_to s);''

printResult :: Show a => SQLResult [a] -> IO()
printResult result = 
  case result of
      Left err  -> putStrLn $ show err
      Right res -> printList res 

   
printList :: Show a => [a] -> IO ()
printList (x:xs) = do putStrLn $ show x
                      printList xs
printList [] = putStrLn ""
