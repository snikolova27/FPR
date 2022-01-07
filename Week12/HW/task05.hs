import Data.List
main :: IO()
main = do

    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] == "Maths"
   -- print $ listSubjects [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)]
  --  print $ subjectAvg ["Maths", "English", "Programming"] [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)]

type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note)


hardestSubject :: [Record] -> Subject
hardestSubject [] = error "no records"
hardestSubject rs = fst $ foldl1( \ (s1, n1) (s2,n2) -> if n1 < n2
                                                   then (s1,n1)
                                                   else (s2, n2)) final 
 where
     final = subjectAvg (listSubjects rs) rs


subjectAvg :: [Subject] -> [Record] -> [(Subject, Note)]
subjectAvg subs rs = map (\ s ->  (s, getAvgSubject s rs )) subs

listSubjects :: [Record] -> [Subject]
listSubjects = nub . map (\ (st,subject,note) -> subject)

getAvgSubject :: Subject -> [Record] -> Note
getAvgSubject _ [] = 0
getAvgSubject subject records = notesSum subject records /  fromIntegral (times subject records)

times :: Subject -> [Record] -> Int 
times _ [] = 0
times subject ((_,s,_) : rs)
 | subject == s = 1 + times subject rs
 | otherwise = times subject rs

notesSum :: Subject -> [Record] -> Note
notesSum _ [] = 0
notesSum subject ((_, s, n) : rs)
 | subject == s = n + notesSum subject rs
 | otherwise = notesSum subject rs