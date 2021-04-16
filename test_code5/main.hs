data Sentence = Phrase Noun Verb Noun
              | And Sentence Sentence
--              deriving Show

data Noun = Dogs | Teeth
            deriving Show
data Verb = Have
            deriving Show


instance Show Sentence where
        show(Phrase s v o) = show s++" "++show v++" "++show o
        show(And s1 s2) = show s1++" and "++show s2

-- example sentences

s1::Sentence
s1 = Phrase Dogs Have Teeth

s2::Sentence
s2 = Phrase Teeth Have Dogs

s3::Sentence
s3 = And s1 s2

