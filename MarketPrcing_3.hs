-- | Pricing a CDS using the Market Approach.
------------------------------------------------------------------------------------
type DiscountFactor = Double
type ProbDefault    = Double
type Recovery       = Double
type Protection     = Double
type Premium        = Double  
type SurvivalProb   = Double  
type Matuity        = Double
type PremiumTwo     = Double
------------------------------------------------------------------------------------
protectFee :: Recovery 
           -> [DiscountFactor] 
           -> [ProbDefault] 
           -> Protection
protectFee rcov discount prob = 
    (1-rcov) * sum [dis * pb | (dis,pb) <- zip discount prob]
------------------------------------------------------------------------------------
-- | we now calculate the premium fee if there is no default
-- | first we define a function that subtracts the first list element from the second
subtrct :: [Double] 
        -> [Double]
subtrct [] = []
subtrct [x] = []
subtrct (x1:x2:xs) = (x2 - x1) : subtrct (x2 : xs)
------------------------------------------------------------------------------------
premium :: [DiscountFactor] 
        -> [SurvivalProb] 
        -> [Matuity]
        -> Premium
premium  df pdf t1 = 
    sum [d*p*t|(d,p,t) <- zip3 df pdf (subtrct t1 )] 
------------------------------------------------------------------------------------
-- | we calculate the premium fee when default occurs
premiumDefault :: [DiscountFactor]  
               -> [ProbDefault] 
               -> [Matuity] 
               -> PremiumTwo
premiumDefault df pd  tm  = 
    sum [d*p*(t1/2) | (d,p,t1) <- zip3 df pd (subtrct tm) ] 
------------------------------------------------------------------------------------
-- | We now price the CDS as follows:
creditDefault :: Recovery
              -> [DiscountFactor]
              -> [ProbDefault]
              -> [SurvivalProb]
              -> [Matuity] 
              -> Protection
creditDefault a b c s d = 
    ( protectFee a b c ) / (premium b s d + premiumDefault b c d)
------------------------------------------------------------------------------------
main :: IO ()
main = cds 
  where
    a = 0.3 
    b = [0.9826,0.9643,0.9455,0.9254,0.9053,0.8849] 
    c = [0.0007,0.0017,0.0028,0.0043,0.0057,0.0068] 
    s = [0.9993,0.9983,0.9972,0.9957,0.9943,0.9932] 
    d = [0.5,1.0,1.5,2.0,2.5,3.0,3.5]
    cds = print $ creditDefault a b c s d