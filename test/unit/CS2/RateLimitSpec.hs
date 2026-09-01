module CS2.RateLimitSpec (spec) where

import Test.Hspec

-- | Token-bucket logic in "CS2.RateLimit". Drive the bucket deterministically
-- through 'CS2.RateLimit.tryAcquireSTM', which takes an explicit @now@, so no
-- sleeping is needed; the linear-refill invariant is a QuickCheck property.
spec :: Spec
spec = do
    it "acquires until capacity is drained, then reports Wait" pending
    it "rejects a toll larger than bucket capacity as Unsatisfiable" pending
    it "refills linearly and clamps at capacity" pending
    it "sums duplicate toll keys (unifyTolls)" pending
    it "reconcile / refundTolls adjust rate, capacity and tokens" pending
