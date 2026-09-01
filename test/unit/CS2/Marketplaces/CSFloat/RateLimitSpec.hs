module CS2.Marketplaces.CSFloat.RateLimitSpec (spec) where

import Test.Hspec

-- | CSFloat-specific limiter wiring in "CS2.Marketplaces.CSFloat.RateLimit":
-- 'csfloatLimiterSpec' validity, 'cacheHit', and the 'reconcileFromHeaders'
-- rate/capacity math. Build 'WithHeaders' values with chained 'addHeader'.
spec :: Spec
spec = do
    it "builds a valid limiter spec for every bucket" pending
    it "detects a Cloudflare cache HIT from cf-cache-status" pending
    it "reconciles capacity to min(remaining, limit * ratio)" pending
    it "floors the header-derived refill window at 60s" pending
