module UITypes where 

-- -------------------------------------------------------------------
-- UI Types

-- | Ticks custom event 
data Tick = Tick

-- | Named resources
data Name = VP1 deriving (Ord, Show, Eq)

-- | Grid cell types
data Cell = Snake | Food | DoubleFood | Empty

