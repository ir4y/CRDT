module Main (main) where

import Crdt
import Lens
import Test.HUnit

test_gcounter = TestCase (let gc_1' = get_gc' "g_counter_1"
                              gc_2' = get_gc' "g_counter_2"
                              g_counter = GCounter [("g_counter_1",10),("g_counter_2", 20)]
                          in do { assertEqual "test getter" (g_counter ^. gc_1') 30
                                ; assertEqual "test getter" (g_counter ^. gc_2') 30
                                -- TODO fix Eq operator
                                ; assertEqual "test setter" ((gc_1' ^= 30) g_counter) $ GCounter [("g_counter_2",20),("g_counter_1", 30)] 
                                ; assertEqual "test setter" ((gc_2' ^= 30) g_counter) $ GCounter [("g_counter_2",30),("g_counter_1", 10)] 
                                })
test_pncounter = TestCase (let pnc' = get_pnc' "none"
                               pn_counter = PNCounter (GCounter [("g_counter_1",10),("g_counter_2", 20)]) (GCounter[("g_counter_3",5),("g_counter_4", 6)])
                           in assertEqual "test getter" (pn_counter ^. pnc') 19)


 


tests = TestList [ TestLabel "Test GCounter" test_gcounter 
                 , TestLabel "Test PNCounter" test_pncounter ]

main = runTestTT tests
