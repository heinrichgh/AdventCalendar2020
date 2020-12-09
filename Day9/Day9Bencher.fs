module AdventCalendar2020.Bencher

open AdventCalendar2020
open BenchmarkDotNet.Attributes

[<MemoryDiagnoser>]
type Day9Part2Comparison () =
    
    [<Benchmark>]
    member self.Standard() = Day9.part2
    
//    [<Benchmark>]
//    member self.Parallel() = Day9.part2Parallel
    