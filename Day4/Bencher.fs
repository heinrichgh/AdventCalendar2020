module AdventCalendar2020.Day9Bencher

open AdventCalendar2020
open BenchmarkDotNet.Attributes

[<MemoryDiagnoser>]
type Day4Part2Comparison () =
    
    [<Benchmark>]
    member self.ManyMap() = Day4.part2ManyMap
    
    [<Benchmark>]
    member self.SingleMap() = Day4.part2
    