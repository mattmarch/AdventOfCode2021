module Common

open System.IO

let readLines path: string seq = File.ReadLines path