module AdventCalendar2020.Day6

open System
open System.Text.RegularExpressions

let input = System.IO.File.ReadAllText("Day6/input.txt").Trim()

let newlineCharacter = "\n";

let questionnaires = input.Split(newlineCharacter + newlineCharacter)

let filterOutNonAlpha array =
    array
    |> Array.filter (fun el -> el >= 'a' && el <= 'z')

let part1 =
    questionnaires
    |> Array.map (
                  fun v -> v.ToCharArray()
                  >> filterOutNonAlpha
                  >> Array.distinct
                  >> Array.length
                  )
    |> Array.sum

let questions = [|'a'..'z'|]

let count (question:char) (questionnaire:string[]) =
    questionnaire
    |> Array.filter (fun v -> v.Contains question)
    |> Array.length

// gets a distinct list of questions answered for a given group's questionnaire
let questionsAnsweredYes (questionnaire:string[]) =
    questionnaire
    |> Array.map (fun v -> v.ToCharArray())
    |> Array.reduce Array.append
    |> Array.distinct

// calculates how many of each question was answered against how many people was in that group
let calculateQuestionsEveryoneAnsweredYes (questionnaire:string[]) =
     questionsAnsweredYes questionnaire
    |> Array.distinct 
    |> Array.map (fun q -> (q, count q questionnaire, Array.length questionnaire))
    |> Array.filter (fun (q, count, people) -> count = people)
    |> Array.length

let part2 =
    questionnaires
    |> Array.map (fun v -> v.Split(newlineCharacter))
    |> Array.map calculateQuestionsEveryoneAnsweredYes
    |> Array.sum
   