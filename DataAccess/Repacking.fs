﻿module StorageMachine.Repacking.Repacking

open StorageMachine
open Common
open BinTree
open SimulatedDatabase
open Repacking

let private binTree outerBin : Option<BinTree> =
    let bins = retrieveBins ()
    let binStructure = retrieveBinNesting ()
    let products = retrieveStock ()
    
    let rec go outerBin =
        // Locate all inner bins of the outer bin
        let innerBins =
            match binStructure |> Map.tryFind outerBin with
            | None -> []
            | Some (NestedBins (oneBin, more)) -> oneBin :: more
        // The outer bin may or may not contain a product
        let product = products |> Map.tryFind outerBin |> Option.map Product
        // Combine the outer bin, its optional product and sub-trees into a tree node
        Bin (outerBin, Option.toList product @ (List.map go innerBins))

    if Set.contains outerBin bins then Some (go outerBin) else None

let binTreeDataAccess = { new IBinTreeDataAccess with

    member this.RetrieveBinTree outerBin = binTree outerBin
    
}