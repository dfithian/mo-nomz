//
//  Types.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import Foundation

struct ReadableFraction: Codable {
    let numerator: Int
    let denominator: Int
    
    func render() -> String {
        return "\(numerator)/\(denominator)"
    }
    
    func toInt() -> Int {
        switch (numerator, denominator) {
        case (1, 4): return 1
        case (1, 3): return 2
        case (1, 2): return 3
        case (2, 3): return 4
        case (3, 4): return 5
        default: return 0
        }
    }
    
    static func fromInt(x: Int) -> ReadableFraction? {
        switch x {
        case 1: return ReadableFraction(numerator: 1, denominator: 4)
        case 2: return ReadableFraction(numerator: 1, denominator: 3)
        case 3: return ReadableFraction(numerator: 1, denominator: 2)
        case 4: return ReadableFraction(numerator: 2, denominator: 3)
        case 5: return ReadableFraction(numerator: 3, denominator: 4)
        default: return nil
        }
    }
}

struct ReadableQuantity: Codable {
    let whole: Int?
    let fraction: ReadableFraction?
    
    func render() -> String? {
        switch (whole, fraction) {
        case (.none, .none): return nil
        case (.some(let w), .none): return String(w)
        case (.none, .some(let f)): return "\(f.render())"
        case (.some(let w), .some(let f)): return "\(w) \(f.render())"
        }
    }
    
    func toInt() -> Int {
        switch (whole, fraction) {
        case (.none, .none): return 0
        case (.some(let w), .none): return 6 * w
        case (.none, .some(let f)): return f.toInt()
        case (.some(let w), .some(let f)): return 6 * w + f.toInt()
        }
    }
    
    static func fromInt(x: Int) -> ReadableQuantity {
        let rawW = Int(x / 6)
        let w = rawW > 0 ? rawW : nil
        let f = ReadableFraction.fromInt(x: x % 6)
        return ReadableQuantity(whole: w, fraction: f)
    }
    
    static func +(left: ReadableQuantity, right: ReadableQuantity) -> ReadableQuantity {
        ReadableQuantity.fromInt(x: left.toInt() + right.toInt())
    }
}

struct ReadableIngredient: Codable {
    let name: String
    let quantity: ReadableQuantity
    let unit: String?
    let active: Bool
    
    func render() -> String {
        switch (quantity.render(), unit) {
        case (.some(let q), .some(let u)): return "\(q) \(u) \(name)"
        case (.some(let q), .none): return "\(q) \(name)"
        case (.none, .some(let u)): return "\(u) \(name)"
        case (.none, .none): return name
        }
    }
}

struct ReadableIngredientAggregate: Codable {
    let ids: [Int]
    let ingredient: ReadableIngredient
}

struct ListIngredientResponse: Codable {
    let ingredients: [ReadableIngredientAggregate]
}

struct MergeIngredientRequest: Codable {
    let ids: [Int]
    let name: String
    let quantity: ReadableQuantity
    let unit: String?
    let active: Bool
}

struct DeleteIngredientRequest: Codable {
    let ids: [Int]
}

struct CreateUserResponse: Codable {
    let userId: Int
    let apiToken: String
}

struct ReadableRecipe: Codable {
    let name: String
    let link: String?
    let active: Bool
    let ingredients: [ReadableIngredient]
}

struct ImportRecipeLinkRequest: Codable {
    let link: String
}

struct ImportRecipeBodyRequest: Codable {
    let name: String
    let content: String
}

struct UpdateRecipeRequest: Codable {
    let id: Int
    let active: Bool
}

struct ListRecipeResponse: Codable {
    let recipes: Dictionary<Int, ReadableRecipe>
}

struct DeleteRecipeRequest: Codable {
    let ids: [Int]
}
