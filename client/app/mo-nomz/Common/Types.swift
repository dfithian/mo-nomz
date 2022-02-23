//
//  Types.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import CoreData
import Foundation
import UIKit

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

struct ReadableGroceryItem: Codable {
    let name: String
    let quantity: ReadableQuantity
    let unit: String?
    let active: Bool
    let order: Int
    
    func render() -> String {
        switch (quantity.render(), unit) {
        case (.some(let q), .some(let u)): return "\(q) \(u) \(name)"
        case (.some(let q), .none): return "\(q) \(name)"
        case (.none, .some(let u)): return "\(u) \(name)"
        case (.none, .none): return name
        }
    }
}

struct ReadableGroceryItemWithId: Codable {
    let item: ReadableGroceryItem
    let id: UUID
}

struct CreateUserResponse: Codable {
    let userId: Int
    let apiToken: String
}

struct ReadableIngredient: Codable {
    let name: String
    let quantity: ReadableQuantity
    let unit: String?
    let order: Int
    
    func render() -> String {
        switch (quantity.render(), unit) {
        case (.some(let q), .some(let u)): return "\(q) \(u) \(name)"
        case (.some(let q), .none): return "\(q) \(name)"
        case (.none, .some(let u)): return "\(u) \(name)"
        case (.none, .none): return name
        }
    }
}

struct ReadableIngredientWithId: Codable, Indexable {
    let id: UUID
    let ingredient: ReadableIngredient
    
    func toGroceryItemWithId(active: Bool, order: Int) -> ReadableGroceryItemWithId {
        return ReadableGroceryItemWithId(item: ReadableGroceryItem(name: ingredient.name, quantity: ingredient.quantity, unit: ingredient.unit, active: active, order: order), id: UUID())
    }
    
    func index() -> [Index] {
        return [
            Index(value: ingredient.name, priority: .high),
            ingredient.unit.map({ Index(value: $0, priority: .medium) })
        ].compactMap({ $0 })
    }
    
    func identifier() -> UUID {
        return id
    }
}

struct Step: Codable {
    let step: String
    let order: Int
}

struct StepWithId: Codable, Indexable {
    let id: UUID
    let step: Step
    
    func index() -> [Index] {
        return [
            Index(value: step.step, priority: .high)
        ]
    }
    
    func identifier() -> UUID {
        return id
    }
}

struct ReadableRecipe: Codable {
    let name: String
    let link: String?
    let active: Bool
    let rating: Int
    let notes: String
    let ingredients: [UUID:ReadableIngredient]
    let steps: [UUID:Step]
    
    func render() -> String {
        var lines = [String]()
        if let l = link?.nonEmpty() {
            lines.append(name + " - " + l)
        } else {
            lines.append(name)
        }
        if !ingredients.isEmpty {
            lines.append("")
            lines.append(contentsOf: ingredients.map({ $0.value.render() }))
        }
        if !steps.isEmpty {
            let s = steps.map({ $0.value }).sorted(by: { $0.order < $1.order }).map({ $0.step })
            lines.append("")
            lines.append(contentsOf: zip(1..., s).map({ String($0.0) + ". " + $0.1 }))
        }
        return lines.joined(separator: "\n")
    }
}

struct ReadableRecipeWithId: Codable, Indexable {
    let recipe: ReadableRecipe
    let id: UUID
    
    func index() -> [Index] {
        let index = [
            Index(value: recipe.name, priority: .high),
            Index(value: recipe.notes, priority: .medium)
        ]
        let ingredientsIndex = recipe.ingredients.map({ ReadableIngredientWithId(id: $0.0, ingredient: $0.1) }).flatMap({ $0.index().map({ $0.decPriority() }) })
        let stepsIndex = recipe.steps.map({ StepWithId(id: $0.0, step: $0.1) }).flatMap({ $0.index().map({ $0.decPriority() }) })
        return index + ingredientsIndex + stepsIndex
    }
    
    func identifier() -> UUID {
        return id
    }
}

struct ParseBlobRequest: Codable {
    let content: String
}

struct ParseLinkRequest: Codable {
    let link: String
}

struct ParseBlobResponse: Codable {
    let ingredients: [ReadableIngredient]
}

struct ParseLinkResponse: Codable {
    let name: String
    let ingredients: [ReadableIngredient]
    let steps: [String]
}

public extension CodingUserInfoKey {
    static let managedObjectContext = CodingUserInfoKey(rawValue: "managedObjectContext")
}

extension GroceryItemData {
    class func req() -> NSFetchRequest<GroceryItemData> {
        return NSFetchRequest<GroceryItemData>(entityName: "GroceryItemData")
    }

    func toReadableGroceryItem() -> ReadableGroceryItem {
        return ReadableGroceryItem(name: name ?? "", quantity: ReadableQuantity.fromInt(x: Int(quantity)), unit: unit, active: active, order: Int(ordering))
    }
    
    func toReadableGroceryItemWithId() -> ReadableGroceryItemWithId {
        return ReadableGroceryItemWithId(item: toReadableGroceryItem(), id: id!)
    }
}

extension IngredientData {
    class func req() -> NSFetchRequest<IngredientData> {
        return NSFetchRequest<IngredientData>(entityName: "IngredientData")
    }

    func toReadableIngredientWithId() -> ReadableIngredientWithId {
        return ReadableIngredientWithId(id: id!, ingredient: ReadableIngredient(name: name ?? "", quantity: ReadableQuantity.fromInt(x: Int(quantity)), unit: unit, order: Int(ordering)))
    }
}

extension StepData {
    class func req() -> NSFetchRequest<StepData> {
        return NSFetchRequest<StepData>(entityName: "StepData")
    }
    
    func toStepWithId() -> StepWithId {
        return StepWithId(id: id!, step: Step(step: step ?? "", order: Int(ordering)))
    }
}

extension RecipeData {
    class func req() -> NSFetchRequest<RecipeData> {
        return NSFetchRequest<RecipeData>(entityName: "RecipeData")
    }

    func toReadableRecipe(ingredientsData: [IngredientData], stepsData: [StepData]) -> ReadableRecipe {
        var ingredients = [UUID:ReadableIngredient]()
        for ingredient in ingredientsData {
            ingredients[ingredient.id!] = ingredient.toReadableIngredientWithId().ingredient
        }
        var steps = [UUID:Step]()
        for step in stepsData {
            steps[step.id!] = step.toStepWithId().step
        }
        return ReadableRecipe(name: name ?? "", link: link, active: active, rating: Int(rating), notes: notes ?? "", ingredients: ingredients, steps: steps)
    }
    
    func toReadableRecipeWithId(ingredientsData: [IngredientData], stepsData: [StepData]) -> ReadableRecipeWithId {
        return ReadableRecipeWithId(recipe: toReadableRecipe(ingredientsData: ingredientsData, stepsData: stepsData), id: id!)
    }
}

struct ExportGroceryItem: Codable {
    let name: String
    let quantity: ReadableQuantity
    let unit: String?
    let active: Bool
    let order: Int
}

struct ExportRecipe: Codable {
    let name: String
    let link: String?
    let active: Bool
    let rating: Int
    let notes: String
}

struct ExportIngredient: Codable {
    let groceryItemId: Int?
    let recipeId: Int?
    let name: String
    let quantity: ReadableQuantity
    let unit: String?
    let order: Int
}

struct ExportResponse: Codable {
    let groceries: [Int:ExportGroceryItem]
    let recipes: [Int:ExportRecipe]
    let ingredients: [Int:ExportIngredient]
}

struct UserPingRequest: Codable {
    let version: String
}

struct RecipeImport {
    let url: URL

    static func parse(_ url: URL) -> RecipeImport? {
        let prefix = "/recipe?recipe_url="
        guard let base = URLComponents(url: url, resolvingAgainstBaseURL: true) else { return nil }
        guard base.fragment?.starts(with: prefix) ?? false else { return nil }
        guard let link = base.fragment?.dropFirst(prefix.count) else { return nil }
        guard let url = URL(string: String(link)), UIApplication.shared.canOpenURL(url) else { return nil }
        return RecipeImport(url: url)
    }
}
