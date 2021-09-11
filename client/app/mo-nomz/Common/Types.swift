//
//  Types.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import CoreData
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
    
    func insert(_ ctx: NSManagedObjectContext) {
        let newGrocery = NSEntityDescription.insertNewObject(forEntityName: "GroceryItemData", into: ctx) as! GroceryItemData
        newGrocery.id = id
        newGrocery.name = item.name
        newGrocery.ordering = Int32(item.order)
        newGrocery.quantity = Int32(item.quantity.toInt())
        newGrocery.unit = item.unit
        newGrocery.active = item.active
    }
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

struct ReadableIngredientWithId: Codable {
    let id: UUID
    let ingredient: ReadableIngredient
    
    func toGroceryItemWithId(active: Bool) -> ReadableGroceryItemWithId {
        return ReadableGroceryItemWithId(item: ReadableGroceryItem(name: ingredient.name, quantity: ingredient.quantity, unit: ingredient.unit, active: active, order: ingredient.order), id: UUID())
    }
    
    func insert(_ ctx: NSManagedObjectContext, recipeId: UUID?, groceryId: UUID?) {
        let newIngredient = NSEntityDescription.insertNewObject(forEntityName: "IngredientData", into: ctx) as! IngredientData
        newIngredient.id = id
        newIngredient.recipe_id = recipeId
        newIngredient.grocery_id = groceryId
        newIngredient.name = ingredient.name
        newIngredient.ordering = Int32(ingredient.order)
        newIngredient.quantity = Int32(ingredient.quantity.toInt())
        newIngredient.unit = ingredient.unit
    }
}

struct ReadableRecipe: Codable {
    let name: String
    let link: String?
    let active: Bool
    let rating: Int
    let notes: String
    let ingredients: [UUID:ReadableIngredient]
}

struct ReadableRecipeWithId: Codable {
    let recipe: ReadableRecipe
    let id: UUID
    
    func insert(_ ctx: NSManagedObjectContext) {
        let newRecipe = NSEntityDescription.insertNewObject(forEntityName: "RecipeData", into: ctx) as! RecipeData
        newRecipe.id = id
        newRecipe.name = recipe.name
        newRecipe.link = recipe.link
        newRecipe.active = recipe.active
        newRecipe.rating = Int32(recipe.rating)
        newRecipe.notes = recipe.notes
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
    
    func insert(_ ctx: NSManagedObjectContext, name: String, link: String?) {
        let recipeId = UUID()
        ReadableRecipeWithId(recipe: ReadableRecipe(name: name, link: link, active: true, rating: 0, notes: "", ingredients: [:]), id: recipeId).insert(ctx)
        let groceryIds = (0...(ingredients.count)).map({ _ in UUID() })
        zip(groceryIds, ingredients).forEach({
            ReadableGroceryItemWithId(item: ReadableGroceryItem(name: $0.1.name, quantity: $0.1.quantity, unit: $0.1.unit, active: true, order: $0.1.order), id: $0.0).insert(ctx)
        })
        zip(groceryIds, ingredients).forEach({
            ReadableIngredientWithId(id: UUID(), ingredient: ReadableIngredient(name: $0.1.name, quantity: $0.1.quantity, unit: $0.1.unit, order: $0.1.order)).insert(ctx, recipeId: recipeId, groceryId: $0.0)
        })
    }
    
    func insert(_ ctx: NSManagedObjectContext) {
        let groceryIds = (0...(ingredients.count)).map({ _ in UUID() })
        zip(groceryIds, ingredients).forEach({
            ReadableGroceryItemWithId(item: ReadableGroceryItem(name: $0.1.name, quantity: $0.1.quantity, unit: $0.1.unit, active: true, order: $0.1.order), id: $0.0).insert(ctx)
        })
        zip(groceryIds, ingredients).forEach({
            ReadableIngredientWithId(id: UUID(), ingredient: ReadableIngredient(name: $0.1.name, quantity: $0.1.quantity, unit: $0.1.unit, order: $0.1.order)).insert(ctx, recipeId: nil, groceryId: $0.0)
        })
    }
}

struct ParseLinkResponse: Codable {
    let name: String
    let ingredients: [ReadableIngredient]
    
    func insert(_ ctx: NSManagedObjectContext, link: String?, active: Bool) {
        let recipeId = UUID()
        ReadableRecipeWithId(recipe: ReadableRecipe(name: name, link: link, active: active, rating: 0, notes: "", ingredients: [:]), id: recipeId).insert(ctx)
        let groceryIds = (0...(ingredients.count)).map({ _ in UUID() })
        zip(groceryIds, ingredients).forEach({
            ReadableGroceryItemWithId(item: ReadableGroceryItem(name: $0.1.name, quantity: $0.1.quantity, unit: $0.1.unit, active: true, order: $0.1.order), id: $0.0).insert(ctx)
        })
        zip(groceryIds, ingredients).forEach({
            ReadableIngredientWithId(id: UUID(), ingredient: ReadableIngredient(name: $0.1.name, quantity: $0.1.quantity, unit: $0.1.unit, order: $0.1.order)).insert(ctx, recipeId: recipeId, groceryId: $0.0)
        })
    }
}

public extension CodingUserInfoKey {
    static let managedObjectContext = CodingUserInfoKey(rawValue: "managedObjectContext")
}

extension GroceryItemData {
    func toReadableGroceryItem() -> ReadableGroceryItem {
        return ReadableGroceryItem(name: name ?? "", quantity: ReadableQuantity.fromInt(x: Int(quantity)), unit: unit, active: active, order: Int(ordering))
    }
    
    func toReadableGroceryItemWithId() -> ReadableGroceryItemWithId {
        return ReadableGroceryItemWithId(item: toReadableGroceryItem(), id: id!)
    }
}

extension IngredientData {
    func toReadableIngredientWithId() -> ReadableIngredientWithId {
        return ReadableIngredientWithId(id: id!, ingredient: ReadableIngredient(name: name ?? "", quantity: ReadableQuantity.fromInt(x: Int(quantity)), unit: unit, order: Int(ordering)))
    }
}

extension RecipeData {
    func toReadableRecipe(ingredientsData: [IngredientData]) -> ReadableRecipe {
        let ingredients = ingredientsData.reduce([UUID:ReadableIngredient](), { (dict, ingredientData) -> [UUID:ReadableIngredient] in
            var d = dict
            let ingredient = ingredientData.toReadableIngredientWithId()
            d[ingredient.id] = ingredient.ingredient
            return d
        })
        return ReadableRecipe(name: name ?? "", link: link, active: active, rating: Int(rating), notes: notes ?? "", ingredients: ingredients)
    }
    
    func toReadableRecipeWithId(ingredientsData: [IngredientData]) -> ReadableRecipeWithId {
        return ReadableRecipeWithId(recipe: toReadableRecipe(ingredientsData: ingredientsData), id: id!)
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
    
    func insert(_ ctx: NSManagedObjectContext) {
        let recipeIds: [Int:UUID] = recipes.reduce([Int:UUID](), { (dict, recipe) -> [Int:UUID] in
            var d = dict
            let recipeId = UUID()
            let newRecipe = NSEntityDescription.insertNewObject(forEntityName: "RecipeData", into: ctx) as! RecipeData
            newRecipe.id = recipeId
            newRecipe.name = recipe.value.name
            newRecipe.link = recipe.value.link
            newRecipe.active = recipe.value.active
            newRecipe.rating = Int32(recipe.value.rating)
            newRecipe.notes = recipe.value.notes
            d[recipe.key] = recipeId
            return d
        })
        let groceryIds: [Int:UUID] = groceries.reduce([Int:UUID](), { (dict, grocery) -> [Int:UUID] in
            var d = dict
            let groceryId = UUID()
            let newGrocery = NSEntityDescription.insertNewObject(forEntityName: "GroceryItemData", into: ctx) as! GroceryItemData
            newGrocery.id = groceryId
            newGrocery.name = grocery.value.name
            newGrocery.quantity = Int32(grocery.value.quantity.toInt())
            newGrocery.unit = grocery.value.unit
            newGrocery.active = grocery.value.active
            newGrocery.ordering = Int32(grocery.value.order)
            d[grocery.key] = groceryId
            return d
        })
        ingredients.forEach({ (ingredient) -> () in
            let newIngredient = NSEntityDescription.insertNewObject(forEntityName: "IngredientData", into: ctx) as! IngredientData
            newIngredient.id = UUID()
            newIngredient.grocery_id = ingredient.value.groceryItemId.map({ groceryIds[$0]! })
            newIngredient.recipe_id = ingredient.value.recipeId.map({ recipeIds[$0]! })
            newIngredient.name = ingredient.value.name
            newIngredient.quantity = Int32(ingredient.value.quantity.toInt())
            newIngredient.unit = ingredient.value.unit
            newIngredient.ordering = Int32(ingredient.value.order)
        })
    }
}
