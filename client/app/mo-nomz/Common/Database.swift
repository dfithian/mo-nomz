//
//  Database.swift
//  mo-nomz
//
//  Created by Dan Fithian on 9/11/21.
//

import CoreData
import Foundation
import UIKit

extension UIViewController {
    private func insertGroceryRaw(_ ctx: NSManagedObjectContext, grocery: ReadableGroceryItemWithId) {
        let newGrocery = NSEntityDescription.insertNewObject(forEntityName: "GroceryItemData", into: ctx) as! GroceryItemData
        newGrocery.id = grocery.id
        newGrocery.name = grocery.item.name
        newGrocery.quantity = Int32(grocery.item.quantity.toInt())
        newGrocery.unit = grocery.item.unit
        newGrocery.active = grocery.item.active
        newGrocery.ordering = Int32(grocery.item.order)
    }
    
    private func insertIngredientRaw(_ ctx: NSManagedObjectContext, ingredient: ReadableIngredientWithId, recipeId: UUID?, groceryId: UUID?) {
        let newIngredient = NSEntityDescription.insertNewObject(forEntityName: "IngredientData", into: ctx) as! IngredientData
        newIngredient.id = ingredient.id
        newIngredient.grocery_id = groceryId
        newIngredient.recipe_id = recipeId
        newIngredient.name = ingredient.ingredient.name
        newIngredient.quantity = Int32(ingredient.ingredient.quantity.toInt())
        newIngredient.unit = ingredient.ingredient.unit
        newIngredient.ordering = Int32(ingredient.ingredient.order)
    }
    
    private func insertRecipeRaw(_ ctx: NSManagedObjectContext, recipe: ReadableRecipeWithId) {
        let newRecipe = NSEntityDescription.insertNewObject(forEntityName: "RecipeData", into: ctx) as! RecipeData
        newRecipe.id = recipe.id
        newRecipe.name = recipe.recipe.name
        newRecipe.link = recipe.recipe.link
        newRecipe.active = recipe.recipe.active
        newRecipe.rating = Int32(recipe.recipe.rating)
        newRecipe.notes = recipe.recipe.notes
        
        for (id, step) in recipe.recipe.steps {
            insertStepRaw(ctx, step: StepWithId(id: id, step: step), recipeId: recipe.id)
        }
    }
    
    private func insertStepRaw(_ ctx: NSManagedObjectContext, step: StepWithId, recipeId: UUID) {
        let newStep = NSEntityDescription.insertNewObject(forEntityName: "StepData", into: ctx) as! StepData
        newStep.id = step.id
        newStep.recipe_id = recipeId
        newStep.step = step.step.step
        newStep.ordering = Int32(step.step.order)
    }
    
    private func selectRecipeIngredientsRaw(_ ctx: NSManagedObjectContext, recipeId: UUID) throws -> [IngredientData] {
        let ingredientReq = IngredientData.req()
        ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", recipeId as CVarArg)
        ingredientReq.sortDescriptors = [
            NSSortDescriptor(key: "ordering", ascending: true),
            NSSortDescriptor(key: "name", ascending: true)
        ]
        return try ctx.fetch(ingredientReq)
    }
    
    private func selectRecipeStepsRaw(_ ctx: NSManagedObjectContext, recipeId: UUID) throws -> [StepData] {
        let stepReq = StepData.req()
        stepReq.predicate = NSPredicate(format: "recipe_id = %@", recipeId as CVarArg)
        stepReq.sortDescriptors = [
            NSSortDescriptor(key: "ordering", ascending: true),
            NSSortDescriptor(key: "step", ascending: true)
        ]
        return try ctx.fetch(stepReq)
    }

    func selectGroceries() -> [ReadableGroceryItemWithId] {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = GroceryItemData.req()
            req.sortDescriptors = [
                NSSortDescriptor(key: "ordering", ascending: true),
                NSSortDescriptor(key: "name", ascending: true)
            ]
            return try ctx.fetch(req).map({ $0.toReadableGroceryItemWithId() })
        } catch let error as NSError {
            print(error)
        }
        return []
    }
    
    func selectMaxOrderRaw(_ ctx: NSManagedObjectContext) throws -> Int {
        let req = GroceryItemData.req()
        req.sortDescriptors = [NSSortDescriptor(key: "ordering", ascending: false)]
        req.fetchLimit = 1
        if let row = try ctx.fetch(req).first {
            return row.value(forKey: "ordering") as! Int + 1
        } else {
            return 1
        }
    }
    
    func selectMaxStepOrderRaw(_ ctx: NSManagedObjectContext, recipeId: UUID) throws -> Int {
        let req = StepData.req()
        req.predicate = NSPredicate(format: "recipe_id = %@", recipeId as CVarArg)
        req.sortDescriptors = [NSSortDescriptor(key: "ordering", ascending: false)]
        req.fetchLimit = 1
        if let row = try ctx.fetch(req).first {
            return row.value(forKey: "ordering") as! Int + 1
        } else {
            return 1
        }
    }
    
    func insertGroceries(ingredients: [ReadableIngredient]) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            var maxOrder = try selectMaxOrderRaw(ctx)
            for ingredient in ingredients {
                let ingredientWithId = ReadableIngredientWithId(id: UUID(), ingredient: ingredient)
                let groceryWithId = ingredientWithId.toGroceryItemWithId(active: true, order: maxOrder)
                maxOrder += 1
                insertGroceryRaw(ctx, grocery: groceryWithId)
                insertIngredientRaw(ctx, ingredient: ingredientWithId, recipeId: nil, groceryId: groceryWithId.id)
            }
            try automergeGroceriesRaw(ctx)
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    func updateGrocery(grocery: ReadableGroceryItemWithId) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = GroceryItemData.req()
            req.predicate = NSPredicate(format: "id = %@", grocery.id as CVarArg)
            req.fetchLimit = 1
            if let fetched = try ctx.fetch(req).first {
                if fetched.ordering != Int32(grocery.item.order) {
                    let reorderReq = GroceryItemData.req()
                    let notThisId = NSPredicate(format: "id <> %@", grocery.id as CVarArg)
                    let largerOrder = NSPredicate(format: "ordering >= %@", grocery.item.order as NSNumber)
                    reorderReq.predicate = NSCompoundPredicate(type: .and, subpredicates: [notThisId, largerOrder])
                    for other in try ctx.fetch(reorderReq) {
                        other.ordering += 1
                    }
                }
                fetched.name = grocery.item.name
                fetched.quantity = Int32(grocery.item.quantity.toInt())
                fetched.unit = grocery.item.unit
                fetched.active = grocery.item.active
                fetched.ordering = Int32(grocery.item.order)
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    func mergeGroceries(ids: [UUID], grocery: ReadableGroceryItemWithId) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            
            // insert new grocery
            insertGroceryRaw(ctx, grocery: grocery)
            
            // set new grocery id for ingredient
            let ingredientReq = IngredientData.req()
            ingredientReq.predicate = NSPredicate(format: "grocery_id in %@", ids as CVarArg)
            for ingredient in try ctx.fetch(ingredientReq) {
                ingredient.grocery_id = grocery.id
            }
            
            // delete old grocery
            let groceryReq = GroceryItemData.req()
            groceryReq.predicate = NSPredicate(format: "id in %@", ids as CVarArg)
            for grocery in try ctx.fetch(groceryReq) {
                ctx.delete(grocery)
            }
            
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    func deleteGrocery(id: UUID) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            
            // set grocery id as null for recipe ingredient, otherwise delete ingredient
            let ingredientReq = IngredientData.req()
            ingredientReq.predicate = NSPredicate(format: "grocery_id = %@", id as CVarArg)
            for ingredient in try ctx.fetch(ingredientReq) {
                if ingredient.recipe_id == nil {
                    ctx.delete(ingredient)
                } else {
                    ingredient.grocery_id = nil
                }
            }
            
            // delete grocery
            let groceryReq = GroceryItemData.req()
            groceryReq.predicate = NSPredicate(format: "id = %@", id as CVarArg)
            for grocery in try ctx.fetch(groceryReq) {
                ctx.delete(grocery)
            }
            
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    func selectRecipes() -> [ReadableRecipeWithId] {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = RecipeData.req()
            req.sortDescriptors = [
                NSSortDescriptor(key: "rating", ascending: false),
                NSSortDescriptor(key: "name", ascending: true)
            ]
            return try ctx.fetch(req).map({ (recipe) in
                let ingredients = try selectRecipeIngredientsRaw(ctx, recipeId: recipe.id!)
                let steps = try selectRecipeStepsRaw(ctx, recipeId: recipe.id!)
                return recipe.toReadableRecipeWithId(ingredientsData: ingredients, stepsData: steps)
            })
        } catch let error as NSError {
            print(error)
        }
        return []
    }
    
    func getRecipe(id: UUID) -> ReadableRecipeWithId? {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let ingredients = try selectRecipeIngredientsRaw(ctx, recipeId: id)
            let steps = try selectRecipeStepsRaw(ctx, recipeId: id)
            
            let recipeReq = RecipeData.req()
            recipeReq.predicate = NSPredicate(format: "id = %@", id as CVarArg)
            recipeReq.fetchLimit = 1
            return try ctx.fetch(recipeReq).first?.toReadableRecipeWithId(ingredientsData: ingredients, stepsData: steps)
        } catch let error as NSError {
            print(error)
        }
        return nil
    }
    
    func insertRecipe(response: ParseLinkResponse, link: String, active: Bool) {
        var ingredients = [UUID:ReadableIngredient]()
        for ingredient in response.ingredients {
            ingredients[UUID()] = ingredient
        }
        var order = 0
        var steps = [UUID:Step]()
        for step in response.steps {
            steps[UUID()] = Step(step: step, order: order)
            order += 1
        }
        let recipe = ReadableRecipe(name: response.name, link: link, active: active, rating: 0, notes: "", ingredients: ingredients, steps: steps)
        insertRecipe(recipe: recipe)
    }
    
    func insertRecipe(response: ParseBlobResponse, name: String, link: String?, rawSteps: [String], active: Bool) {
        var ingredients = [UUID:ReadableIngredient]()
        for ingredient in response.ingredients {
            ingredients[UUID()] = ingredient
        }
        var order = 0
        var steps = [UUID:Step]()
        for step in rawSteps {
            steps[UUID()] = Step(step: step, order: order)
            order += 1
        }
        let recipe = ReadableRecipe(name: name, link: link, active: active, rating: 0, notes: "", ingredients: ingredients, steps: steps)
        insertRecipe(recipe: recipe)
    }
    
    func insertRecipe(recipe: ReadableRecipe) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            
            // insert recipe
            let recipeWithId = ReadableRecipeWithId(recipe: recipe, id: UUID())
            insertRecipeRaw(ctx, recipe: recipeWithId)
            
            // insert ingredients and groceries
            var maxOrder = try selectMaxOrderRaw(ctx)
            for (ingredientId, ingredient) in recipe.ingredients {
                let ingredientWithId = ReadableIngredientWithId(id: ingredientId, ingredient: ingredient)
                let groceryWithId = ingredientWithId.toGroceryItemWithId(active: recipe.active, order: maxOrder)
                maxOrder += 1
                insertGroceryRaw(ctx, grocery: groceryWithId)
                insertIngredientRaw(ctx, ingredient: ingredientWithId, recipeId: recipeWithId.id, groceryId: groceryWithId.id)
            }

            // automerge
            try automergeGroceriesRaw(ctx)

            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    private func activateRecipeRaw(_ ctx: NSManagedObjectContext, id: UUID) throws {
        // setting recipe active bit handled by caller
        let ingredientReq = IngredientData.req()
        ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
        ingredientReq.sortDescriptors = [
            NSSortDescriptor(key: "ordering", ascending: true),
            NSSortDescriptor(key: "name", ascending: true)
        ]
        var maxOrder = try selectMaxOrderRaw(ctx)
        for ingredient in try ctx.fetch(ingredientReq) {
            let grocery = ingredient.toReadableIngredientWithId().toGroceryItemWithId(active: true, order: maxOrder)
            maxOrder += 1
            insertGroceryRaw(ctx, grocery: grocery)
            ingredient.grocery_id = grocery.id
        }
    }
    
    private func deactivateRecipeRaw(_ ctx: NSManagedObjectContext, id: UUID) throws {
        // setting recipe active bit handled by caller
        let ingredientReq = IngredientData.req()
        ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
        var ingredientIds = Set<UUID>()
        for ingredient in try ctx.fetch(ingredientReq) {
            ingredientIds.insert(ingredient.id!)
        }
        try unmergeGroceriesRaw(ctx, ingredientIds: Array(ingredientIds))
    }
    
    func updateRecipe(id: UUID, recipe: ReadableRecipe) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = RecipeData.req()
            req.predicate = NSPredicate(format: "id = %@", id as CVarArg)
            req.fetchLimit = 1
            if let fetched = try ctx.fetch(req).first {
                if fetched.active != recipe.active {
                    let ingredientReq = IngredientData.req()
                    ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
                    if recipe.active {
                        try activateRecipeRaw(ctx, id: id)
                    } else {
                        try deactivateRecipeRaw(ctx, id: id)
                    }
                }
                fetched.name = recipe.name
                fetched.active = recipe.active
                fetched.notes = recipe.notes
                fetched.rating = Int32(recipe.rating)
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    func updateRecipeIngredients(id: UUID, active: Bool, deletes: [UUID], adds: [ReadableIngredientWithId]) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            
            // unmerge groceries
            try unmergeGroceriesRaw(ctx, ingredientIds: deletes)
            
            // delete the deletes
            let req = IngredientData.req()
            req.predicate = NSPredicate(format: "id in %@", deletes as CVarArg)
            for ingredient in try ctx.fetch(req) {
                ctx.delete(ingredient)
            }
            
            // insert the adds
            var maxOrder = try selectMaxOrderRaw(ctx)
            for ingredient in adds {
                if active {
                    let grocery = ingredient.toGroceryItemWithId(active: active, order: maxOrder)
                    maxOrder += 1
                    insertGroceryRaw(ctx, grocery: grocery)
                    insertIngredientRaw(ctx, ingredient: ingredient, recipeId: id, groceryId: grocery.id)
                } else {
                    insertIngredientRaw(ctx, ingredient: ingredient, recipeId: id, groceryId: nil)
                }
            }
            
            // automerge
            try automergeGroceriesRaw(ctx)
            
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    func addRecipeSteps(recipeId: UUID, rawSteps: [String]) -> [StepWithId] {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            var ordering = try selectMaxStepOrderRaw(ctx, recipeId: recipeId)
            var steps = [StepWithId]()
            for rawStep in rawSteps {
                let step = StepWithId(id: UUID(), step: Step(step: rawStep, order: ordering))
                insertStepRaw(ctx, step: step, recipeId: recipeId)
                steps.append(step)
                ordering += 1
            }
            try ctx.save()
            
            return steps
        } catch let error as NSError {
            print(error)
        }
        return []
    }
    
    func deleteRecipeStep(id: UUID) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = StepData.req()
            req.predicate = NSPredicate(format: "id = %@", id as CVarArg)
            for step in try ctx.fetch(req) {
                ctx.delete(step)
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    func updateRecipeStep(recipeId: UUID, step: StepWithId) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = StepData.req()
            req.predicate = NSPredicate(format: "id = %@", step.id as CVarArg)
            req.fetchLimit = 1
            
            if let fetched = try ctx.fetch(req).first {
                if fetched.ordering != Int32(step.step.order) {
                    let reorderReq = StepData.req()
                    let notThisId = NSPredicate(format: "id <> %@", step.id as CVarArg)
                    let thisRecipe = NSPredicate(format: "recipe_id = %@", recipeId as CVarArg)
                    let largerOrder = NSPredicate(format: "ordering >= %@", step.step.order as NSNumber)
                    reorderReq.predicate = NSCompoundPredicate(type: .and, subpredicates: [notThisId, thisRecipe, largerOrder])
                    for other in try ctx.fetch(reorderReq) {
                        other.ordering += 1
                    }
                }
                fetched.step = step.step.step
                fetched.ordering = Int32(step.step.order)
            }
            
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    func deleteRecipe(id: UUID) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            
            // unmerge groceries with ingredients for this recipe and delete the ingredients
            let ingredientReq = IngredientData.req()
            ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
            let ingredients = try ctx.fetch(ingredientReq)
            let ingredientIds = ingredients.compactMap({ $0.id })
            try unmergeGroceriesRaw(ctx, ingredientIds: ingredientIds)
            for ingredient in ingredients {
                ctx.delete(ingredient)
            }
            
            // delete the recipe
            let recipeReq = RecipeData.req()
            recipeReq.predicate = NSPredicate(format: "id = %@", id as CVarArg)
            for recipe in try ctx.fetch(recipeReq) {
                ctx.delete(recipe)
            }
            
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    private func automergeGroceriesRaw(_ ctx: NSManagedObjectContext, active: Bool, groceries: [ReadableGroceryItemWithId]) throws {
        struct NameAndUnit: Hashable {
            let name: String
            let unit: String?
        }
        
        // aggregate groceries by name and unit
        var byNameAndUnit = [NameAndUnit:([UUID], ReadableQuantity, Int)]()
        for grocery in groceries {
            let nameAndUnit = NameAndUnit(name: grocery.item.name, unit: grocery.item.unit)
            let (ids, quantity, order) = byNameAndUnit[nameAndUnit] ?? ([], ReadableQuantity(whole: 0, fraction: nil), Int.max)
            byNameAndUnit[nameAndUnit] = (ids + [grocery.id], quantity + grocery.item.quantity, min(grocery.item.order, order))
        }
        
        for (nameAndUnit, (ids, quantity, order)) in byNameAndUnit {
            if ids.count >= 2 {
                // insert new grocery for merge
                let newGrocery = ReadableGroceryItemWithId(item: ReadableGroceryItem(name: nameAndUnit.name, quantity: quantity, unit: nameAndUnit.unit, active: active, order: order), id: UUID())
                insertGroceryRaw(ctx, grocery: newGrocery)
                
                // update the ingredient grocery id
                let ingredientReq = IngredientData.req()
                ingredientReq.predicate = NSPredicate(format: "grocery_id in %@", ids as CVarArg)
                for ingredient in try ctx.fetch(ingredientReq) {
                    ingredient.grocery_id = newGrocery.id
                }
                
                // delete the old groceries
                let groceryReq2 = GroceryItemData.req()
                groceryReq2.predicate = NSPredicate(format: "id in %@", ids as CVarArg)
                for grocery in try ctx.fetch(groceryReq2) {
                    ctx.delete(grocery)
                }
            }
        }
    }
    
    private func automergeGroceriesRaw(_ ctx: NSManagedObjectContext) throws {
        // select the existing groceries
        var activeGroceries: [ReadableGroceryItemWithId] = []
        var inactiveGroceries: [ReadableGroceryItemWithId] = []
        for grocery in try ctx.fetch(GroceryItemData.req()) {
            if grocery.active {
                activeGroceries.append(grocery.toReadableGroceryItemWithId())
            } else {
                inactiveGroceries.append(grocery.toReadableGroceryItemWithId())
            }
        }
        
        try automergeGroceriesRaw(ctx, active: true, groceries: activeGroceries)
        try automergeGroceriesRaw(ctx, active: false, groceries: inactiveGroceries)
    }
    
    private func unmergeGroceriesRaw(_ ctx: NSManagedObjectContext, ingredientIds: [UUID]) throws {
        // unset the grocery id for the existing ingredients
        let ingredientReq = IngredientData.req()
        ingredientReq.predicate = NSPredicate(format: "id in %@", ingredientIds as CVarArg)
        var oldGroceryIds = Set<UUID>()
        for ingredient in try ctx.fetch(ingredientReq) {
            if let groceryId = ingredient.grocery_id {
                oldGroceryIds.insert(groceryId)
            }
            ingredient.grocery_id = nil
        }
        
        // select old groceries to delete
        let groceryReq = GroceryItemData.req()
        groceryReq.predicate = NSPredicate(format: "id in %@", Array(oldGroceryIds) as CVarArg)
        var oldGroceries = [UUID:(ReadableGroceryItemWithId, GroceryItemData)]()
        for grocery in try ctx.fetch(groceryReq) {
            oldGroceries[grocery.id!] = (grocery.toReadableGroceryItemWithId(), grocery)
        }
        
        // insert new groceries from unmerged old groceries
        let ingredientReq2 = IngredientData.req()
        ingredientReq2.predicate = NSPredicate(format: "grocery_id in %@", Array(oldGroceryIds) as CVarArg)
        for ingredient in try ctx.fetch(ingredientReq2) {
            if let groceryId = ingredient.grocery_id, let (grocery, _) = oldGroceries[groceryId] {
                let newGrocery = ReadableGroceryItemWithId(item: ReadableGroceryItem(name: grocery.item.name, quantity: ReadableQuantity.fromInt(x: Int(ingredient.quantity)), unit: ingredient.unit, active: grocery.item.active, order: grocery.item.order), id: UUID())
                insertGroceryRaw(ctx, grocery: newGrocery)
                ingredient.grocery_id = newGrocery.id
            }
        }
        
        // delete old groceries
        for (_, grocery) in oldGroceries.values {
            ctx.delete(grocery)
        }
        
        // automerge
        try automergeGroceriesRaw(ctx)
    }
    
    func clearAll() {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            
            // delete ingredients not belonging to recipes, otherwise set the grocery to null
            for ingredient in try ctx.fetch(IngredientData.req()) {
                if ingredient.recipe_id == nil {
                    ctx.delete(ingredient)
                } else {
                    ingredient.grocery_id = nil
                }
            }
            
            // deactivate all recipes
            for recipe in try ctx.fetch(RecipeData.req()) {
                recipe.active = false
            }
            
            // delete all groceries
            try ctx.execute(NSBatchDeleteRequest(fetchRequest: NSFetchRequest(entityName: "GroceryItemData")))
            
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
    
    func overwrite(export: ExportResponse) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            
            // delete all data
            try ctx.execute(NSBatchDeleteRequest(fetchRequest: NSFetchRequest(entityName: "RecipeData")))
            try ctx.execute(NSBatchDeleteRequest(fetchRequest: NSFetchRequest(entityName: "GroceryItemData")))
            try ctx.execute(NSBatchDeleteRequest(fetchRequest: NSFetchRequest(entityName: "IngredientData")))
            
            // insert recipes
            var recipeIds = [Int:UUID]()
            for (recipeKey, recipe) in export.recipes {
                let recipeWithId = ReadableRecipeWithId(recipe: ReadableRecipe(name: recipe.name, link: recipe.link, active: recipe.active, rating: recipe.rating, notes: recipe.notes, ingredients: [:], steps: [:]), id: UUID())
                insertRecipeRaw(ctx, recipe: recipeWithId)
                recipeIds[recipeKey] = recipeWithId.id
            }
            
            // insert groceries
            var groceryIds = [Int:UUID]()
            for (groceryKey, grocery) in export.groceries {
                let groceryWithId = ReadableGroceryItemWithId(item: ReadableGroceryItem(name: grocery.name, quantity: grocery.quantity, unit: grocery.unit, active: grocery.active, order: grocery.order), id: UUID())
                insertGroceryRaw(ctx, grocery: groceryWithId)
                groceryIds[groceryKey] = groceryWithId.id
            }
            
            // insert ingredients
            export.ingredients.forEach({ (ingredientKey, ingredient) -> () in
                let ingredientWithId = ReadableIngredientWithId(id: UUID(), ingredient: ReadableIngredient(name: ingredient.name, quantity: ingredient.quantity, unit: ingredient.unit, order: ingredient.order))
                let recipeId = ingredient.recipeId.map({ recipeIds[$0]! })
                let groceryId = ingredient.groceryItemId.map({ groceryIds[$0]! })
                insertIngredientRaw(ctx, ingredient: ingredientWithId, recipeId: recipeId, groceryId: groceryId)
            })
            
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
}
