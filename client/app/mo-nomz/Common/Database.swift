//
//  Database.swift
//  mo-nomz
//
//  Created by Dan Fithian on 9/11/21.
//

import CoreData
import Foundation
import UIKit

class Database {
    private static func insertGroceryRaw(_ ctx: NSManagedObjectContext, grocery: ReadableGroceryItemWithId) {
        let newGrocery = NSEntityDescription.insertNewObject(forEntityName: "GroceryItemData", into: ctx) as! GroceryItemData
        newGrocery.id = grocery.id
        newGrocery.name = grocery.item.name
        newGrocery.quantity = Int32(grocery.item.quantity.toInt())
        newGrocery.unit = grocery.item.unit
        newGrocery.active = grocery.item.active
        newGrocery.ordering = Int32(grocery.item.order)
        newGrocery.group = grocery.item.group?.id
    }

    private static func insertIngredientRaw(_ ctx: NSManagedObjectContext, ingredient: ReadableIngredientWithId, recipeId: UUID?, groceryId: UUID?) {
        let newIngredient = NSEntityDescription.insertNewObject(forEntityName: "IngredientData", into: ctx) as! IngredientData
        newIngredient.id = ingredient.id
        newIngredient.grocery_id = groceryId
        newIngredient.recipe_id = recipeId
        newIngredient.name = ingredient.ingredient.name
        newIngredient.quantity = Int32(ingredient.ingredient.quantity.toInt())
        newIngredient.unit = ingredient.ingredient.unit
        newIngredient.ordering = Int32(ingredient.ingredient.order)
    }

    private static func insertRecipeRaw(_ ctx: NSManagedObjectContext, recipe: ReadableRecipeWithId) {
        let newRecipe = NSEntityDescription.insertNewObject(forEntityName: "RecipeData", into: ctx) as! RecipeData
        newRecipe.id = recipe.id
        newRecipe.name = recipe.recipe.name
        newRecipe.link = recipe.recipe.link
        newRecipe.active = recipe.recipe.active
        newRecipe.rating = Int32(recipe.recipe.rating)
        newRecipe.notes = recipe.recipe.notes
        newRecipe.tags = recipe.recipe.tags

        for (id, step) in recipe.recipe.steps {
            insertStepRaw(ctx, step: StepWithId(id: id, step: step), recipeId: recipe.id)
        }
    }

    private static func insertStepRaw(_ ctx: NSManagedObjectContext, step: StepWithId, recipeId: UUID) {
        let newStep = NSEntityDescription.insertNewObject(forEntityName: "StepData", into: ctx) as! StepData
        newStep.id = step.id
        newStep.recipe_id = recipeId
        newStep.step = step.step.step
        newStep.ordering = Int32(step.step.order)
    }

    private static func selectRecipeIngredientsRaw(_ ctx: NSManagedObjectContext, recipeId: UUID) throws -> [IngredientData] {
        let ingredientReq = IngredientData.req()
        ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", recipeId as CVarArg)
        ingredientReq.sortDescriptors = [
            NSSortDescriptor(key: "ordering", ascending: true),
            NSSortDescriptor(key: "name", ascending: true)
        ]
        return try ctx.fetch(ingredientReq)
    }

    private static func selectRecipeStepsRaw(_ ctx: NSManagedObjectContext, recipeId: UUID) throws -> [StepData] {
        let stepReq = StepData.req()
        stepReq.predicate = NSPredicate(format: "recipe_id = %@", recipeId as CVarArg)
        stepReq.sortDescriptors = [
            NSSortDescriptor(key: "ordering", ascending: true),
            NSSortDescriptor(key: "step", ascending: true)
        ]
        return try ctx.fetch(stepReq)
    }

    private static func insertGroceryGroupRaw(_ ctx: NSManagedObjectContext, group: GroceryGroupWithId) {
        let newGroup = NSEntityDescription.insertNewObject(forEntityName: "GroceryGroupData", into: ctx) as! GroceryGroupData
        newGroup.id = group.id
        newGroup.name = group.group.name
        newGroup.ordering = Int32(group.group.order)
    }

    static func selectGroceries() -> [ReadableGroceryItemWithId] {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = GroceryItemData.req()
            req.sortDescriptors = [
                NSSortDescriptor(key: "ordering", ascending: true),
                NSSortDescriptor(key: "name", ascending: true)
            ]

            return try ctx.fetch(req).map({ (grocery) in
                let groupData = try groceryGroupForRaw(ctx, grocery: grocery)
                return grocery.toReadableGroceryItemWithId(groupData: groupData)
            })
        } catch let error as NSError {
            print(error)
        }
        return []
    }

    static func selectMaxOrderRaw(_ ctx: NSManagedObjectContext) throws -> Int {
        let req = GroceryItemData.req()
        req.sortDescriptors = [NSSortDescriptor(key: "ordering", ascending: false)]
        req.fetchLimit = 1
        if let row = try ctx.fetch(req).first {
            return row.value(forKey: "ordering") as! Int + 1
        } else {
            return 1
        }
    }

    static func selectMaxStepOrderRaw(_ ctx: NSManagedObjectContext, recipeId: UUID) throws -> Int {
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

    static func selectMaxGroupOrderRaw(_ ctx: NSManagedObjectContext) throws -> Int {
        let req = GroceryGroupData.req()
        req.sortDescriptors = [NSSortDescriptor(key: "ordering", ascending: false)]
        req.fetchLimit = 1
        if let row = try ctx.fetch(req).first {
            return row.value(forKey: "ordering") as! Int + 1
        } else {
            return 1
        }
    }

    static func insertGroceries(ingredients: [ReadableIngredient]) {
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

    static func updateGrocery(grocery: ReadableGroceryItemWithId) {
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
                fetched.group = grocery.item.group?.id
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }

    static func updateGroup(group: GroceryGroupWithId) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = GroceryGroupData.req()
            req.predicate = NSPredicate(format: "id = %@", group.id as CVarArg)
            req.fetchLimit = 1
            if let fetched = try ctx.fetch(req).first {
                if fetched.ordering != Int32(group.group.order) {
                    let reorderReq = GroceryGroupData.req()
                    let notThisId = NSPredicate(format: "id <> %@", group.id as CVarArg)
                    let largerOrder = NSPredicate(format: "ordering >= %@", group.group.order as NSNumber)
                    reorderReq.predicate = NSCompoundPredicate(type: .and, subpredicates: [notThisId, largerOrder])
                    for other in try ctx.fetch(reorderReq) {
                        other.ordering += 1
                    }
                }
                fetched.name = group.group.name
                fetched.ordering = Int32(group.group.order)
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }

    static func mergeGroceries(ids: [UUID], grocery: ReadableGroceryItemWithId) {
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

    static func deleteGrocery(id: UUID) {
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

    static func selectRecipes() -> [ReadableRecipeWithId] {
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

    static func findRecipeByLink(host: String, path: String) -> ReadableRecipeWithId? {
        do {
            let ctx = DataAccess.shared.managedObjectContext

            let req = RecipeData.req()
            req.predicate = NSPredicate(format: "link like[c] %@", "*\(host)*\(path)*" as CVarArg)
            req.fetchLimit = 1
            if let recipeData = try ctx.fetch(req).first {
                let ingredients = try selectRecipeIngredientsRaw(ctx, recipeId: recipeData.id!)
                let steps = try selectRecipeStepsRaw(ctx, recipeId: recipeData.id!)
                return recipeData.toReadableRecipeWithId(ingredientsData: ingredients, stepsData: steps)
            }
        } catch let error as NSError {
            print(error)
        }
        return nil
    }

    static func getRecipe(id: UUID) -> ReadableRecipeWithId? {
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
    
    static func insertRecipe(response: ParseLinkResponse, link: String, active: Bool, tags: [String]) -> ReadableRecipeWithId {
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
        let recipe = ReadableRecipe(name: response.name, link: link, active: active, rating: 0, notes: "", ingredients: ingredients, steps: steps, tags: tags)
        return insertRecipe(recipe: recipe)
    }

    static func insertRecipe(response: ParseBlobResponse, name: String, link: String?, rawSteps: [String], active: Bool, tags: [String]) -> ReadableRecipeWithId {
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
        let recipe = ReadableRecipe(name: name, link: link, active: active, rating: 0, notes: "", ingredients: ingredients, steps: steps, tags: tags)
        return insertRecipe(recipe: recipe)
    }

    static func insertRecipe(recipe: ReadableRecipe) -> ReadableRecipeWithId {
        let recipeWithId = ReadableRecipeWithId(recipe: recipe, id: UUID())
        do {
            let ctx = DataAccess.shared.managedObjectContext

            // insert recipe
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
        return recipeWithId
    }

    private static func activateRecipeRaw(_ ctx: NSManagedObjectContext, id: UUID) throws {
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

    private static func deactivateRecipeRaw(_ ctx: NSManagedObjectContext, id: UUID) throws {
        // setting recipe active bit handled by caller
        let ingredientReq = IngredientData.req()
        ingredientReq.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
        var ingredientIds = Set<UUID>()
        for ingredient in try ctx.fetch(ingredientReq) {
            ingredientIds.insert(ingredient.id!)
        }
        try unmergeGroceriesRaw(ctx, ingredientIds: Array(ingredientIds))
    }

    static func updateRecipe(id: UUID, recipe: ReadableRecipe) {
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
                fetched.tags = recipe.tags
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }

    static func updateRecipeIngredients(id: UUID, active: Bool, deletes: [UUID], adds: [ReadableIngredientWithId]) {
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

    static func addRecipeSteps(recipeId: UUID, rawSteps: [String]) -> [StepWithId] {
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

    static func deleteRecipeStep(id: UUID) {
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

    static func updateRecipeStep(recipeId: UUID, step: StepWithId) {
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

    static func deleteRecipe(id: UUID) {
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

            // delete the steps
            let stepReq = StepData.req()
            stepReq.predicate = NSPredicate(format: "recipe_id = %@", id as CVarArg)
            for step in try ctx.fetch(stepReq) {
                ctx.delete(step)
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

    private static func groceryGroupForRaw(_ ctx: NSManagedObjectContext, grocery: GroceryItemData) throws -> GroceryGroupData? {
        if let groupId = grocery.group {
            let groupReq = GroceryGroupData.req()
            groupReq.predicate = NSPredicate(format: "id = %@", groupId as CVarArg)
            return try ctx.fetch(groupReq).first
        }
        return nil
    }

    private static func automergeGroceriesRaw(_ ctx: NSManagedObjectContext, active: Bool, groceries: [ReadableGroceryItemWithId]) throws {
        struct NameAndUnit: Hashable {
            let name: String
            let unit: String?
        }

        // aggregate groceries by name and unit
        var byNameAndUnit = [NameAndUnit:([UUID], ReadableQuantity, Int, GroceryGroupWithId?)]()
        for grocery in groceries {
            let nameAndUnit = NameAndUnit(name: grocery.item.name, unit: grocery.item.unit)
            let (ids, quantity, order, group) = byNameAndUnit[nameAndUnit] ?? ([], ReadableQuantity(whole: 0, fraction: nil), Int.max, nil)
            byNameAndUnit[nameAndUnit] = (ids + [grocery.id], quantity + grocery.item.quantity, min(grocery.item.order, order), group ?? grocery.item.group)
        }

        for (nameAndUnit, (ids, quantity, order, group)) in byNameAndUnit {
            if ids.count >= 2 {
                // insert new grocery for merge
                let newGrocery = ReadableGroceryItemWithId(item: ReadableGroceryItem(name: nameAndUnit.name, quantity: quantity, unit: nameAndUnit.unit, active: active, order: order, group: group), id: UUID())
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

    private static func automergeGroceriesRaw(_ ctx: NSManagedObjectContext) throws {
        // select the existing groceries
        var activeGroceries: [ReadableGroceryItemWithId] = []
        var inactiveGroceries: [ReadableGroceryItemWithId] = []
        for grocery in try ctx.fetch(GroceryItemData.req()) {
            let groupData = try groceryGroupForRaw(ctx, grocery: grocery)
            if grocery.active {
                activeGroceries.append(grocery.toReadableGroceryItemWithId(groupData: groupData))
            } else {
                inactiveGroceries.append(grocery.toReadableGroceryItemWithId(groupData: groupData))
            }
        }

        try automergeGroceriesRaw(ctx, active: true, groceries: activeGroceries)
        try automergeGroceriesRaw(ctx, active: false, groceries: inactiveGroceries)
    }

    private static func unmergeGroceriesRaw(_ ctx: NSManagedObjectContext, ingredientIds: [UUID]) throws {
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
            let groupData = try groceryGroupForRaw(ctx, grocery: grocery)
            oldGroceries[grocery.id!] = (grocery.toReadableGroceryItemWithId(groupData: groupData), grocery)
        }

        // insert new groceries from unmerged old groceries
        let ingredientReq2 = IngredientData.req()
        ingredientReq2.predicate = NSPredicate(format: "grocery_id in %@", Array(oldGroceryIds) as CVarArg)
        for ingredient in try ctx.fetch(ingredientReq2) {
            if let groceryId = ingredient.grocery_id, let (grocery, _) = oldGroceries[groceryId] {
                let newGrocery = ReadableGroceryItemWithId(item: ReadableGroceryItem(name: grocery.item.name, quantity: ReadableQuantity.fromInt(x: Int(ingredient.quantity)), unit: ingredient.unit, active: grocery.item.active, order: grocery.item.order, group: grocery.item.group), id: UUID())
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

    static func clearAll() {
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

    static func getAllTags() -> [String] {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let rawTags = try ctx.fetch(RecipeData.req()).compactMap({ $0.tags }).flatMap({ $0 })
            return Array(Array(Dictionary(rawTags.map({ ($0, 1) }), uniquingKeysWith: +)).sorted(by: { ($0.1, $0.0) < ($1.1, $1.0) }).reversed().map({ $0.0 }))
        } catch let error as NSError {
            print(error)
        }
        return []
    }

    static func selectGroups() -> [GroceryGroupWithId] {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = GroceryGroupData.req()
            return try ctx.fetch(req).map({ $0.toGroceryGroupWithId() }).sorted(by: { $0.group.order < $1.group.order })
        } catch let error as NSError {
            print(error)
        }
        return []
    }

    static func updateTag(old: String, new: String) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = RecipeData.req()
            for recipe in try ctx.fetch(req) {
                recipe.tags = recipe.tags?.map({ $0 == old ? new : $0 })
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }

    static func deleteTag(old: String) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = RecipeData.req()
            for recipe in try ctx.fetch(req) {
                recipe.tags = recipe.tags?.filter({ $0 != old })
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }

    static func selectMaxGroupOrder() -> Int {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            return try selectMaxGroupOrderRaw(ctx)
        } catch let error as NSError {
            print(error)
        }
        return 1
    }

    static func insertGroups(groups: [GroceryGroupWithId]) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            for group in groups {
                insertGroceryGroupRaw(ctx, group: group)
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }

    static func deleteGroup(id: UUID) {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let groceryReq = GroceryItemData.req()
            groceryReq.predicate = NSPredicate(format: "group = %@", id as CVarArg)
            for grocery in try ctx.fetch(groceryReq) {
                grocery.group = nil
            }
            let req = GroceryGroupData.req()
            req.predicate = NSPredicate(format: "id = %@", id as CVarArg)
            for group in try ctx.fetch(req) {
                ctx.delete(group)
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }

    static func uncategorizeAll() {
        do {
            let ctx = DataAccess.shared.managedObjectContext
            let req = GroceryItemData.req()
            for grocery in try ctx.fetch(req) {
                grocery.group = nil
            }
            try ctx.save()
        } catch let error as NSError {
            print(error)
        }
    }
}
